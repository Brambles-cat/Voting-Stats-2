library(httr)
library(dotenv)

dotenv::load_dot_env()
api_key <- Sys.getenv("APIKEY")

fetch_2024_plus_votes <- function() { # YT Shared Spreadsheets
  items <- c()
  next_page_token <- ""

  print("Fetching 2024+ sheet links from TT10PV playlist...")
  repeat {
    resp <- GET(
      "https://www.googleapis.com/youtube/v3/playlistItems",
      query = list(
        part = "contentDetails,snippet",
        playlistId = "PLfu8OwqMlN0SuFqAZaM5bPYvEGLKqwTez",
        maxResults = 50,
        pageToken = next_page_token,
        key = api_key
      )
    )
    resp <- content(resp)
    next_page_token <- resp$nextPageToken

    items <- c(items, resp$items)
    
    if (is.null(next_page_token)) break
  }
  
  links <- sapply(
    items,
    function(item) {
      return(c(
        period = item$contentDetails$videoPublishedAt,
        link = item$snippet$description
      ))
    }
  )

  links <- as.data.frame(t(links))

  # Subtracting a month for the real showcase period 2025-01 -> 2024-12
  links$month <- (as.numeric(substr(links$period, nchar("yyyy-m"), nchar("yyyy-mm"))) - 2) %% 12 + 1
  links$year <- as.numeric(substr(links$period, 1, nchar("yyyy"))) - ifelse(links$month == 12, 1, 0)

  links <- links[links$year >= 2024, c("year", "month", "link")]
  
  links$link <- regmatches(links$link, gregexpr("Spreadsheet: [^\n]+", links$link))
  links$link <- substr(links$link, nchar("Spreadsheet: ."), nchar(links$link))
  links$link <- gsub("/edit\\?.+", "/export?format=csv", links$link)
  
  print("Fetching spreadsheet data...")
  
  present <- list.files("vote_data", recursive = TRUE)
  months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

  present_months <- regmatches(present, gregexpr(paste(months, collapse = "|"), present))
  present_months <- sapply(present_months, function(month) {which(months == month)})
  present <- list(year = as.numeric(dirname(present)), month = present_months)

  links <- links[!(paste(links$year, links$month) %in% paste(present$year, present$month)),]
  present <- data.frame(present)[present$year >= 2024,] # Leave that bit to to the archive function
  
  present <- mapply(function(y, m) {
    group_file_names <- list.files(file.path("vote_data", y))
    return (read.csv(file.path("vote_data", y, group_file_names[grep(months[m], group_file_names)]), header = TRUE))
  }, SIMPLIFY = FALSE, present$year, present$month)
  
  present <- do.call(rbind, present)

  data <- apply(links, 1, function(row) {
    df <- read.csv(row["link"])
    df$Month <- as.integer(row["month"])
    df$Year <- as.integer(row["year"])
    df <- df[, c("Year", "Month", "Rank", "Title", "Link", "Votes", "Total.voters", "Notes")]
    
    if (!dir.exists(file.path("vote_data", row["year"])))
      dir.create(file.path("vote_data", row["year"]))
    
    write.csv(df, file.path("vote_data", row["year"], paste0(months[as.numeric(row["month"])], ".csv")), row.names = FALSE)
    return(df)
  })
  
  data <- do.call(rbind, data)
  names(data) <- c("Year", "Month", "Rank", "Title", "Link", "Votes", "Voters", "Notes")
  
  data$Year <- as.numeric(data$Year)
  data$Month <- as.numeric(data$Month)

  return(data)
}

# TT10PV Site Archive
fetch_archived_votes <- function() {
  
  aggregated <- data.frame(
    Year = numeric(),
    Month = numeric(),
    Rank = character(),
    Title = character(),
    Votes = numeric(),
    Voters = numeric(),
    Note = character()
  )
  
  months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
  # Most likely folders wouldn't be deleted so checking only for 2017 should be fine
  local <- file.info(file.path("vote_data", "2017"))$isdir
  local <- !is.na(local) && local

  if (!local) {
    temp_zip <- tempfile(fileext = ".zip")
    temp_dir <- tempdir()
    
    print("Downloading Past Voting Results...")
    download.file(
      "https://drive.usercontent.google.com/u/0/uc?id=1OwH7XR6wZUWmZgHsggbkiO5-zchXnyl3&export=download",
      destfile = temp_zip,
      mode = "wb"
    )
    
    unzipped_name <- unzip(temp_zip, list = TRUE)$Name[1]
    unzip(temp_zip, exdir = temp_dir)

    output_dump <- file.copy(
      list.files(
        file.path(temp_dir, unzipped_name),
        full.names = TRUE
      ),
      "vote_data", recursive = TRUE, copy.date = TRUE
    )
  }
  
  data_file_names <- list.files("vote_data", recursive = TRUE)
  data_file_names <- data_file_names[grep("^(201\\d|202[0-3])", data_file_names)]

  for (file_name in data_file_names) {
    data <- read.csv(file.path("vote_data", file_name), header = FALSE)

    year <- as.integer(regmatches(file_name, gregexpr("\\d{4}", file_name))[[1]][1])
    month <- which(months == regmatches(file_name, gregexpr("January|February|March|April|May|June|July|August|September|October|November|December", file_name))[[1]][1])
    
    if (year <= 2018) {
      data <- data[, c(1, 2, ncol(data))]
      names(data) <- c("Title", "Votes", "Notes")
      data$Voters <- NA
    } else {
      data <- data[, c(1, 2, 4, ncol(data))]
      names(data) <- c("Title", "Votes", "Voters", "Notes")
      data$Voters <- data$Voters[1]
    }
    
    data$Rank <- NA
    data$Link <- NA
    
    data$Year <- year
    data$Month <- month
    
    data <- data[, c("Year", "Month", "Rank", "Title", "Link", "Votes", "Voters", "Notes")]
    
    aggregated <- rbind(aggregated, data)
  }

  aggregated <- aggregated[!is.na(as.numeric(aggregated$Votes)),] # nope, it's only two data points and I do not feel like handling it today
  aggregated$Notes[!is.na(as.numeric(aggregated$Note))] <- ""
  aggregated$Votes <- as.integer(aggregated$Votes)
  aggregated$Voters[aggregated$Year == 2019 & aggregated$Month == 7] <- 68 # Consarn it Littleshy T-T
  
  aggregated <- aggregated[order(aggregated$Year, aggregated$Month, decreasing = TRUE),]
  
  return(aggregated)
}

fetch_master_archive <- function() {}

fetch_hm_arhive <- function() {}

fetch_data <- function() {
  if (!file.exists("aggregated.csv")) {
    data_sub_2024 <- fetch_archived_votes()
    data_2024_plus <- fetch_2024_plus_votes()
    
    aggregated <- rbind(data_2024_plus, data_sub_2024)
    
    write.csv(aggregated, "aggregated.csv", row.names = FALSE)
  } else {
    aggregated <- read.csv("aggregated.csv")
  }
  
  return(aggregated)
}

output_dump <- fetch_data()
