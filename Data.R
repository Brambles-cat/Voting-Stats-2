df <- read.csv("aggregated.csv")

missing_voters <- df[df$Year <= 2018,]

df <- unique(df[, c("Year", "Month", "Rank", "Title", "Votes", "Voters")])

missing_voters <- as.data.frame(as.table(tapply(
  missing_voters$Votes, list(
    missing_voters$Year, missing_voters$Month
  ), max
)))

names(missing_voters) <- c("Year", "Month", "Voters")

for (y in 2017:2018) {
  for (m in unique(missing_voters$Month)) {
    df[df$Year == y & df$Month == m,]$Voters <- missing_voters$Voters[missing_voters$Year == y & missing_voters$Month == m]
  }
}

df <- df[order(df$Year, df$Month),]
