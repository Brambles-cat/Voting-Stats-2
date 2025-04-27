df <- read.csv("aggregated.csv")

missing_voters <- df[df$Year <= 2018,]

df <- df[df$Year > 2018,]
df <- unique(df[, c("Year", "Month", "Voters")])

missing_voters <- as.data.frame(as.table(tapply(
  missing_voters$Votes, list(
    missing_voters$Year, missing_voters$Month
  ), max
)))

names(missing_voters) <- c("Year", "Month", "Voters")
missing_voters$Year <- as.integer(as.character(missing_voters$Year))
missing_voters$Month <- as.integer(as.character(missing_voters$Month))

df <- rbind(missing_voters, df)
df <- df[order(df$Year, df$Month),]
