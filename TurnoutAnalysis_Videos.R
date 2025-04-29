library(ggplot2)
source("Data.R")

df$Period <- df$Year - 2017 + (df$Month - 1) / 12
df$Type <- ifelse(df$Year >= 2024, "Capped", "Regular")

discovered_videos <- tapply(df$Title, factor(df$Period), function(x) { return(length(unique(x))) })
discovered_videos <- as.data.frame(discovered_videos)
names(discovered_videos) <- "Discovered"
discovered_videos$Period <- as.numeric(rownames(discovered_videos))
rownames(discovered_videos) <- NULL

df <- merge(df, discovered_videos, by = "Period")

unique(df[,c("Discovered", "Month", "Year")])
ggplot(df, aes(x = Period, y = Discovered)) +
  geom_line() +
  geom_point()
