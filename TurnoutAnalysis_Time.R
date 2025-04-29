library(ggplot2)
source("Data.R")

df <- unique(df[,c("Year", "Month", "Voters")])
df$Type <- "Regular"
df$Type[df$Year <= 2018] <- "Underestimate"
df$Period <- df$Year - 2017 + (df$Month - 1) / 12

breaks <- 2017:max(df$Year) - 2017


p1 <- ggplot(data = df, aes(x = Period, y = Voters, colour = Type)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, max(df$Voters) + 0.01)) +
  scale_x_continuous(breaks = breaks, labels = 2017:max(df$Year)) +
  labs(title = "Voter Turnouts Over Time", x = "Voting Period")

set.seed(1)
n <- 1000
bootstrap_df <- df[df$Year > 2018,]
intercepts <- numeric(n)
slopes <- numeric(n)

for (i in 1:n) {
  samp <- sample(1:nrow(bootstrap_df), nrow(bootstrap_df), replace = TRUE)
  samp <- bootstrap_df[samp, ]
  samp$Period <- samp$Period - 2 # Since 2017 and 2018 aren't considered useable
  coefs <- coef(lm(Voters ~ Period, samp))
  
  intercepts[i] <- coefs[1]
  slopes[i] <- coefs[2]
}

intercepts <- quantile(intercepts, probs = c(0.025, 0.975))
slopes <- quantile(slopes, probs = c(0.025, 0.975))

df2 <- df[df$Year > 2018,]
df2$Period <- df2$Period - 2
coefs_base <- coef(lm(Voters ~ Period, df2))

coefs_upper <- c(intercepts[2], slopes[2])
coefs_lower <- c(intercepts[1], slopes[1])

print(paste("Upper regression line coefs:", paste(coefs_upper[1], coefs_upper[2])))
print(paste("Base regression line coefs:", paste(coefs_base[1], coefs_base[2])))
print(paste("Lower regression line coefs:", paste(coefs_lower[1], coefs_lower[2])))
correlation_coef <- cor(df2$Voters, df2$Period)
print(paste("Correlation Coefficient:", correlation_coef))
print(paste("Coefficient of Determination:", correlation_coef ^ 2))

p1a1 <- ggplot(data = df, aes(x = Period, y = Voters, colour = Type)) +
  geom_point() +
  geom_line() +
  
  # Intercept adjusted by two years since only 2019+ data were used for slope/intercept calculations
  geom_abline(intercept = coefs_upper[1] + coefs_upper[2] * -2, slope = coefs_upper[2], linetype = "dashed", alpha = 0.5) +
  geom_abline(intercept = coefs_base[1] + coefs_base[2] * -2, slope = coefs_base[2]) +
  geom_abline(intercept = coefs_lower[1] + coefs_lower[2] * -2, slope = coefs_lower[2], linetype = "dashed", alpha = 0.5) +
  
  scale_y_continuous(limits = c(0, max(df$Voters) + 0.01)) +
  scale_x_continuous(breaks = breaks, labels = 2017:max(df$Year)) +
  labs(title = "Voter Turnouts Over Time", x = "Voting Period")

p2 <- ggplot(data = df, aes(x = factor(Year), y = Voters, fill = factor(Year))) +
  geom_boxplot() +
  geom_jitter(width = 0.15, alpha = 0.2) +
  scale_y_continuous(limits = c(0, max(df$Voters) + 0.01)) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "orange") +
  scale_fill_manual(values = c("2017" = "lightblue", "2018" = "lightblue"), guide = "none", na.value = "lightgrey") +
  labs(title = "Annual Voter Turnout Distributions", x = "Year")



monthly <- data.frame(matrix(nrow = 0, ncol = 13))
names(monthly) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
include_cur_yr <- table(df$Year)[as.character(max(df$Year))] == 12

for (y in unique(df$Year[include_cur_yr | df$Year < max(df$Year)])) {
  epoch <- df[df$Year == y,]
  epoch_voters <- sum(epoch$Voters)
  epoch$Proportion <- epoch$Voters / epoch_voters
  
  row <- data.frame(t(c(y, epoch$Proportion)))
  names(row) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  monthly <- rbind(monthly, row)
}

monthly <- monthly[monthly$Year > 2018,]
monthly <- reshape(monthly, timevar = "Month", idvar = "Year", direction = "long", varying = substr(months,1, 3), v.names = "Proportion")
monthly$Month <- substr(months[monthly$Month],1, 3)
monthly$Month <- factor(monthly$Month, levels = substr(months,1, 3))

p3 <- ggplot(monthly, aes(x = Month, y = Proportion, group = factor(Year))) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Year) +
  geom_hline(yintercept = 1/12, linetype = "dashed", color = "orange") +
  scale_y_continuous(limits = c(0, max(monthly$Proportion) + 0.01)) +
  labs(title = "Monthly Turnout Proportions of Each Year", y = "Voter Turnout %")

df2 <- monthly[monthly$Year > 2018,]
print(t.test(df2[df2$Month == "Jan",]$Proportion, mu = 1/12, alternative = "less"))
print(t.test(df2[df2$Month == "Oct",]$Proportion, mu = 1/12, alternative = "greater"))

p4 <- ggplot(df2, aes(x = Month, y = Proportion)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.2) +
  geom_hline(yintercept = 1/12, linetype = "dashed", color = "orange") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "orange") +
  scale_y_continuous(limits = c(0, max(df2$Proportion) + 0.01)) +
  labs(title = paste0("Distributions of Monthly Turnout % (2019-", max(df2$Year), ")"), y = "Turnout % of Each Year")

ggsave("Voters Over Time.png", p1, width = 10, path = "plots")
ggsave("VOT With Regression Lines.png", p1a1, width = 10, path = "plots")

ggsave("Annual Voter Dist.png", p2, width = 7, path = "plots")

ggsave("Monthly Turnout Proportions of Each Year.png", p3, width = 9, path = "plots")

ggsave("Monthly Turnout Percent Distrubtions.png", p4, width = 7, path = "plots")
