# Number of Crashes by Time of Day and Weekday
library(dplyr)
library(ggplot2)

df <- read.csv("clean_data.csv")

df_counted <- as.data.frame(table(Weekday = df$Weekday, CrashHour = df$CrashHour))

day_of_week_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", 
                       "Sunday")

# Plot
df_counted$Weekday <- factor(df_counted$Weekday, levels = day_of_week_order)

p <- ggplot(df_counted, aes(x = factor(CrashHour), y = Freq, group = Weekday, color = Weekday)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Hour of Day", 
    y = "Number of Crashes",
    title = "Crashes by Time of Day and Weekday",
    color = "Weekday"
  ) +
  theme_minimal()

ggsave("Crashes_by_Time_and_Day.png", p, width = 10, height = 6, units = "in", dpi = 300)
