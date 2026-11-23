# Crash Severity on Hour, day of the week, month
library(dplyr)
library(ggplot2)
df <- read.csv("clean_data.csv")

# Split severity into two groups (Severe, Not_Severe), filter out unkwown injuries

severe_injury_df <- df %>% 
  filter(CrashSevr %in% c("A: Suspected Serious Injury",
                          "K: Killed")) %>% 
  mutate(Severity = "Severe")

not_severe_injury_df <- df %>% 
  filter(CrashSevr %in% c("B: Suspected Minor Injury",
                          "C: Possible Injury",
                          "O: No Injury")) %>% 
  mutate(Severity = "Not_Severe")

injury_df <- rbind(severe_injury_df,not_severe_injury_df)
write.csv(injury_df, "injury_df.csv")

# plot of severity proportions over the year
month_order <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")
injury_df$CrashMonth <- factor(injury_df$CrashMonth, levels = month_order)

p<- ggplot(injury_df, aes(
  x = CrashMonth,
  fill = Severity
)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c(
    "Severe" = "#C44E52",
    "Not_Severe" = "#4C72B0"
  )) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "Proportion of Severe and Non-Severe Crashes by Month",
    x = "Month",
    y = "Proportion of Monthly Crashes",
    fill = "Crash Severity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Crash_Severity_Prop_by_Month.png", p)


# plot of severity propoertions over the time of day

p<- ggplot(injury_df, aes(
  x = factor(
    CrashHour,
    levels = 0:23,
    labels = c(
      "12 am", paste0(1:11, " am"),
      "12 pm", paste0(1:11, " pm")
    )
  ),
  fill = Severity
)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c(
    "Severe" = "#C44E52",
    "Not_Severe" = "#4C72B0"
  )) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    x = "Hour of Day",
    y = "Proportion of Hourly Crashes",
    fill = "Crash Severity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Crash_Severity_Prop_by_Time_of_Day.png", p)


# day of the week
day_of_week_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", 
                       "Sunday")

injury_df$Weekday <- factor(injury_df$Weekday, levels = day_of_week_order)

p<- ggplot(injury_df, aes(
  x = Weekday,
  fill = Severity
)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c(
    "Severe" = "#C44E52",
    "Not_Severe" = "#4C72B0"
  )) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "Proportion of Severe and Non-Severe Crashes by Day of the Week",
    x = "Day of the Week",
    y = "Proportion of Weekday Crashes",
    fill = "Crash Severity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Crash_Severity_Prop_by_Day_of_Week.png", p)
