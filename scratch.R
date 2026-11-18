
# Scratch for BIOS 611
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

df <- read.csv("NCDOT_NonMotoristCrashes.csv")

# data cleaning - turn all to lowercase
df <- df %>%
  mutate(CrashTypGrp_Lower = str_to_lower(str_squish(CrashTypGrp)))

# add weekdays
df$Date_converted <- as.Date(df$CrashDate, format = "%m/%d/%Y")

df$Weekday <- weekdays(df$Date_converted)

# exploratory figures

# by year
ggplot(df, aes(x=factor(CrashYear))) + geom_bar()

# by month
month_order <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")
df$CrashMonth <- factor(df$CrashMonth, levels = month_order)
ggplot(df, aes(x=CrashMonth)) + geom_bar()

# by hour
ggplot(df, aes(x=factor(CrashHour))) + geom_bar()








################################################## VISUALIZATION #1
df_date_hour <- df %>% 
  select(CrashDate, CrashHour)

df_date_hour$Date_converted <- as.Date(df_date_hour$CrashDate, format = "%m/%d/%Y")

df_date_hour$Weekday <- weekdays(df_date_hour$Date_converted)

df_counted <- as.data.frame(table(Weekday = df_date_hour$Weekday, CrashHour = df_date_hour$CrashHour))

day_of_week_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", 
                       "Sunday")

# Plot
df_counted <- as.data.frame(table(Weekday = df_date_hour$Weekday, CrashHour = df_date_hour$CrashHour))

df_counted$Weekday <- factor(df_counted$Weekday, levels = day_of_week_order)

ggplot(df_counted, aes(x = factor(CrashHour), y = Freq, group = Weekday, color = Weekday)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Hour of Day", 
    y = "Number of Crashes",
    title = "Crashes by Time of Day and Weekday",
    color = "Weekday"
  ) +
  theme_minimal()

##################################################








# Alc related
ggplot(df, aes(x=factor(CrashAlcoh))) + geom_bar()

# Severity based on Alc
ggplot(df, aes(fill=CrashSevr, x=CrashAlcoh)) + 
  geom_bar(position="dodge", stat="count")

ggplot(df, aes(fill=CrashSevr, x=CrashAlcoh)) + 
  geom_bar(position="fill", stat="count") # by percentage

# Non-motorist age and race
df$NM_Age_numeric <- as.numeric(as.character(df$NM_Age))
ggplot(df, aes(x=NM_Age_numeric)) + geom_bar()

# Driver age and race
df$DrvrAge_Numeric <- as.numeric(as.character(df$DrvrAge))
ggplot(df, aes(x=DrvrAge_Numeric)) + geom_bar()

ggplot(df, aes(x=factor(DrvrRace))) + geom_bar()

# Joint histogram comparing ages between the non-motorist vs the driver
age_df <- df %>% 
  select(NM_Age_numeric,DrvrAge_Numeric) %>% 
  pivot_longer(
    cols = c("NM_Age_numeric", "DrvrAge_Numeric"),
    names_to = "person",
    values_to = "age"
  )

ggplot(age_df, aes(x = age, fill = person)) +
  geom_histogram(color = "#e9ecef",
                 alpha = 0.4,
                 position = 'identity') +
  labs(fill = "") +
  theme_minimal()











###################################################### VISUALIZATION #2

# code pedestrian vs driver crash types
## I am filtering the non motorists that are pedestrians and that are bicyclist
pedestrian_df <- df %>%
  filter(NM_Type == "Pedestrian")

bicyclist_df <- df %>%
  filter(NM_Type == "Bicyclist")

# general plot
ggplot(pedestrian_df, aes(x=factor(CrashTypGrp_Lower))) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(bicyclist_df, aes(x=factor(CrashTypGrp_Lower))) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### Plot for the differences between crash type percentage between rural and urban
# y axis is the percentage
# lines to connect the crash type between rural and urban 
# a plot of cyclists and one for bicyclist


# pedestrian
ped_rural_urban_counts <- pedestrian_df %>% 
  count(RuralUrban)

ped_rural_crash_prop <- pedestrian_df %>% 
  group_by(CrashTypGrp_Lower, RuralUrban) %>% 
  summarize(n_crashes=n()) %>% 
  ungroup() %>% 
  left_join(ped_rural_urban_counts, by="RuralUrban") %>% 
  mutate(prop = 100*n_crashes/n) 

# Top 15
ped_rural_crash_mean_prop <- ped_rural_crash_prop %>% 
  group_by(CrashTypGrp_Lower) %>% 
  summarize(avg_prop = mean(prop)) %>% 
  arrange(desc(avg_prop)) %>% 
  mutate(rank=seq(length(avg_prop))) %>% 
  filter(rank <=10)

ped_plot_data_top10 <- ped_rural_crash_prop %>% 
  filter(CrashTypGrp_Lower %in% ped_rural_crash_mean_prop$CrashTypGrp_Lower)

plot_data <- ped_plot_data_top10 %>%
  mutate(RuralUrban = factor(RuralUrban, levels = c("Rural", "Urban")))

ggplot(
  plot_data,
  aes(x = RuralUrban, y = prop, group = CrashTypGrp_Lower)
) +
  geom_line(aes(color = CrashTypGrp_Lower), size = 0.6) +
  geom_point(aes(color = CrashTypGrp_Lower), size = 2.5) +
  geom_text_repel(
    data = filter(plot_data, RuralUrban == "Rural"),
    aes(label = CrashTypGrp_Lower),
    nudge_x = -0.3,
    direction = "y",
    hjust = 1,
    box.padding = 0.4,
    point.padding = 0.15,
    segment.color = "grey65",
    segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    max.iter = 20000,
    size = 3,
    seed = 123
  ) +
  geom_text_repel(
    data = filter(plot_data, RuralUrban == "Urban"),
    aes(label = CrashTypGrp_Lower),
    nudge_x = 0.3,
    direction = "y",
    hjust = 0,
    box.padding = 0.4,
    point.padding = 0.15,
    segment.color = "grey65",
    segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    max.iter = 20000,
    size = 3,
    seed = 123
  ) +
  scale_x_discrete(expand = expansion(add = 1.5)) +
  labs(
    x = NULL,
    y = "Percentage of crashes (%)",
    title = "Top 10 Pedestrian crash types in rural and urban areas"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(color = "none")

# bicyclist

# filter out where Rural/Urban is NA
bicyclist_df <- bicyclist_df %>% 
  filter(RuralUrban %in% c("Rural", "Urban"))
bike_rural_urban_counts <- bicyclist_df %>% 
  count(RuralUrban)

bike_rural_crash_prop <- bicyclist_df %>% 
  group_by(CrashTypGrp_Lower, RuralUrban) %>% 
  summarize(n_crashes=n()) %>% 
  ungroup() %>% 
  left_join(bike_rural_urban_counts, by="RuralUrban") %>% 
  mutate(prop = 100*n_crashes/n)

# Top 15
bike_rural_crash_mean_prop <- bike_rural_crash_prop %>% 
  group_by(CrashTypGrp_Lower) %>% 
  summarize(avg_prop = mean(prop)) %>% 
  arrange(desc(avg_prop)) %>% 
  mutate(rank=seq(length(avg_prop))) %>% 
  filter(rank <=10)

bike_plot_data_top10 <- bike_rural_crash_prop %>% 
  filter(CrashTypGrp_Lower %in% bike_rural_crash_mean_prop$CrashTypGrp_Lower)

plot_data <- bike_plot_data_top10 %>%
  mutate(RuralUrban = factor(RuralUrban, levels = c("Rural", "Urban")))

ggplot(
  plot_data,
  aes(x = RuralUrban, y = prop, group = CrashTypGrp_Lower)
) +
  geom_line(aes(color = CrashTypGrp_Lower), size = 0.6) +
  geom_point(aes(color = CrashTypGrp_Lower), size = 2.5) +
  geom_text_repel(
    data = filter(plot_data, RuralUrban == "Rural"),
    aes(label = CrashTypGrp_Lower),
    nudge_x = -0.3,
    direction = "y",
    hjust = 1,
    box.padding = 0.4,
    point.padding = 0.15,
    segment.color = "grey65",
    segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    max.iter = 20000,
    size = 3,
    seed = 123
  ) +
  geom_text_repel(
    data = filter(plot_data, RuralUrban == "Urban"),
    aes(label = CrashTypGrp_Lower),
    nudge_x = 0.3,
    direction = "y",
    hjust = 0,
    box.padding = 0.4,
    point.padding = 0.15,
    segment.color = "grey65",
    segment.size = 0.3,
    min.segment.length = 0,
    max.overlaps = Inf,
    max.iter = 20000,
    size = 3,
    seed = 123
  ) +
  scale_x_discrete(expand = expansion(add = 1.5)) +
  labs(
    x = NULL,
    y = "Percentage of crashes (%)",
    title = "Top 10 Bicyclist crash types in rural and urban areas"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(color = "none")





################################### VISUALIZATION #3: Crash Severity (general)
# variables of interest: CrashMonth, CrashHour, CrashAlcoh, CrashLoc, Development, LightCond, Locality,
# RdConditio, RdDefects, Weather

# plot severity
ggplot(df, aes(x=factor(CrashSevr))) + geom_bar()
crash_severity_count <- df %>% 
  count(CrashSevr)

# split severity into two groups (Severe, Not_Severe), filter out unkwown injuries

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


# plot of severity proportions over the year
month_order <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")
injury_df$CrashMonth <- factor(injury_df$CrashMonth, levels = month_order)

ggplot(injury_df, aes(
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



# plot of severity propoertions over the time of day

ggplot(injury_df, aes(
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




# day of the week
day_of_week_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", 
                 "Sunday")

injury_df$Weekday <- factor(injury_df$Weekday, levels = day_of_week_order)

ggplot(injury_df, aes(
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
    x = "Month",
    y = "Proportion of Weekday Crashes",
    fill = "Crash Severity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Logistic Regression for crash severity
df_filt <- injury_df %>% 
  dplyr::select(Severity, LightCond,CrashHour,SpeedLimit,Weather,
                RuralUrban, CrashAlcoh)

library(dplyr)

df_model <- df_filt %>%
  mutate(severe = factor(
    Severity,
    levels = c("Not_Severe", "Severe")),
    light = case_when(
      LightCond == "Daylight" ~ "Daylight",
      LightCond == "Dark - Lighted Roadway" ~ "Dark, lighted",
      LightCond == "Dark - Roadway Not Lighted" ~ "Dark, unlighted",
      LightCond %in% c("Dawn", "Dusk") ~ "Dawn/dusk",
      LightCond == "Dark - Unknown Lighting" ~ "Dark, lighting unknown",
      TRUE ~ NA_character_
    ),
    
    time_period = case_when(
      CrashHour %in% 0:5   ~ "Overnight (00???05)",
      CrashHour %in% 6:11  ~ "Morning (06???11)",
      CrashHour %in% 12:17 ~ "Afternoon (12???17)",
      CrashHour %in% 18:23 ~ "Evening (18???23)",
      TRUE ~ NA_character_
    ),
    
    # Numeric entries and existing ranges get consistent groups.
    speed_numeric = suppressWarnings(as.numeric(SpeedLimit)),
    
    speed_group = case_when(
      SpeedLimit == "5 - 15 MPH"  ~ "5???15",
      SpeedLimit == "20 - 25 MPH" ~ "20???25",
      SpeedLimit == "30 - 35 MPH" ~ "30???35",
      SpeedLimit == "40 - 45 MPH" ~ "40???45",
      SpeedLimit == "50 - 55 MPH" ~ "50???55",
      SpeedLimit == "60 - 75 MPH" ~ "60???75",
      between(speed_numeric, 5, 15)  ~ "5???15",
      between(speed_numeric, 20, 25) ~ "20???25",
      between(speed_numeric, 30, 35) ~ "30???35",
      between(speed_numeric, 40, 45) ~ "40???45",
      between(speed_numeric, 50, 55) ~ "50???55",
      between(speed_numeric, 60, 75) ~ "60???75",
      TRUE ~ NA_character_
    ),
    
    weather_group = case_when(
      Weather %in% c("Clear", "Cloudy") ~ "Clear/cloudy",
      Weather == "Rain" ~ "Rain",
      Weather == "Fog, Smog, Smoke" ~ "Fog/smog/smoke",
      Weather %in% c(
        "Snow",
        "Sleet, Hail, Freezing Rain/Drizzle",
        "Snow, Sleet, Hail, Freezing Rain/Drizzle"
      ) ~ "Snow/winter precipitation",
      Weather %in% c("Other", "Blowing Sand, Dirt, Snow") ~ "Other",
      TRUE ~ NA_character_
    ),
    
    light = relevel(factor(light), ref = "Daylight"),
    time_period = relevel(
      factor(time_period), ref = "Afternoon (12???17)"
    ),
    speed_group = relevel(factor(speed_group), ref = "30???35"),
    weather_group = relevel(factor(weather_group), ref = "Clear/cloudy"),
    RuralUrban = relevel(factor(RuralUrban), ref = "Urban"),
    CrashAlcoh = relevel(factor(CrashAlcoh), ref = "No")
  )


table(df_model$severe, useNA = "ifany")

# Once you have created severe as 0/1:
predictors <- c(
  "light", "time_period", "speed_group",
  "weather_group", "RuralUrban", "CrashAlcoh"
)

# Once you have created severe as 0/1:
predictors <- c(
  "light", "time_period", "speed_group",
  "weather_group", "RuralUrban", "CrashAlcoh"
)

lapply(
  setNames(predictors, predictors),
  function(x) table(
    predictor = df_model[[x]],
    severe = df_model$severe,
    useNA = "ifany"
  )
)

model <- glm(
  severe ~ light + time_period + speed_group +
    weather_group + RuralUrban + CrashAlcoh,
  data = df_model,
  family = binomial
)

summary(model)


