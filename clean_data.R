# Data Cleaning
library(dplyr)
library(stringr)

df <- read.csv("NCDOT_NonMotoristCrashes.csv")

# data cleaning - turn all to lowercase
df <- df %>%
  mutate(CrashTypGrp_Lower = str_to_lower(str_squish(CrashTypGrp)))

# add weekdays
df$Date_converted <- as.Date(df$CrashDate, format = "%m/%d/%Y")

df$Weekday <- weekdays(df$Date_converted)

write.csv(df, "clean_data.csv")