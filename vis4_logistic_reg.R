# Logistic Regression for Crash Severity 
library(dplyr)
library(ggplot2)
library(broom)
injury_df <- read.csv("injury_df.csv")

# Columns of interest
df_filt <- injury_df %>% 
  dplyr::select(Severity, LightCond,CrashHour,SpeedLimit,Weather,
                RuralUrban, CrashAlcoh)

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
      CrashHour %in% 0:5   ~ "Overnight (12am-5am)",
      CrashHour %in% 6:11  ~ "Morning (6am-11am)",
      CrashHour %in% 12:17 ~ "Afternoon (12pm-5pm)",
      CrashHour %in% 18:23 ~ "Evening (6pm-11pm)",
      TRUE ~ NA_character_
    ),
    
    # Numeric entries and existing ranges get consistent groups.
    speed_numeric = suppressWarnings(as.numeric(SpeedLimit)),
    
    speed_group = case_when(
      SpeedLimit == "5 - 15 MPH"  ~ "5-15",
      SpeedLimit == "20 - 25 MPH" ~ "20-25",
      SpeedLimit == "30 - 35 MPH" ~ "30-35",
      SpeedLimit == "40 - 45 MPH" ~ "40-45",
      SpeedLimit == "50 - 55 MPH" ~ "50-55",
      SpeedLimit == "60 - 75 MPH" ~ "60-75",
      between(speed_numeric, 5, 15)  ~ "5-15",
      between(speed_numeric, 20, 25) ~ "20-25",
      between(speed_numeric, 30, 35) ~ "30-35",
      between(speed_numeric, 40, 45) ~ "40-45",
      between(speed_numeric, 50, 55) ~ "50-55",
      between(speed_numeric, 60, 75) ~ "60-75",
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
      factor(time_period), ref = "Afternoon (12pm-5pm)"
    ),
    speed_group = relevel(factor(speed_group), ref = "30-35"),
    weather_group = relevel(factor(weather_group), ref = "Clear/cloudy"),
    RuralUrban = relevel(factor(RuralUrban), ref = "Urban"),
    CrashAlcoh = relevel(factor(CrashAlcoh), ref = "No")
  )

predictors <- c(
  "light", "time_period", "speed_group",
  "weather_group", "RuralUrban", "CrashAlcoh"
)

model <- glm(
  severe ~ light + time_period + speed_group +
    weather_group + RuralUrban + CrashAlcoh,
  data = df_model,
  family = binomial
)

summary(model)

# Calculate adjusted odds ratio with 95% CI and create col for significance
or_results <- broom::tidy(
  model,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(significance = ifelse(
    p.value < 0.05,
    "p < 0.05",
    "p >= 0.05"
  ))

pretty_labels <- c(
  "speed_group60-75" = "Speed limit: 60-75 mph",
  "speed_group50-55" = "Speed limit: 50-55 mph",
  "speed_group40-45" = "Speed limit: 40-45 mph",
  "speed_group20-25" = "Speed limit: 20-25 mph",
  "speed_group5-15"  = "Speed limit: 5-15 mph",
  "lightDark, unlighted" = "Dark: unlighted roadway",
  "lightDark, lighted" = "Dark: lighted roadway",
  "lightDark, lighting unknown" = "Dark: lighting unknown",
  "lightDawn/dusk" = "Dawn or dusk",
  "CrashAlcohYes" = "Alcohol involvement",
  "RuralUrbanRural" = "Rural location",
  "time_periodOvernight (12am-5am)" = "Overnight (12am-5am)",
  "time_periodMorning (6am-11am)" = "Morning (6am-11am)",
  "time_periodEvening (6pm-11pm)" = "Evening (6pm-11pm)",
  "weather_groupFog/smog/smoke" = "Weather: fog, smog, or smoke",
  "weather_groupRain" = "Weather: rain",
  "weather_groupOther" = "Weather: other",
  "weather_groupSnow/winter precipitation" = "Weather: winter precipitation"
)

p <- ggplot(or_results, aes(
  x = estimate,
  y = reorder(term, estimate),
  color = significance
)) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "gray50"
  ) +
  geom_segment(aes(
    x = conf.low,
    xend = conf.high,
    yend = reorder(term, estimate)
  )) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "p < 0.05" = "red3",
    "p >= 0.05" = "gray40"
  )) +
  scale_x_log10() +
  scale_y_discrete(labels = function(x) {
    labels <- unname(pretty_labels[x])
    labels[is.na(labels)] <- x[is.na(labels)]
    labels
  }) +
  labs(
    title = "Factors associated with severe crash injury",
    subtitle = paste(
      "References: speed 30-35 mph; daylight; afternoon;\n",
      "clear/cloudy; urban; no alcohol",
      sep = ""
    ),
    x = "Adjusted odds ratio (95% confidence interval)",
    y = NULL,
    color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    plot.subtitle = element_text(size = 9),
    legend.position = "bottom"
  )

ggsave("Logit_Reg_for_Crash_Severity.png", p)
