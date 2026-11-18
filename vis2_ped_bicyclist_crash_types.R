# Pedestrian vs Bicylist Crash Types
library(dplyr)
library(ggplot2)
library(ggrepel)

# Filter pedestrian vs bicyclist
df <- read.csv("clean_data.csv")

pedestrian_df <- df %>%
  dplyr::filter(NM_Type == "Pedestrian")

bicyclist_df <- df %>%
  dplyr::filter(NM_Type == "Bicyclist")


#### Plot for the differences between crash type percentage between rural and urban
# y axis is the percentage
# lines to connect the crash type between rural and urban 
# a plot of cyclists and one for bicyclist


# pedestrian
ped_rural_urban_counts <- pedestrian_df %>% 
  dplyr::count(RuralUrban)

ped_rural_crash_prop <- pedestrian_df %>% 
  dplyr::group_by(CrashTypGrp_Lower, RuralUrban) %>% 
  dplyr::summarize(n_crashes=dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(ped_rural_urban_counts, by="RuralUrban") %>% 
  dplyr::mutate(prop = 100*n_crashes/n) 

# Top 10
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

p<-ggplot(
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

ggsave("Pedestrian_Ranked_Crash_Type.png", p, width = 10, height = 6, units = "in", dpi = 300)

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

# Top 10
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

p<- ggplot(
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

ggsave("Bicyclist_Ranked_Crash_Type.png", p, width = 10, height = 6, units = "in", dpi = 300)

