library(tidyverse)
library(ggthemes)

colors <- c(
  slow_regular="#FF0100",
  fast_firing="#00B34E",
  slow_irregular="#806A9D",
  no_baseline="#7E8283",
  baseline="#F6931D"
)

theme_set(theme_gdocs() + 
            theme(axis.title.x = element_text(colour = "black", size=16),
                  axis.title.y = element_text(colour = "black", size=16),
                  axis.text = element_text(color="black", size=14),
                  strip.text = element_text(colour = "black", size=15, hjust=0.5),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=18)))

df <- read_csv(file.path("data", "baseline.csv")) %>%
  filter(group_name %in% c("chronic_citalopram", 
                           "chronic_saline", 
                           "chronic_saline_", 
                           "citalopram_continuation", 
                           "citalopram_discontinuation"),
         cluster != "no_baseline") %>%
  mutate(cluster = factor(cluster),
    group = if_else(group_name %in% c("chronic_saline", "chronic_saline_"),
                    "Chronic Saline",
                    if_else(group_name %in% c("citalopram_continuation", "chronic_citalopram"),
                            "Chronic Citalopram", "Citalopram Discontinuation")),
    group = factor(group, levels=c("Chronic Saline", "Chronic Citalopram", "Citalopram Discontinuation")))

df %>%
  ggplot(aes(x=cv2_isi, y=mean_firing_rate, color=cluster)) +
  geom_point(alpha=0.4) +
  scale_color_manual(values=colors, 
                     labels=c("Fast Firing", "Slow Irregular", "Slow Regular")) +
  labs(y="Mean Firing  [Hz]", x="Spike Regularity [CV(ISI)]") +
  theme(legend.position = c(0.7, 0.8), legend.background= element_rect(fill="NA"))

df %>%
  ggplot(aes(x=cv2_isi, y=mean_firing_rate, color=cluster)) +
  geom_point(alpha=0.4) +
  scale_color_manual(values=colors, 
                     labels=c("Fast Firing", "Slow Irregular", "Slow Regular")) +
  labs(y="Mean Firing  [Hz]", x="Spike Regularity [CV(ISI)]") +
  facet_grid(cols=vars(group), scales="fixed") +
  theme(legend.position = "None")


df %>%
  filter(group == "Citalopram Discontinuation") %>%
  select(mean_firing_rate, cluser)
  ggplot(aes(x=cv2_isi, y=mean_firing_rate, color=cluster)) +
  geom_point()
