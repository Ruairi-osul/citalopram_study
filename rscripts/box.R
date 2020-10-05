library(tidyverse)
library(ggthemes)
library(scales)

colors <- c(
  "Slow Regular"="#FF0100",
  "Fast Firing"="#00B34E",
  "Slow Irregular"="#806A9D",
  no_baseline="#7E8283",
  baseline="#F6931D"
)

theme_set(theme_gdocs() + 
            theme(axis.title.x = element_text(colour = "black", size=16),
                  axis.title.y = element_text(colour = "black", size=16),
                  strip.text = element_text(colour = "black", size=10.1, hjust=0.5),
                  axis.text = element_text(color="black", size=13),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=18)))

df <- read_csv(file.path("data", "baseline.csv")) %>%
  filter(group_name %in% c("chronic_citalopram", 
                           "chronic_saline", 
                           "chronic_saline_", 
                           "citalopram_continuation", 
                           "citalopram_discontinuation"),
         cluster != "no_baseline") %>%
  mutate(
    group = if_else(group_name %in% c("chronic_saline", "chronic_saline_"),
                    "CS",
                    if_else(group_name %in% c("citalopram_continuation", "chronic_citalopram"),
                            "CC", "CD")),
    group = factor(group, levels=c("CS", "CC", "CD")),
    cluster = factor(cluster, labels=c("Fast Firing", "Slow Irregular", "Slow Regular")),
         waveform_width=waveform_width / 30)


df_melt <- df %>%
  mutate(lmfr = log(mean_firing_rate)) %>%
  select(neuron_id, lmfr, cv2_isi, waveform_width, peak_asymmetry, cluster, channel, group) %>%
  rename("Log Mean Firing Rate\n[Hz]"=lmfr,
         "Spike Regularity\n[CV(ISI)]"=cv2_isi,
         "Channel Number"=channel,
         "Waveform Asymmetry\n[AU]"= peak_asymmetry,
         "Waveform Width\n[ms]"= waveform_width) %>%
  gather(key="metric", value="value", -neuron_id, -cluster, -group)

df_melt %>%
  ggplot(aes(x=cluster, y=value, color=group)) +
  geom_boxplot(width=0.6, lwd=0.8) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  labs(x="", y="") +
  coord_flip() +
  facet_grid(cols=vars(metric), scales="free") +
  theme(legend.position = "right")

