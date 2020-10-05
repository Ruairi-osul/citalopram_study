library(tidyverse)
library(scales)


df <- read_csv(file.path("data", "cit_zscores.csv")) %>%
  filter(cluster %in% c("fast_firing", 
                        "slow_irregular",
                        "slow_regular")) %>%
  mutate(cluster=factor(cluster, 
                        levels=c("fast_firing", 
                                 "slow_irregular",
                                 "slow_regular"),
                        labels=c("FF", "SIR", "SR")),
    group = if_else(group_name %in% c("chronic_saline", "chronic_saline_"),
                    "CS",
                    if_else(group_name %in% c("citalopram_continuation", "chronic_citalopram"),
                            "CC", "CD")),
    group = factor(group, levels=c("CS", "CC", "CD")),
    time=bin/60)


colors <- c(
  SAL="#7E8283",
  CIT="#31A6D9",
  slow_regular="#FF0100",
  fast_firing="#00B34E",
  slow_irregular="#806A9D"
)

theme_set(theme_minimal() + 
            theme(axis.title.x = element_text(colour = "black", size=25),
                  axis.title.y = element_text(colour = "black", size=25),
                  strip.text = element_text(color="black", size=20),
                  axis.text.y = element_text(color="black", size=15),
                  axis.text.x = element_text(color="black", size=15),
                  legend.title =element_text(color="black", size=18),
                  legend.text = element_text(color="black", size=15)))


df %>%
  filter(cluster=="SR") %>%
  ggplot(aes(x=time, y=zscore, color=group, fill=group)) +
  geom_smooth(method="loess", alpha=0.3, size=1.3, span=0.8) +
  geom_vline(xintercept=0, linetype="dotted", size=1.1) +
  # scale_color_manual(values=c(Saline="#7E8283", Citalopram="#00B34E")) +
  # scale_fill_manual(values=c(Saline="#7E8283", Citalopram="#00B34E"), guide="none") +
  coord_cartesian(ylim=c(-4.5,3)) +
  scale_x_continuous(breaks=c(-10, 0, 10, 20)) +
  scale_y_continuous(breaks=c(-3,  0, 3)) +
  labs(x="Time", y="Z Score", color="Fast Firing", fill="")

df %>%
  filter(cluster=="SIR") %>%
  ggplot(aes(x=time, y=zscore, color=group, fill=group)) +
  geom_smooth(method="loess", alpha=0.3, size=1.3, span=0.8) +
  geom_vline(xintercept=0, linetype="dotted", size=1.1) +
  # scale_color_manual(values=c(Saline="#7E8283", Citalopram="#00B34E")) +
  # scale_fill_manual(values=c(Saline="#7E8283", Citalopram="#00B34E"), guide="none") +
  coord_cartesian(ylim=c(-4.5,3)) +
  scale_x_continuous(breaks=c(-10, 0, 10, 20)) +
  scale_y_continuous(breaks=c(-3,  0, 3)) +
  labs(x="Time", y="Z Score", color="Fast Firing", fill="")

df$time
