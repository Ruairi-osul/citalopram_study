library(tidyverse)
library(ggthemes)
library(scales)

colors <- c(
  slow_regular="#FF0100",
  fast_firing="#00B34E",
  slow_irregular="#806A9D",
  "Non Responder"="#7E8283",
  Stimulated="#F6931D",
  Inhibited="#31A6D9"
)

theme_set(theme_gdocs() + 
            theme(axis.title.x = element_text(colour = "black", size=16),
                  axis.title.y = element_text(colour = "black", size=16),
                  strip.text = element_text(colour = "black", size=10.1, hjust=0.5, angle=0),
                  axis.text = element_text(color="black", size=13),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=13)))

df <- read_csv(file.path("data", "baseshock_responses.csv")) %>%
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
    cluster = factor(cluster, labels=c("FF", "SIR", "SR")))



df %>% 
  ggplot(aes(x=group, fill=response_baseshock)) +
  geom_bar(position=position_dodge()) +
  scale_fill_manual(values=colors) + 
  labs(x="", y="Neuron Count") +
  # facet_grid(rows=vars(cluster), scales="free", switch="both")+
  facet_wrap(vars(cluster), nrow=3, scales="free") +
  theme(axis.text.x = element_text(size=13, angle = 0, hjust=0.5),
        strip.text = element_text(angle=0, size=14),
        legend.position = "right")

##################################

# SR
df_sr <- filter(df, cluster=="SR")
chisq.test(table(df_sr$response_baseshock, df_sr$group))

# SIR
df_sir <- filter(df, cluster=="SIR")
fisher.test(table(df_sir$response_baseshock, df_sir$group))

# FF
df_ff <- filter(df, cluster=="FF")
fisher.test(table(df_ff$response_baseshock, df_ff$group))
