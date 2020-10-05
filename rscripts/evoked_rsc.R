library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(ggthemes)
library(EnvStats)

theme_set(theme_ipsum()+ 
            theme(axis.title.x = element_text(colour = "black", size=16),
                  axis.text.x=element_text(size=13),
                  plot.title=element_text(size=12),
                  axis.title.y = element_text(colour = "black", size=16),
                  strip.text = element_text(colour = "black", size=10.1, hjust=0.5),
                  axis.text = element_text(color="black", size=13),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=18),
                  legend.position = "none"))


df <- read_csv(file.path("data", "evoked_rsc.csv")) %>%
  mutate(group=factor(group, levels=c("CS", "CC", "CD")),
  comb = factor(comb, levels=c("sr_sr", "sr_sir", "sr_ff", "sir_sir", "sir_ff", "ff_ff"),
                       labels=c("SR:SR", "SR:SIR", "SR:FF", "SIR:SIR", "SIR:FF", "FF:FF"))
  )

df %>%
  filter(group %in% c("CS", "CC"),
         has_ff == T) %>%
  group_by(comb, group) %>%
  summarise("% of Pairs Correlated"=mean(p < 0.05) * 100) %>%
  ungroup() %>%
  ggplot(aes(x=`% of Pairs Correlated`, y=comb, color=group, group=group)) + 
  geom_point(size=4, position =position_dodge(width=0.5)) +
  labs(y="") +
  xlim(0, 20) +
  ggtitle("Evoked Correlations") +
  theme(legend.position="right")

df %>%
  filter(group %in% c("CS", "CC"),
         has_sr == T) %>%
  group_by(comb, group) %>%
  summarise("% of Pairs Correlated"=mean(p < 0.05) * 100) %>%
  ungroup() %>%
  ggplot(aes(x=`% of Pairs Correlated`, y=comb, color=group, group=group)) + 
  geom_point(size=4, position =position_dodge(width=0.5)) +
  labs(y="") +
  xlim(0, 20) +
  ggtitle("Evoked Correlations") +
  theme(legend.position="right")

