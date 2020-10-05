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

df200 <- read_csv(file.path("data", "SpontRsc.csv")) %>%
  filter(binsize==0.2) %>%
  mutate(comb = factor(comb, levels=c("sr_sr", "sr_sir", "sr_ff", "sir_sir", "sir_ff", "ff_ff"),
                       labels=c("SR:SR", "SR:SIR", "SR:FF", "SIR:SIR", "SIR:FF", "FF:FF")),
         label = ave(.$R_spike_count, .$comb, FUN=function(x)paste0("(n=",length(x), ")")),
         comb_labeled = paste(comb, label),
         group=factor(group, levels=c("CS", "CC", "CD")))

##############

# What changes with CC?

df200 %>%
  filter(group %in% c("CS", "CC")) %>%
  group_by(comb, group) %>%
  summarise("% of Pairs Correlated"=mean(p < 0.05) * 100) %>%
  ungroup() %>%
  ggplot(aes(y=`% of Pairs Correlated`, x=comb, fill=group)) + 
  geom_col(position=position_dodge()) +
  labs(x="", y="% Correlated") +
  coord_flip() +
  theme(legend.position = "right")


df200 %>%
  filter(group %in% c("CS", "CC"),
         has_ff == T) %>%
  group_by(comb, group) %>%
  summarise("% of Pairs Correlated"=mean(p < 0.05) * 100) %>%
  ungroup() %>%
  ggplot(aes(x=`% of Pairs Correlated`, y=comb, color=group, group=group)) + 
  geom_point(size=4, position =position_dodge(width=0.5)) +
  labs(y="") +
  xlim(50, 100) +
  ggtitle("Spontaneous Correlations") +
  theme(legend.position="right")


#######################



df200_mag <- df200 %>%
  filter(p < 0.05,
         group %in% c("CS", "CD")) %>%
  mutate(was_correlated = p < 0.05,
         corr_mag = abs(R_spike_count))

mod <- lm(corr_mag ~ comb * group, data=df200_mag)
anova(mod)  
summary(mod)

df200_mag %>%
  mutate(comb=factor(comb),
         comb=fct_reorder(comb, corr_mag, mean)) %>%
  ggplot(aes(x=comb, y=corr_mag, fill=group)) +
  geom_boxplot(width=0.5) +
  coord_flip() +
  labs(y="Magnitude of Correlation", x="") +
  theme(legend.position="right")

########################

# Were negative correlations more common in certain groups

df200_2 <- df200 %>%
  filter(p < 0.05, 
         group %in% c("CS", "CC")) %>%
  mutate(was_negative = R_spike_count < 0) 


df200_2 %>%
  group_by(comb, group) %>%
  summarise("% of Correlated Pairs Negative"=mean(R_spike_count < 0) * 100) %>%
  ungroup() %>%
  ggplot(aes(x=`% of Correlated Pairs Negative`, y=comb, color=group)) + 
  geom_point(size=4) +
  labs(y="") +
  xlim(0, 50) +
  ggtitle("Significantly Correlated Pairs") + 
  theme(legend.position="right")

mod <- glm(was_negative ~ group * comb, data=df200_2)
anova(mod, test="Chisq")
summary(mod)
