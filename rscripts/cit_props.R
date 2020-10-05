library(tidyverse)
library(scales)

colors <- c(
  slow_regular="#FF0100",
  fast_firing="#00B34E",
  slow_irregular="#806A9D",
  "Non Responder"="#7E8283",
  Inhibited="#31A6D9"
)

compare_cluster_props <- function(df, response_dm="response_dm", group="group", method="BH"){
  tab <- table(pull(df, response_dm), pull(df, group))
  n <- colSums(tab)
  
  p1 <- prop.test(c(tab[1, 1], tab[1, 2]), c(n[1], n[2]))$p.value
  p2 <- prop.test(c(tab[1, 1], tab[1, 3]), c(n[1], n[3]))$p.value
  p3 <- prop.test(c(tab[1, 2], tab[1, 3]), c(n[2], n[3]))$p.value
  
  p4 <- prop.test(c(tab[2, 1], tab[2, 2]), c(n[1], n[2]))$p.value
  p5 <- prop.test(c(tab[2, 1], tab[2, 3]), c(n[1], n[3]))$p.value
  p6 <- prop.test(c(tab[2, 2], tab[2, 3]), c(n[2], n[3]))$p.value
  
  p7 <- prop.test(c(tab[3, 1], tab[3, 2]), c(n[1], n[2]))$p.value
  p8 <- prop.test(c(tab[3, 1], tab[3, 3]), c(n[1], n[3]))$p.value
  p9 <- prop.test(c(tab[3, 2], tab[3, 3]), c(n[2], n[3]))$p.value
  
  
  
  p <- p.adjust(c(p1, p2, p3, p4, p5, p6, p7, p8, p9), method=method)
  mat <- matrix(p, nrow=3, byrow=T)
  colnames(mat) <- c("CSvCC", "CSvCD", "CCvCD")
  rownames(mat) <- levels(pull(df, response_dm))
  return(mat)
}

theme_set(theme_minimal() + 
            theme(axis.title.x = element_text(colour = "black", size=20),
                  axis.title.y = element_text(colour = "black", size=16),
                  strip.text = element_text(color="black", size=15),
                  axis.text.y = element_text(color="black", size=15),
                  axis.text.x = element_text(color="black", size=15, angle = 0, hjust=0.9),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=13)))

df <- read_csv(file.path("data", "cit_effects.csv")) %>%
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
    group = factor(group, levels=c("CS", "CC", "CD")))


###############


df_sr <- filter(df, cluster=="SR")
table(df_sr$response_dm, df_sr$group)

compare_cluster_props(df_sr, method="BH")

df_sir <- filter(df, cluster=="SIR")
table(df_sir$response_dm, df_sir$group)

compare_cluster_props(df_sir, method="BH")

df_ff <- filter(df, cluster=="FF")
table(df_ff$response_dm, df_ff$group)

compare_cluster_props(df_ff, method="BH")

######################


df %>% 
  filter(response_dm != "Stimulated",
         cluster != "FF") %>%
  mutate(cluster = fct_rev(cluster)) %>%
  ggplot(aes(x=group, fill=response_dm)) +
  geom_bar(position=position_fill()) +
  scale_fill_manual(values=colors) +
  scale_y_continuous(n.breaks = 3) +
  labs(x="", y="Proportion") +
  theme(axis.text.x = element_text(size=15, angle = 0, hjust=0.5)) +
  facet_grid(cols=vars(cluster), scales="free")
  
df %>% 
  filter(response_dm != "Stimulated",
         cluster == "SR",
         group != "CD") %>%
  mutate(cluster = fct_rev(cluster)) %>%
  group_by(group, cluster) %>%
  summarise(frac_inhibited = mean(response_dm == "Inhibited") * 100) %>%
  ungroup() %>%
  ggplot(aes(x=group, y=frac_inhibited)) +
  geom_col(fill='#31A6D9', width=0.6) +
  scale_y_continuous(breaks=c(0, 30, 60)) +
  ylim(0, 65) +
  labs(x="", y="% Inhibited") +
  theme(axis.text.x = element_text(size=15, angle = 0, hjust=0.5),
        axis.title.y= element_text(size=16)) +
  facet_grid(cols=vars(cluster), scales="free")
