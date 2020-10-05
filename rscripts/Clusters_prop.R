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
                  legend.text = element_text(colour="black", size=13)))

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

df %>% 
  ggplot(aes(x=group, fill=cluster)) +
  geom_bar(position=position_dodge()) +
  scale_fill_manual(values=colors) + 
  labs(x="", y="Neuron Count") +
  theme(axis.text.x = element_text(size=15, angle = 0, hjust=0.5))

#############

mod <- chisq.test(table(df$group, df$cluster))
mod

z.test <- function(z){
  2 * pnorm(-abs(z))
}

dfz <- as.data.frame(mod$stdres) %>%
  rename(response=Var1, cluster=Var2, z=Freq)

dfz %>%
  mutate(p = z.test(z),
         pa = p.adjust(p, method="hochberg"))


########################

df = df


contrasts(df$group) <- cbind(
  "tvc"=c(-2, 1, 1),
  "wvc"=c(0, -1, 1)
)
##########################

compare_cluster_props <- function(df, cluster="cluster", group="group", method="BH"){
  tab <- table(pull(df, cluster), pull(df, group))
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
  colnames(mat) <- c("CSvCC", "CSvCD", "CSvCD")
  rownames(mat) <- levels(pull(df, cluster))
  return(mat)
}

compare_cluster_props(df, method="BH")
table(df$cluster, df$group) / colSums(table(df$cluster, df$group)) * 100

