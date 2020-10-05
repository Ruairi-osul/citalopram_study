library(tidyverse)


colors <- c(
  "FALSE"="#7E8283",
  "TRUE"="#F6931D",
  "HAMILTON"="#00B34E",
  "CITWAY"="#806A9D"
)

df <- read_csv(file.path("data", "baseline.csv")) %>%
  filter(group_name %in% c("chronic_citalopram", 
                           "chronic_saline", 
                           "chronic_saline_", 
                           "citalopram_continuation", 
                           "citalopram_discontinuation"),
         cluster != "no_baseline") %>%
  mutate(group_name = factor(group_name, levels=c("chronic_saline", 
                                                  "chronic_saline_", 
                                                  "citalopram_continuation",
                                                  "chronic_citalopram",
                                                  "citalopram_discontinuation"),
                             labels=c("Chronic Saline", "Chronic Saline (No Footshock)",
                                      "Chronic Citalopram", "Chronic Citalopram (No Footshock)",
                                      "Citalopram Discontinuation")))


df %>%
  select(session_name, experiment_name) %>%
  distinct() %>%
  mutate(has_one_hour = experiment_name != "HAMILTON") %>%
  group_by(has_one_hour) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count) * 100) %>%
  ggplot(aes(x=1, y=pct, fill=has_one_hour)) +
  geom_bar(position=position_stack(), stat="identity", width=2) + 
  geom_text(aes(label=count), 
            position=position_stack(vjust=0.5), 
            size=6, 
            color="white") +
  scale_fill_manual(values=colors,
                    labels=c("20 Minutes", "60 Minutes"),
                    name="Baseline Duration") +  
  ggtitle("Number of Recordings") +
  theme_void() +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14),
  )


df %>%
  select(session_name, experiment_name) %>%
  distinct() %>%
  mutate(has_one_hour = experiment_name != "HAMILTON") %>%
  group_by(has_one_hour) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count) * 100) %>%
  ggplot(aes(x=1, y=pct, fill=has_one_hour)) +
  geom_bar(position=position_stack(), stat="identity", width=2) + 
  geom_text(aes(label=count), 
            position=position_stack(vjust=0.5), 
            size=6, 
            color="white") +
  scale_fill_manual(values=colors,
                    labels=c("No Footshock", "Footshock"),
                    name="") +  
  ggtitle("Number of Recordings") +
  theme_void() +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14),
  )

df %>%
  select(session_name, group_name) %>%
  distinct() %>%
  count(group_name) %>%
  ggplot(aes(x=1, y=n, fill=group_name)) +
  geom_bar(position=position_stack(), stat="identity", width=2) + 
  geom_text(aes(label=n), 
            position=position_stack(vjust=0.5), 
            size=6, 
            color="white") +
  scale_fill_discrete(name="") +
  ggtitle("Number of Recordings") +
  theme_void() +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14),
  )
