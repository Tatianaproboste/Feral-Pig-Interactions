# Author: Abigail Turnlund & Tatiana Proboste
# date: 08/12/23


library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
set.seed(101)

# call the script created in 12_statistical_test
df <- read_csv("./node_level_network_direct_indirect.csv") 
df <- df %>% 
  janitor::clean_names() %>% 
  mutate(log_betweenness = log(betweenness)) %>% 
  select(centrality, degree, strength, betweenness, log_betweenness,
         type, sex, year,threshold, population,number_of_nodes)
df$sex <- tolower(df$sex)

df1 <- df %>% 
  pivot_longer(
    cols = c(centrality, degree, strength, betweenness, log_betweenness),
    names_to = "variable",
    values_to = "value"
  )

# Plot to visualize direct and indirect network - node level
# to visualise just direct or indirect network, we can use the code below to  filter

# df_direct <- df1 %>% filter(type=="direct") 


tiff(filename="./Plots/node_level_network.tiff", units="in", width=10, height=6, res=300)
ggplot(df1, aes(x=type, y=value, color=type)) +
  geom_jitter(color="#882255", size=0.4, alpha=0.9) +
  geom_boxplot(color="#882255", alpha=0.5)+
  facet_wrap(~variable, scales="free") +
 # scale_color_manual(values = my_palette) +
  theme_ipsum()+
  labs(title="c",
       x="type of network",
       y="Value",
       color="variable") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=14),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=10),
        legend.title = element_text(face="bold", size=10),
        legend.text = element_text(size=10),
        strip.text = element_text(face="bold", size=12),
        axis.text = element_text(size=10))
dev.off()
