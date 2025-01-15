# Author: Abigail Turnlund
# Date: 10/11/2023 
# This code is to create the bubble plot (figure 6)


library(dplyr)
library(ggplot2)

indirect_data <- read.csv("Contact_Rate_Indirect.csv", row.names = "X")
direct_data <- read.csv("Contact_Rate_Direct.csv", row.names = "X")

# Add a new column specifying if data is direct or indirect contact
indirect_data <- indirect_data %>%
  mutate(Data = "Indirect")

direct_data <- direct_data %>%
  mutate(Data = "Direct")

# Combine to graph
dat <- rbind(direct_data, indirect_data)

# Relabel sounder names in graph
Sounder_names <- c(
  `0` = "Between Sounders",
  `1` = "Within Sounders",
  "female_female" = "Female - Female",
  "male_male" = "Male - Male",
  "mixed" = "Female - Male"
)

plot_labeller <- function(variable,value){
  if (variable=='facet1') {
    return(facet1_names[value])
  } else if (variable=='facet2') {
    return(facet2_names[value])
  } else {
    return(as.character(value))
  }
}

# Plot
tiff(filename="Contact_Bubble.tiff", units="in", width=10, height=8, res=300)
ggplot(dat, aes(x=population, y=yr, size=mean, color = Data)) +
  geom_point(alpha=0.6, position = position_dodge(width = 0.2)) +
  xlab("Population") +
  ylab("Year") +
  scale_color_manual(values = c("#44aa99", "#882255")) +
  scale_size(range=c(2,10), #expand=c(2,0),
             breaks=c(0.1,1,10),
             labels=c("<0.1","1","10"),
             guide="legend") +
  labs(color="Contact", size = "Mean Contact Rate", title = "5 Meter Threshold") +
  theme(axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 15),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#dfe0e0"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                        colour = "#dfe0e0"),
        plot.title = element_text(hjust = 0.5),
        legend.key = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(fill = "#FFE5E5"),
        panel.border = element_rect(size = 0.5,
                                    colour = "#dfe0e0",
                                    fill = NA),
        legend.box.background = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  scale_y_continuous(breaks = seq(2017, 2023, by = 1)) +
  facet_grid(combined_sex ~ contact_type, labeller = as_labeller(Sounder_names))
dev.off()
