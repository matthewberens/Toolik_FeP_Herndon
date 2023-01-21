#MJBERENS
#January 20 2023

#Geochemical analysis of field samples

#Load libraries and set directory
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
setwd("~/Documents/Toolik_FeP_Herndon")

#General plot formatting
theme_mb1 <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=1, fill = NA),
          plot.title = element_text(hjust = 0.5, size = 10),
          plot.subtitle = element_text(hjust = 0.5, size = 10, lineheight = 1.5),
          axis.text = element_text(size = 10, color = "black"),
          axis.title = element_text(size = 10, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=10, face="bold"), #facet labels
          strip.text.y = element_text(size=10, face="bold", angle = 270) #facet labels
    )
}



FeP_geochem <- read.csv("data/Toolik_Fep_geochem_data.csv")

filtered_geochem_pct <- FeP_geochem %>%
  subset(hillslope_data == "N") %>%
  select(-horizon, -matches("mgkg"), -hillslope_data, -LOI_pct) %>%
  mutate(other = 100 - rowSums(across((contains("pct")))))

filtered_geochem_pct %>%
  pivot_longer(c("Si_pct":"Ca_pct", "other"), names_to = "element", values_to = "value") %>%
  ggplot(aes(x = sampleID, y = value, fill = factor(element, levels = c("other", "Al_pct", "Ca_pct", "Si_pct", "Fe_pct")))) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(~site, scales = "free_x") +
  theme_mb1()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = NULL, y = "Atomic % wt.")
