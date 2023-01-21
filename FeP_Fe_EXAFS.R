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

FeP_Fe_EXAFS <- read.csv("data/Toolik_FeP_Fe_EXAFS_data.csv")

Fe_EXAFS_calcs <-
  FeP_Fe_EXAFS %>%
  mutate(Fe_III_org = ((Fe_III_oxalate + Fe_dextran + Fe_III_citrate)/scaleby),
         Fe_III_oxide = ((hematite + goethite + ferrihydrite + (magnetite*(2/3)))/scaleby),
         Fe_II_oxide = ((FeO + magnetite/3) /scaleby),
         Fe_III_P = ((Fe_III_phosphate + vivianite)/scaleby),
         Fe_II_Si = ((biotite + chlorite_ripidolite)/scaleby),
         Fe_II_S = ((pyrite + FeS)/scaleby),
         other = (1- rowSums(across((Fe_III_org:Fe_II_S)))))


filtered_Fe_EXAFS <- Fe_EXAFS_calcs %>%
  select(-hillslope_data, -c(rfactor:scaleby)) %>%
  mutate(cleanID = paste(area,site,plot,date, sep = "_")) 


filtered_Fe_EXAFS %>%
  pivot_longer(c("Fe_III_org":"other"), names_to = "name", values_to = "value") %>%
  ggplot(aes(x = cleanID, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(~area, scales = "free_x") +
  theme_mb1()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = NULL, y = "Atomic % wt.")

