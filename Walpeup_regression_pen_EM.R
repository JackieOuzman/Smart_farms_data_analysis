library(MESS)
library(tidyverse)
library(readxl)
library(dplyr)
#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
library("Hmisc")
library(ggplot2)

library(plotly)


library(hrbrthemes)
library(viridis)
library(forcats)
library(hrbrthemes)
library(stringr)
#install.packages("hrbrthemes")

library(ggpubr)



#http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software


##################################################################################
### import dataset  
##################################################################################

Walpeup <- read.csv("X:/Therese_Jackie/smart_farms/sites/Walpeup/Walpeup_EM_PEN_for _analysis.csv", skip=2)

names(Walpeup)



#################################################################################
### Plots
##################################################################################
names(for_Pen_EM_correlation)
#Lets try ... based on the correlation matrix - not that its very good!


#1. "mean.resistance.value.for.the.profile" vs "EM_1m_PRED"  
#4. "The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.75cm" vs "EM_1m_PRED"  

str(Walpeup)

Walpeup <- Walpeup %>% 
  mutate(GPS_Drift = case_when(
    Insertion == 83 ~ "drift" ,
     Insertion == 82 ~ "drift" ,
     Insertion == 81 ~ "drift" ,
     Insertion == 80 ~ "drift" ,
     Insertion == 79 ~ "drift" ,
     Insertion == 78 ~ "drift" ,
     Insertion == 77 ~ "drift" ,
     Insertion == 76 ~ "drift" ,
     Insertion == 75 ~ "drift" ,
    TRUE ~ "no_drift"
  ))

Walpeup_drift_pts <- Walpeup %>% filter(GPS_Drift == "drift")

##################################################################################
### 1a. 
##################################################################################
names(Walpeup)
EM1VMean <- ggplot(Walpeup, aes(x=`EM_1m_PRED`, y=`mean.resistance.value.for.the.profile`)) + 
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_point(data = Walpeup_drift_pts, colour = "red")+
  labs(#title = "Walpeup EM vs penetrometer ",
               #subtitle = "",
               #x = "EM mS/m (depth 1meter)", 
               x = "",
               y = "Mean \nresistance"     )
ggsave(
  device = "png",
  filename = "EM1VMean.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 





EM1VMean_sand <-Walpeup %>% filter(EM_1m_PRED < 10) %>% 
  ggplot(aes(x=`EM_1m_PRED`, y=`mean.resistance.value.for.the.profile`)) + 
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_point(data = Walpeup_drift_pts %>% filter(EM_1m_PRED < 10) , colour = "red")+
  labs(#title = "Walpeup EM vs penetrometer - Sand only",
       #subtitle = "",
       x = "", 
       #x = "EM mS/m (depth 0.5meter)", 
       y = "Mean \nresistance"
  )+
  theme(panel.background = element_rect(fill = "#BFD5E3"))





ggsave(
  device = "png",
  filename = "EM1VMean_sand.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 




##################################################################################
### #4a. "The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.75cm" vs "EM_05m_PRE"  
##################################################################################

names(Walpeup)
EM1Vs_depth_exc2Mpa <- ggplot(Walpeup, aes(x=`EM_1m_PRED`, y=`The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.75cm`)) + 
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_point(data = Walpeup_drift_pts, colour = "red")+
  labs(#title = "Walpeup EM vs penetrometer ",
       #subtitle = "",
       #x = "EM mS/m (depth 1meter)",
       x = "", 
       y = "Depth exceedes \n2.5MPa"     )
ggsave(
  device = "png",
  filename = "EM1vsdepth_exc2Mpa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



################################################################################
### #4b. "The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.75cm" vs "EM_05m_PRE"    SAND


EM1Vs_depth_exc2Mpa_sand <-Walpeup %>% filter(EM_1m_PRED < 10) %>% 
  ggplot( aes(x=`EM_05m_PRE`, y=`The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.75cm`)) + 
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_point(data = Walpeup_drift_pts %>% filter(EM_1m_PRED < 10) , colour = "red")+
  labs(#title = "Walpeup EM vs penetrometer sand ",
       #subtitle = "",
       #x = "EM mS/m (depth 1meter)", 
       x = "",
       y = "Depth exceedes \n2.5MPa"     )  +
  theme(panel.background = element_rect(fill = "#BFD5E3"))

ggsave(
  device = "png",
  filename = "EM1Vs_depth_exc2Mpa_sand.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 


################################################################################
### Group the plots

#1
EM1VMean
EM1VMean_sand


#4
EM1Vs_depth_exc2Mpa
EM1Vs_depth_exc2Mpa_sand


EM_1m_plots_Reg <-
  ggarrange(
    EM1VMean,
   # EM1VMean_sand,
    
    EM1Vs_depth_exc2Mpa,
    #EM1Vs_depth_exc2Mpa_sand,
    
    #labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
    ncol = 2,
    nrow = 1
  ) 

EM_1m_plots_Reg_v1 <-
  annotate_figure(EM_1m_plots_Reg, top = text_grob(
    "EM38 Depth 1 m vs Penetrometer parameters. 
    Site: Walpeup",
    color = "Black",
    face = "bold",
    size = 14
  ))
EM_1m_plots_Reg_v1
ggexport(EM_1m_plots_Reg_v1, filename = "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/plots_regression/EM_1m_plots_Reg.png")
