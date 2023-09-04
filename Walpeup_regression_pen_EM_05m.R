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

##################################################################################
### edit the dataset - I am just looking at pen readings 
##################################################################################
for_Pen_correlation <- Walpeup %>% 
  dplyr::select("X0.25":"Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.75cm.")

for_Pen_EM_correlation <- Walpeup %>% 
  dplyr::select("X0.25":"EM_05m_PRE")

# #################################################################################
# ### correlation options - 1
# ##################################################################################
# 
# correltion_matrix_pen_1 <- cor(for_Pen_correlation, method = c("pearson"#, 
#                                                              #use = "complete.obs" #This is for missing data - but doesnt work with my dataset
#                                                              ))
# correltion_matrix_pen_1 <- round(correltion_matrix_pen, 2)

#################################################################################
### correlation options - 2
##################################################################################

correltion_matrix_pen_em <- rcorr(as.matrix(for_Pen_EM_correlation))


#function to flatten output
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
###use above function to flatten correlations
correltion_matrix_pen_em_flat <- flattenCorrMatrix(correltion_matrix_pen_em$r, correltion_matrix_pen_em$P)



write.csv(correltion_matrix_pen_em_flat,
          file = "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/correltion_matrix_pen_em_flat.csv",
          row.names = FALSE)




# #################################################################################
# ### correlation options - 3
# ##################################################################################
# chart.Correlation(for_Pen_correlation, histogram=TRUE, pch=19)



#################################################################################
### Plots
##################################################################################
names(for_Pen_EM_correlation)
#Lets try ... based on the correlation matrix - not that its very good!
#1. "mean.resistance.value.for.the.profile" vs "EM_05m_PRE"  
#2. "max..Peak..resistance.value.for.the.profile" vs "EM_05m_PRE"  
#3. "location.in.the.profile.of.first.peak..up.to.50cm." vs "EM_05m_PRE"  
#4. "The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.75cm" vs "EM_05m_PRE"  


#1. "mean.resistance.value.for.the.profile" vs "EM_1m_PRED"  
#2. "The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.75cm" vs "EM_1m_PRED"  

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

EM05VMean <- ggplot(Walpeup, aes(x=`EM_05m_PRE`, y=`mean.resistance.value.for.the.profile`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_point(data = Walpeup_drift_pts, colour = "red", size=1.0)+
  labs(#title = "Walpeup EM vs penetrometer ",
               #subtitle = "",
               #x = "EM mS/m (depth 0.5meter)", 
               x = "",
               y = "Mean \nresistance"     )
ggsave(
  device = "png",
  filename = "EM05VMean.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



#https://link.springer.com/article/10.1023/b:prag.0000040807.18932.80
#Sandy soils less than 10 - but this is not calibrated

EM05VMean_sand <-Walpeup %>% filter(EM_05m_PRE < 10) %>% 
  ggplot(aes(x=`EM_05m_PRE`, y=`mean.resistance.value.for.the.profile`)) + 
  geom_point(alpha =0.5, size=0.3)+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_point(data = Walpeup_drift_pts %>% filter(EM_05m_PRE < 10) , colour = "red",size=1.0)+
  labs(#title = "Walpeup EM vs penetrometer - Sand only",
       #subtitle = "",
       x = "", 
       #x = "EM mS/m (depth 0.5meter)", 
       y = "Mean \nresistance"
  )+
  theme(panel.background = element_rect(fill = "#BFD5E3"))





ggsave(
  device = "png",
  filename = "EM05VMean_sand.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



##################################################################################
### 2a. "max..Peak..resistance.value.for.the.profile" vs "EM_05m_PRE" 
##################################################################################
EM05VMax <- ggplot(Walpeup, aes(x=`EM_05m_PRE`, y=`max..Peak..resistance.value.for.the.profile`)) + 
  geom_point(alpha =0.5, size=0.3)+theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_point(data = Walpeup_drift_pts, colour = "red", size=1.0)+
  labs(#title = "Walpeup EM vs penetrometer ",
       #subtitle = "",
       x = "",
       #x = "EM mS/m (depth 0.5meter)",
       y = "Max \nresistance"     )
ggsave(
  device = "png",
  filename = "EM05VMax.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



################################################################################
### 2b. "max..Peak..resistance.value.for.the.profile" vs "EM_05m_PRE"  SAND


EM05VMax_sand <-Walpeup %>% filter(EM_05m_PRE < 10) %>% 
  ggplot( aes(x=`EM_05m_PRE`, y=`max..Peak..resistance.value.for.the.profile`)) + 
  geom_point(alpha =0.5, size=0.3)+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_point(data = Walpeup_drift_pts %>% filter(EM_05m_PRE < 10) , colour = "red",size=1.0)+
  labs(#title = "Walpeup EM vs penetrometer sand ",
       #subtitle = "",
       x = "",
       #x = "EM mS/m (depth 0.5meter)",
       y = "Max \nresistance"     )+
  theme(panel.background = element_rect(fill = "#BFD5E3"))

ggsave(
  device = "png",
  filename = "EM05VMax_sand.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



##################################################################################
### #3a. "location.in.the.profile.of.first.peak..up.to.50cm." vs "EM_05m_PRE"  
##################################################################################

names(Walpeup)
EM05Vlocation_peak <- ggplot(Walpeup, aes(x=`EM_05m_PRE`, y=`location.in.the.profile.of.first.peak..up.to.50cm.`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_point(data = Walpeup_drift_pts, colour = "red", size=1.0)+
  labs(#title = "Walpeup EM vs penetrometer ",
       #subtitle = "",
       x = "", 
       #x = "EM mS/m (depth 0.5meter)", 
       y = "Depth of peak \nresistance"     )
ggsave(
  device = "png",
  filename = "EM05Vlocation_peak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



################################################################################
### #3b. "location.in.the.profile.of.first.peak..up.to.50cm." vs "EM_05m_PRE"    SAND


EM05Vlocation_peak_sand <-Walpeup %>% filter(EM_05m_PRE < 10) %>% 
  ggplot( aes(x=`EM_05m_PRE`, y=`location.in.the.profile.of.first.peak..up.to.50cm.`)) + 
  geom_point(alpha =0.5, size=0.3)+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_point(data = Walpeup_drift_pts %>% filter(EM_05m_PRE < 10) , colour = "red", size=1.0)+
  labs(#title = "Walpeup EM vs penetrometer sand ",
       #subtitle = "",
       #x = "EM mS/m (depth 0.5meter)", 
       x = "", 
       y = "Depth of peak \nresistance"     )+
  theme(panel.background = element_rect(fill = "#BFD5E3"))

ggsave(
  device = "png",
  filename = "EM05Vlocation_peak_sand.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



##################################################################################
### #4a. "The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.75cm" vs "EM_05m_PRE"  
##################################################################################

names(Walpeup)
EM05Vs_depth_exc2Mpa <- ggplot(Walpeup, aes(x=`EM_05m_PRE`, y=`The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.75cm`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_point(data = Walpeup_drift_pts, colour = "red", size=1.0)+
  labs(#title = "Walpeup EM vs penetrometer ",
       #subtitle = "",
       #x = "EM mS/m (depth 0.5meter)",
       x = "", 
       y = "Depth exceedes \n2.5MPa"     )
ggsave(
  device = "png",
  filename = "EM05vsdepth_exc2Mpa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



################################################################################
### #4b. "The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.75cm" vs "EM_05m_PRE"    SAND


EM05Vs_depth_exc2Mpa_sand <-Walpeup %>% filter(EM_05m_PRE < 10) %>% 
  ggplot( aes(x=`EM_05m_PRE`, y=`The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.75cm`)) + 
  geom_point(alpha =0.5, size=0.3)+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_point(data = Walpeup_drift_pts %>% filter(EM_05m_PRE < 10) , colour = "red",size=1.0)+
  labs(#title = "Walpeup EM vs penetrometer sand ",
       #subtitle = "",
       #x = "EM mS/m (depth 0.5meter)", 
       x = "",
       y = "Depth exceedes \n2.5MPa"     )  +
  theme(panel.background = element_rect(fill = "#BFD5E3"))

ggsave(
  device = "png",
  filename = "EM05Vs_depth_exc2Mpa_sand.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 


################################################################################
### Group the plots

#1
EM05VMean
EM05VMean_sand

#2
EM05VMax
EM05VMax_sand

#3
EM05Vlocation_peak
EM05Vlocation_peak_sand
#4
EM05Vs_depth_exc2Mpa
EM05Vs_depth_exc2Mpa_sand


EM0_5m_plots_Reg <-
  ggarrange(
    EM05VMean,
    EM05VMean_sand,
    EM05VMax,
    EM05VMax_sand,
    EM05Vlocation_peak,
    EM05Vlocation_peak_sand,
    EM05Vs_depth_exc2Mpa,
    EM05Vs_depth_exc2Mpa_sand,
    
    #labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
    ncol = 2,
    nrow = 4
  ) 

EM0_5m_plots_Reg_v1 <-
  annotate_figure(EM0_5m_plots_Reg, top = text_grob(
    "EM38 Depth 0.5m vs Penetrometer parameters. 
    Site: Walpeup",
    color = "Black",
    face = "bold",
    size = 14
  ))
EM0_5m_plots_Reg_v1
ggexport(EM0_5m_plots_Reg_v1, filename = "X:/Therese_Jackie/smart_farms/sites/Walpeup/Analysis/plots_regression/EM0_5m_plots_Reg.png")
