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

Bute <- read.csv("X:/Therese_Jackie/smart_farms/sites/Bute/EM_pts_Pen_for_analysis.csv", skip=2)
                                         
names(Bute)

##################################################################################
### edit the dataset - I am just looking at pen readings 
##################################################################################
for_Pen_correlation <- Bute %>% 
  dplyr::select("X0.25":"Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.")

for_Pen_EM_correlation <- Bute %>% 
  dplyr::select("X0.25":"EM1m"      )
names(for_Pen_EM_correlation)

#

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
          file = "X:/Therese_Jackie/smart_farms/sites/Bute/Analysis/correltion_matrix_pen_em_flat.csv",
          row.names = FALSE)






#################################################################################
### Plots
##################################################################################
names(Bute)
#Lets try ... based on the correlation matrix - not that its very good!
#1. "mean.resistance.value.for.the.profile" vs "EM05m"  
#1a. "medium.resistance.value.for.the.profile"                             
  
#2. "total" vs "EM05m"  
#3. "X0.50" vs "EM05m"
#4.  "max..Peak..resistance.value.for.the.profile"  vs "EM05m"   
#5.  "The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.75cm"     


#1. "mean.resistance.value.for.the.profile" vs "Deep"  
#2. "X0.50" vs "Deep"  


##################################################################################
### 1a. 
##################################################################################

EMShallowVMean <- ggplot(Bute, aes(x=`EM05m`, y=`mean.resistance.value.for.the.profile`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(#title = "Bute EM vs penetrometer ",
               #subtitle = "",
               #x = "EM mS/m (Shallow)", 
               x = "",
               y = "Mean \nresistance"     )
ggsave(
  device = "png",
  filename = "EMShallowVMean.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Bute/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 








##################################################################################
### 2a. "total" vs "Shallow" 
##################################################################################
EM_Shallow_total <- ggplot(Bute, aes(x=`EM05m`, y=`total`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(#title = "Bute EM vs penetrometer ",
       #subtitle = "",
       x = "",
       #x = "EM mS/m (depth Shallow)",
       y = "Area \nprofile"     )
ggsave(
  device = "png",
  filename = "EM_Shallow_profile.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Bute/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 







##################################################################################
### #3a. "max" vs "EM05m"  
##################################################################################

names(Bute)
EMShallow_Max <- ggplot(Bute, aes(x=`EM05m`, y=`max..Peak..resistance.value.for.the.profile`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Bute EM vs penetrometer ",
       #subtitle = "",
       x = "", 
       #x = "EM mS/m (depth Shallow)", 
       y = "Max \nresistance"     )
ggsave(
  device = "png",
  filename = "EMShallow_MaxResi.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Bute/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 





################################################################################

##################################################################################
### #5a. "location.in.the.profile.of.max..peak." vs "EM05mw"  
##################################################################################

names(Bute)
EMShallow_location_to_max_profile <- ggplot(Bute, aes(x=`EM05m`, y=`location.in.the.profile.of.max..peak.`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Bute EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Shallow)", 
    y = "Depth to peak \nfor profile"     )
ggsave(
  device = "png",
  filename = "EMShallow_location_to_max_profile.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Bute/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 
################################################################################


##################################################################################
### #5a. "The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.75cm" vs "EM05m"  
##################################################################################

names(Bute)
EMShallow_depth_exceeds_2_5 <- ggplot(Bute, aes(x=`EM05m`, y=`The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.50cm`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(#title = "Bute EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Shallow)", 
    y = "Depth exceeds\n2.5MPa"     )
ggsave(
  device = "png",
  filename = "EMShallow_depth_exceeds_2_5.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Bute/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 
################################################################################





EMShallowVMean
EMShallow_Max
EM_Shallow_total
EMShallow_location_to_max_profile
EMShallow_depth_exceeds_2_5

################################################################################
### Group the plots






EMShallow_plots_Reg <-
  ggarrange(
    EMShallowVMean,
    EMShallow_Max,
    EM_Shallow_total,
    EMShallow_location_to_max_profile,
    EMShallow_depth_exceeds_2_5,
    
    #labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
    ncol = 2,
    nrow = 3
  ) 

EMShallow_plots_Reg_1 <-
  annotate_figure(EMShallow_plots_Reg, top = text_grob(
    "EM38 Shallow vs Penetrometer parameters. 
    Site: Bute",
    color = "Black",
    face = "bold",
    size = 14
  ))
EMShallow_plots_Reg_1
ggexport(EMShallow_plots_Reg_1, filename = "X:/Therese_Jackie/smart_farms/sites/Bute/Analysis/plots_regression/EM_Shallow_plots_Reg.png")

##################################################################################
######################################   EM DEEP #################################

### Most of the EM deep readings are negative I wont use them
#