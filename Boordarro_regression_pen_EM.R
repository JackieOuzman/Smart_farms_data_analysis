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

Boordaroo <- read.csv("X:/Therese_Jackie/smart_farms/sites/Boordaroo/Boordaroo_pen_2023_for_analysis.csv", skip=2)
                                         
names(Boordaroo)

##################################################################################
### edit the dataset - I am just looking at pen readings 
##################################################################################
for_Pen_correlation <- Boordaroo %>% 
  dplyr::select("X0.25":"Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.70cm.")

for_Pen_EM_correlation <- Boordaroo %>% 
  dplyr::select("X0.25":"EM05m", -"notes"      )
names(for_Pen_EM_correlation)

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
          file = "X:/Therese_Jackie/smart_farms/sites/Boordaroo/Analysis/correltion_matrix_pen_em_flat.csv",
          row.names = FALSE)




# #################################################################################
# ### correlation options - 3
# ##################################################################################
# chart.Correlation(for_Pen_correlation, histogram=TRUE, pch=19)



#################################################################################
### Plots
##################################################################################
names(Boordaroo)
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

EMShallowVMean <- ggplot(Boordaroo, aes(x=`EM05m`, y=`mean.resistance.value.for.the.profile`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(#title = "Boordaroo EM vs penetrometer ",
               #subtitle = "",
               #x = "EM mS/m (Shallow)", 
               x = "",
               y = "Mean \nresistance"     )
ggsave(
  device = "png",
  filename = "EMShallowVMean.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Boordaroo/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 


# EMShallowVMedium <- ggplot(Boordaroo, aes(x=`EM05m`, y=`medium.resistance.value.for.the.profile`)) + 
#   geom_point(alpha =0.5, size=0.3)+
#   theme_bw()+
#   geom_smooth(method = lm, se = FALSE) +
#   stat_regline_equation(
#     aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
#     formula = (y ~ x)
#   ) +
#   labs(#title = "Boordaroo EM vs penetrometer ",
#     #subtitle = "",
#     #x = "EM mS/m (Shallow)", 
#     x = "",
#     y = "Medium \nresistance"     )







##################################################################################
### 2a. "EX25_50" vs "Shallow" 
##################################################################################
EM_Shallow_total <- ggplot(Boordaroo, aes(x=`EM05m`, y=`total`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(#title = "Boordaroo EM vs penetrometer ",
       #subtitle = "",
       x = "",
       #x = "EM mS/m (depth Shallow)",
       y = "Area \nprofile"     )
ggsave(
  device = "png",
  filename = "EM_Shallow_profile.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Boordaroo/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 







##################################################################################
### #3a. "X50.72.5" vs "EM05m"  
##################################################################################

names(Boordaroo)
EMShallow_AreaX0_50 <- ggplot(Boordaroo, aes(x=`EM05m`, y=`X0.50`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Boordaroo EM vs penetrometer ",
       #subtitle = "",
       x = "", 
       #x = "EM mS/m (depth Shallow)", 
       y = "Area \n0-50cm"     )
ggsave(
  device = "png",
  filename = "EMShallow_Area0_50.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Boordaroo/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 




##################################################################################
### #4a. "max..Peak..resistance.value.for.the.profile" vs "EM05m"  
##################################################################################

names(Boordaroo)
EMShallow_MaxPeak_profile <- ggplot(Boordaroo, aes(x=`EM05m`, y=`max..Peak..resistance.value.for.the.profile`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Boordaroo EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth EM05m)", 
    y = "Max \nresistance"     )
ggsave(
  device = "png",
  filename = "EMShallow_MaxPeak_profile.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Boordaroo/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



################################################################################

##################################################################################
### #5a. "location.in.the.profile.of.first.peak..up.to.50cm." vs "EM05mw"  
##################################################################################

names(Boordaroo)
EMShallow_location2_5_top50 <- ggplot(Boordaroo, aes(x=`EM05m`, y=`location.in.the.profile.of.first.peak..up.to.50cm.`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Boordaroo EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Shallow)", 
    y = "Depth to peak \nup to 50cm"     )
ggsave(
  device = "png",
  filename = "EMShallow_dpeth_to_peak_top50.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Boordaroo/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 
################################################################################


##################################################################################
### #5a. "The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.75cm" vs "EM05mw"  
##################################################################################

names(Boordaroo)
EMShallow_depth_exceeds_2_5 <- ggplot(Boordaroo, aes(x=`EM05m`, y=`The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.70cm`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(#title = "Boordaroo EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Shallow)", 
    y = "Depth exceeds\n2.5MPa"     )
ggsave(
  device = "png",
  filename = "EMShallow_depth_exceeds_2_5.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Boordaroo/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 
################################################################################





EMShallowVMean
EM_Shallow_total
EMShallow_AreaX0_50

EMShallow_location2_5_top50
EMShallow_depth_exceeds_2_5

################################################################################
### Group the plots






EMShallow_plots_Reg <-
  ggarrange(
    EMShallowVMean,
    EM_Shallow_total,
    EMShallow_AreaX0_50,
    EMShallow_location2_5_top50,
    EMShallow_depth_exceeds_2_5,
    
    #labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
    ncol = 2,
    nrow = 3
  ) 

EMShallow_plots_Reg_1 <-
  annotate_figure(EMShallow_plots_Reg, top = text_grob(
    "EM38 Shallow vs Penetrometer parameters. 
    Site: Boordaroo",
    color = "Black",
    face = "bold",
    size = 14
  ))
EMShallow_plots_Reg_1
ggexport(EMShallow_plots_Reg_1, filename = "X:/Therese_Jackie/smart_farms/sites/Boordaroo/Analysis/plots_regression/EM_Shallow_plots_Reg.png")

##################################################################################
######################################   EM DEEP #################################


##################################################################################
### #1a. "mean.resistance.value.for.the.profile" vs "Deep"  
##################################################################################

names(Boordaroo)
EMDeep_mean_resistance <- ggplot(Boordaroo, aes(x=`EM1m`, y=`mean.resistance.value.for.the.profile`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(#title = "Boordaroo EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Deep)", 
    y = "Mean\nresistance"     )
ggsave(
  device = "png",
  filename = "EMDeep_mean_resistance.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Boordaroo/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 
################################################################################


##################################################################################
### #2a. "total" vs "Deep"  
##################################################################################

names(Boordaroo)
EMDeep_area_profile <- ggplot(Boordaroo, aes(x=`EM1m`, y=`total`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(#title = "Boordaroo EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Deep)", 
    y = "Area\nprofile"     )
ggsave(
  device = "png",
  filename = "EMDeep_area_profile.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Boordaroo/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 
################################################################################
##################################################################################
### #3a. "X0.50" vs "Deep"  
##################################################################################

names(Boordaroo)
EMDeep_area_top_50 <- ggplot(Boordaroo, aes(x=`EM1m`, y=`X0.50`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(#title = "Boordaroo EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Deep)", 
    y = "Area\ntop 50cm"     )
ggsave(
  device = "png",
  filename = "EMDeep_area_top_50.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Boordaroo/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 
################################################################################

EMDeep_mean_resistance
EMDeep_area_profile
EMDeep_area_top_50


EMDeep_plots_Reg <-
  ggarrange(
    EMDeep_mean_resistance,
    EMDeep_area_profile,
    EMDeep_area_top_50,
    ncol = 2,
    nrow = 2
  ) 

EMDeep_plots_Reg_1 <-
  annotate_figure(EMDeep_plots_Reg, top = text_grob(
    "EM38 deep vs Penetrometer parameters. 
    Site: Boordaroo",
    color = "Black",
    face = "bold",
    size = 14
  ))
EMDeep_plots_Reg_1
ggexport(EMDeep_plots_Reg_1, filename = "X:/Therese_Jackie/smart_farms/sites/Boordaroo/Analysis/plots_regression/EM_deep_plots_Reg.png")
