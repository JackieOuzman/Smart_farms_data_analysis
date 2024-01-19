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
                        
Coomandook <- read.csv("X:/Therese_Jackie/smart_farms/sites/Coomandook/Coomandook_pen_vs_em_2023.csv", skip=2)
                                         
names(Coomandook)
# remove the na
Coomandook <- Coomandook %>% 
  filter(!is.na(Group))

##################################################################################
### edit the dataset - I am just looking at pen readings 
##################################################################################
for_Pen_correlation <- Coomandook %>% 
  dplyr::select("X0.25":"Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.") 


for_Pen_EM_correlation <- Coomandook %>% 
  dplyr::select("X0.25":"EM1m"      )%>% 
  dplyr::select(-"notes" )   
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
          file = "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/correltion_matrix_pen_em_flat.csv",
          row.names = FALSE)


Coomandook <- Coomandook %>% select(
  "max..Peak..resistance.value.up.to.50cm"    ,
  "location.in.the.profile.of.first.peak..up.to.50cm."   ,
  "The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.50cm"  ,
  "Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.",
  "X0.50" ,
  "EM05m" ,                                                              
  "EM1m"
)

Coomandook <- Coomandook %>% rename(
  Peak_Resistance = "max..Peak..resistance.value.up.to.50cm"    , #a
  Depth_to_peak = "location.in.the.profile.of.first.peak..up.to.50cm."   , #b 
  Depth_to_2.5MPa ="The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.50cm"  , #c
  Area_under_curve_to_2.5MPa ="Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.", #d
  Area_under_curve_to_50cm ="X0.50" #e
)





##################################################################################
### 1a. 
##################################################################################

EMShallowVPeak <- ggplot(Coomandook, aes(x=`EM05m`, y=`Peak_Resistance`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs( 
    x = "",
    y = "Peak \nresistance"     )
ggsave(
  device = "png",
  filename = "EMShallowVPeak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##1b. 
#################################################################################

EMShallowVDepth_to_peak <- ggplot(Coomandook, aes(x=`EM05m`, y=`Depth_to_peak`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(
    x = "",
    y = "Depth to peak"     )
ggsave(
  device = "png",
  filename = "EMShallowVDepth_to_peak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##1c. 
#################################################################################

EMShallowVDepth_to_2.5MPa <- ggplot(Coomandook, aes(x=`EM05m`, y=`Depth_to_2.5MPa`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(
    x = "",
    y = "Depth to 2.5MPa")
ggsave(
  device = "png",
  filename = "Depth_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##1d. 
#################################################################################

EMShallowVArea_under_curve_to_2.5MPa <- ggplot(Coomandook, aes(x=`EM05m`, y=`Area_under_curve_to_2.5MPa`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(
    x = "",
    y = "Area under curve to 2.5MPa")
ggsave(
  device = "png",
  filename = "Area_under_curve_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##1e. 
#################################################################################

EMShallowVArea_under_curve_to_50cm <- ggplot(Coomandook, aes(x=`EM05m`, y=`Area_under_curve_to_50cm`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(
    x = "",
    y = "Area under curve to 50cm")
ggsave(
  device = "png",
  filename = "Area_under_curve_to_50cm.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 


EMShallowVPeak
EMShallowVDepth_to_peak
EMShallowVDepth_to_2.5MPa
EMShallowVArea_under_curve_to_2.5MPa
EMShallowVArea_under_curve_to_50cm

################################################################################


################################################################################
### Group the plots


EMShallow_plots_Reg <-
  ggarrange(
    EMShallowVPeak,
    EMShallowVDepth_to_peak,
    EMShallowVDepth_to_2.5MPa,
    EMShallowVArea_under_curve_to_2.5MPa,
    EMShallowVArea_under_curve_to_50cm,
    
    #labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
    ncol = 2,
    nrow = 3
  ) 

EMShallow_plots_Reg_1 <-
  annotate_figure(EMShallow_plots_Reg, top = text_grob(
    "EM38 Shallow vs Penetrometer parameters. 
    Site: Coomandook",
    color = "Black",
    face = "bold",
    size = 14
  ))
EMShallow_plots_Reg_1
ggexport(EMShallow_plots_Reg_1, filename = "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/plots_regression/EM_Shallow_plots_Reg.png")


##################################################################################
### 2a. 
##################################################################################

EMDeepVPeak <- ggplot(Coomandook, aes(x=`EM1m`, y=`Peak_Resistance`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(
    x = "",
    y = "Peak \nresistance"     )
ggsave(
  device = "png",
  filename = "EMDeepVPeak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



################################################################################
##2b. 
#################################################################################

EMDeepVDepth_to_peak <- ggplot(Coomandook, aes(x=`EM1m`, y=`Depth_to_peak`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(
    x = "",
    y = "Depth to peak"     )
ggsave(
  device = "png",
  filename = "EMDeepVDepth_to_peak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##2c. 
#################################################################################

EMDeepVDepth_to_2.5MPa <- ggplot(Coomandook, aes(x=`EM1m`, y=`Depth_to_2.5MPa`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(
    x = "",
    y = "Depth to 2.5MPa")
ggsave(
  device = "png",
  filename = "EMDeepVSDepth_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##2d. 
#################################################################################

EMDeepVArea_under_curve_to_2.5MPa <- ggplot(Coomandook, aes(x=`EM1m`, y=`Area_under_curve_to_2.5MPa`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(
    x = "",
    y = "Area under curve to 2.5MPa")
ggsave(
  device = "png",
  filename = "EMDeepVsArea_under_curve_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##2e. 
#################################################################################

EMDeepVArea_under_curve_to_50cm <- ggplot(Coomandook, aes(x=`EM1m`, y=`Area_under_curve_to_50cm`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(
    x = "",
    y = "Area under curve to 50cm")
ggsave(
  device = "png",
  filename = "EMDeepVsArea_under_curve_to_50cm.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 
#################################################################################
#################################################################################
#################################################################################

######################################   EM DEEP #################################


EMDeepVPeak
EMDeepVDepth_to_peak
EMDeepVDepth_to_2.5MPa
EMDeepVArea_under_curve_to_2.5MPa
EMDeepVArea_under_curve_to_50cm

EMDeep_plots_Reg <-
  ggarrange(
    EMDeepVPeak,
    EMDeepVDepth_to_peak,
    EMDeepVDepth_to_2.5MPa,
    EMDeepVArea_under_curve_to_2.5MPa,
    EMDeepVArea_under_curve_to_50cm,
    
    #labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
    ncol = 2,
    nrow = 3
  ) 

EMDeep_plots_Reg_1 <-
  annotate_figure(EMDeep_plots_Reg, top = text_grob(
    "EM38 deep vs Penetrometer parameters. 
    Site: Coomandook",
    color = "Black",
    face = "bold",
    size = 14
  ))
EMDeep_plots_Reg_1
ggexport(EMDeep_plots_Reg_1, filename = "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/plots_regression/EM_Deep_plots_Reg.png")





