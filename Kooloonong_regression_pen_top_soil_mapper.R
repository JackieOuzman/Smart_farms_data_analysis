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

Kooloonong <- read.csv("X:/Therese_Jackie/smart_farms/sites/Kooloonong/data_processing_jaxs/Penetrometer2022/Kooloonong penetrometer readings 2022.csv", skip=2)
                                         
names(Kooloonong)

##################################################################################
### edit the dataset - I am just looking at pen readings 
##################################################################################
for_Pen_correlation <- Kooloonong %>% 
  dplyr::select("X0.25":"Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.")

for_Pen_EM_correlation <- Kooloonong %>% 
  dplyr::select("X0.25":"Krig_D2I"      )
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
          file = "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/correltion_matrix_pen_em_flat.csv",
          row.names = FALSE)






#################################################################################
### Plots
##################################################################################
names(Kooloonong)
#Lets try ... based on the correlation matrix - not that its very good!
  
Kooloonong <- Kooloonong %>% select(
  "max..Peak..resistance.value.up.to.50cm"    ,
  "location.in.the.profile.of.first.peak..up.to.50cm."   ,
  "The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.50cm"  ,
  "Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.",
  "X0.50" ,
  "ECa_1",                                                               
  "ECa_2",                                                               
  "ECa_3" ,                                                              
   "ECa_4" ,                                                              
   "Krig_Tilla" ,                                                         
  "Krig_D2I"
)

Kooloonong <- Kooloonong %>% rename(
  Peak_Resistance = "max..Peak..resistance.value.up.to.50cm"    , #a
  Depth_to_peak = "location.in.the.profile.of.first.peak..up.to.50cm."   , #b
  Depth_to_2.5MPa ="The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.50cm"  , #c
  Area_under_curve_to_2.5MPa ="Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.", #d
  Area_under_curve_to_50cm ="X0.50", #e
  ECa1 ="ECa_1",
  ECa2 ="ECa_2",
  ECa3 ="ECa_3" ,
  ECa4 ="ECa_4" , 
  
  Tillage  = Krig_Tilla,
  D2I = "Krig_D2I"
  
)
names(Kooloonong)

##################################################################################
### 1a. 
##################################################################################

ECa1VPeak <- ggplot(Kooloonong, aes(x=`ECa1`, y=`Peak_Resistance`)) + 
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
  filename = "ECa1VPeak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 
#################################################################################
##1b. 
#################################################################################
ECa1VDepth_to_peak <- ggplot(Kooloonong, aes(x=`ECa1`, y=`Depth_to_peak`)) + 
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
ECa1VDepth_to_peak
ggsave(
  device = "png",
  filename = "ECa1VDepth_to_peak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 
#################################################################################
##1c. 
#################################################################################

ECa1VDepth_to_2.5MPa <- ggplot(Kooloonong, aes(x=`ECa1`, y=`Depth_to_2.5MPa`)) + 
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
  filename = "ECa1_Depth_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 
#################################################################################
##1d. 
#################################################################################

ECa1VArea_under_curve_to_2.5MPa <- ggplot(Kooloonong, aes(x=`ECa1`, y=`Area_under_curve_to_2.5MPa`)) + 
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
  filename = "ECa1_Area_under_curve_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 
#################################################################################
##1e. 
#################################################################################

ECa1VArea_under_curve_to_50cm <- ggplot(Kooloonong, aes(x=`ECa1`, y=`Area_under_curve_to_50cm`)) + 
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
  filename = "ECa1_Area_under_curve_to_50cm.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 


#################################################################################
#################################################################################
#################################################################################


ECa1VPeak
ECa1VDepth_to_peak
ECa1VDepth_to_2.5MPa
ECa1VArea_under_curve_to_2.5MPa
ECa1VArea_under_curve_to_50cm

################################################################################


################################################################################
### Group the plots


ECa1_plots_Reg <-
  ggarrange(
    ECa1VPeak,
    ECa1VDepth_to_peak,
    ECa1VDepth_to_2.5MPa,
    ECa1VArea_under_curve_to_2.5MPa,
    ECa1VArea_under_curve_to_50cm,
    
    #labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
    ncol = 2,
    nrow = 3
  ) 

ECa1_plots_Reg_1 <-
  annotate_figure(ECa1_plots_Reg, top = text_grob(
    "Topsoil mapper ECa1 vs Penetrometer parameters. 
    Site: Kooloonong",
    color = "Black",
    face = "bold",
    size = 14
  ))
ECa1_plots_Reg_1
ggexport(ECa1_plots_Reg_1, filename = "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/ECa1_plots_Reg.png")




################################################################################
################################################################################
### ECa2
################################################################################
################################################################################




##################################################################################
### 2a. 
##################################################################################

ECa2VPeak <- ggplot(Kooloonong, aes(x=`ECa2`, y=`Peak_Resistance`)) + 
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
  filename = "ECa2VPeak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



################################################################################
##2b. 
#################################################################################

ECa2VDepth_to_peak <- ggplot(Kooloonong, aes(x=`ECa2`, y=`Depth_to_peak`)) + 
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
  filename = "ECa2VDepth_to_peak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##2c. 
#################################################################################

ECa2VDepth_to_2.5MPa <- ggplot(Kooloonong, aes(x=`ECa2`, y=`Depth_to_2.5MPa`)) + 
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
  filename = "ECa2VSDepth_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##2d. 
#################################################################################

ECa2VArea_under_curve_to_2.5MPa <- ggplot(Kooloonong, aes(x=`ECa2`, y=`Area_under_curve_to_2.5MPa`)) + 
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
  filename = "ECa2VsArea_under_curve_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##2e. 
#################################################################################

ECa2VArea_under_curve_to_50cm <- ggplot(Kooloonong, aes(x=`ECa2`, y=`Area_under_curve_to_50cm`)) + 
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
  filename = "ECa2VsArea_under_curve_to_50cm.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

##################################################################################
######################################   ECa2 #################################


ECa2VPeak
ECa2VDepth_to_peak
ECa2VDepth_to_2.5MPa
ECa2VArea_under_curve_to_2.5MPa
ECa2VArea_under_curve_to_50cm

ECa2_plots_Reg <-
  ggarrange(
    ECa2VPeak,
    ECa2VDepth_to_peak,
    ECa2VDepth_to_2.5MPa,
    ECa2VArea_under_curve_to_2.5MPa,
    ECa2VArea_under_curve_to_50cm,
    
    
    #labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
    ncol = 2,
    nrow = 3
  ) 

ECa2_plots_Reg_1 <-
  annotate_figure(ECa2_plots_Reg, top = text_grob(
    "Top soil mapper ECa2 vs Penetrometer parameters. 
    Site: Kooloonong",
    color = "Black",
    face = "bold",
    size = 14
  ))
ECa2_plots_Reg_1
ggexport(ECa2_plots_Reg_1, filename = "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/ECa2_plots_Reg.png")



################################################################################
################################################################################
### ECa3
################################################################################
################################################################################




##################################################################################
### 3a. 
##################################################################################

ECa3VPeak <- ggplot(Kooloonong, aes(x=`ECa3`, y=`Peak_Resistance`)) + 
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
  filename = "ECa3VPeak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



################################################################################
##3b. 
#################################################################################

ECa3VDepth_to_peak <- ggplot(Kooloonong, aes(x=`ECa3`, y=`Depth_to_peak`)) + 
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
  filename = "ECa3VDepth_to_peak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##3c. 
#################################################################################

ECa3VDepth_to_2.5MPa <- ggplot(Kooloonong, aes(x=`ECa3`, y=`Depth_to_2.5MPa`)) + 
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
  filename = "ECa3VSDepth_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##3d. 
#################################################################################

ECa3VArea_under_curve_to_2.5MPa <- ggplot(Kooloonong, aes(x=`ECa3`, y=`Area_under_curve_to_2.5MPa`)) + 
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
  filename = "ECa3VsArea_under_curve_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##3e. 
#################################################################################

ECa3VArea_under_curve_to_50cm <- ggplot(Kooloonong, aes(x=`ECa3`, y=`Area_under_curve_to_50cm`)) + 
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
  filename = "ECa3VsArea_under_curve_to_50cm.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

##################################################################################
######################################   ECa3 #################################


ECa3VPeak
ECa3VDepth_to_peak
ECa3VDepth_to_2.5MPa
ECa3VArea_under_curve_to_2.5MPa
ECa3VArea_under_curve_to_50cm

ECa3_plots_Reg <-
  ggarrange(
    ECa3VPeak,
    ECa3VDepth_to_peak,
    ECa3VDepth_to_2.5MPa,
    ECa3VArea_under_curve_to_2.5MPa,
    ECa3VArea_under_curve_to_50cm,
    
    
    #labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
    ncol = 2,
    nrow = 3
  ) 

ECa3_plots_Reg_1 <-
  annotate_figure(ECa3_plots_Reg, top = text_grob(
    "Top soil mapper ECa3 vs Penetrometer parameters. 
    Site: Kooloonong",
    color = "Black",
    face = "bold",
    size = 14
  ))
ECa3_plots_Reg_1
ggexport(ECa3_plots_Reg_1, filename = "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/ECa3_plots_Reg.png")


################################################################################
################################################################################
### ECa4
################################################################################
################################################################################


names(Kooloonong)

##################################################################################
### 4a. 
##################################################################################

ECa4VPeak <- ggplot(Kooloonong, aes(x=`ECa4`, y=`Peak_Resistance`)) + 
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
  filename = "ECa4VPeak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



################################################################################
##4b. 
#################################################################################

ECa4VDepth_to_peak <- ggplot(Kooloonong, aes(x=`ECa4`, y=`Depth_to_peak`)) + 
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
  filename = "ECa4VDepth_to_peak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##4c. 
#################################################################################

ECa4VDepth_to_2.5MPa <- ggplot(Kooloonong, aes(x=`ECa4`, y=`Depth_to_2.5MPa`)) + 
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
  filename = "ECa4VSDepth_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##4d. 
#################################################################################

ECa4VArea_under_curve_to_2.5MPa <- ggplot(Kooloonong, aes(x=`ECa4`, y=`Area_under_curve_to_2.5MPa`)) + 
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
  filename = "ECa4VsArea_under_curve_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##4e. 
#################################################################################

ECa4VArea_under_curve_to_50cm <- ggplot(Kooloonong, aes(x=`ECa4`, y=`Area_under_curve_to_50cm`)) + 
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
  filename = "ECa4VsArea_under_curve_to_50cm.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

##################################################################################
######################################   ECa4 #################################


ECa4VPeak
ECa4VDepth_to_peak
ECa4VDepth_to_2.5MPa
ECa4VArea_under_curve_to_2.5MPa
ECa4VArea_under_curve_to_50cm

ECa4_plots_Reg <-
  ggarrange(
    ECa4VPeak,
    ECa4VDepth_to_peak,
    ECa4VDepth_to_2.5MPa,
    ECa4VArea_under_curve_to_2.5MPa,
    ECa4VArea_under_curve_to_50cm,
    
    
    #labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
    ncol = 2,
    nrow = 3
  ) 

ECa4_plots_Reg_1 <-
  annotate_figure(ECa4_plots_Reg, top = text_grob(
    "Top soil mapper ECa4 vs Penetrometer parameters. 
    Site: Kooloonong",
    color = "Black",
    face = "bold",
    size = 14
  ))
ECa4_plots_Reg_1
ggexport(ECa4_plots_Reg_1, filename = "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/ECa4_plots_Reg.png")

##################################################################################
### Tillage_a. 
##################################################################################

TillageVPeak <- ggplot(Kooloonong, aes(x=`Tillage`, y=`Peak_Resistance`)) + 
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
  filename = "TillageVPeak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



################################################################################
## Tillage_b. 
#################################################################################

TillageVDepth_to_peak <- ggplot(Kooloonong, aes(x=`Tillage`, y=`Depth_to_peak`)) + 
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
  filename = "TillageVDepth_to_peak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
## Tillage_c. 
#################################################################################

TillageVDepth_to_2.5MPa <- ggplot(Kooloonong, aes(x=`Tillage`, y=`Depth_to_2.5MPa`)) + 
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
  filename = "TillageVSDepth_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
## Tillage_d. 
#################################################################################

TillageVArea_under_curve_to_2.5MPa <- ggplot(Kooloonong, aes(x=`Tillage`, y=`Area_under_curve_to_2.5MPa`)) + 
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
  filename = "TillageVsArea_under_curve_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
## Tillage_e. 
#################################################################################

TillageVArea_under_curve_to_50cm <- ggplot(Kooloonong, aes(x=`Tillage`, y=`Area_under_curve_to_50cm`)) + 
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
  filename = "TillageVsArea_under_curve_to_50cm.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

##################################################################################
######################################   Tillage #################################


TillageVPeak
TillageVDepth_to_peak
TillageVDepth_to_2.5MPa
TillageVArea_under_curve_to_2.5MPa
TillageVArea_under_curve_to_50cm

Tillage_plots_Reg <-
  ggarrange(
    TillageVPeak,
    TillageVDepth_to_peak,
    TillageVDepth_to_2.5MPa,
    TillageVArea_under_curve_to_2.5MPa,
    TillageVArea_under_curve_to_50cm,
    
    
    #labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
    ncol = 2,
    nrow = 3
  ) 

Tillage_plots_Reg_1 <-
  annotate_figure(Tillage_plots_Reg, top = text_grob(
    "Top soil mapper Tillage vs Penetrometer parameters. 
    Site: Kooloonong",
    color = "Black",
    face = "bold",
    size = 14
  ))
Tillage_plots_Reg_1
ggexport(Tillage_plots_Reg_1, filename = "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/Tillage_plots_Reg.png")


##################################################################################
### D2I_a. 
##################################################################################

D2IVPeak <- ggplot(Lameroo, aes(x=`D2I`, y=`Peak_Resistance`)) + 
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
  filename = "D2IVPeak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Wikarie/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



################################################################################
##D2I_b. 
#################################################################################

D2IVDepth_to_peak <- ggplot(Lameroo, aes(x=`D2I`, y=`Depth_to_peak`)) + 
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
  filename = "D2IVDepth_to_peak.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Wikarie/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##D2I_c. 
#################################################################################

D2IVDepth_to_2.5MPa <- ggplot(Lameroo, aes(x=`D2I`, y=`Depth_to_2.5MPa`)) + 
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
  filename = "D2IVSDepth_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Wikarie/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##D2I_d. 
#################################################################################

D2IVArea_under_curve_to_2.5MPa <- ggplot(Lameroo, aes(x=`D2I`, y=`Area_under_curve_to_2.5MPa`)) + 
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
  filename = "D2IVsArea_under_curve_to_2.5MPa.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Wikarie/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

#################################################################################
##D2I_e. 
#################################################################################

D2IVArea_under_curve_to_50cm <- ggplot(Lameroo, aes(x=`D2I`, y=`Area_under_curve_to_50cm`)) + 
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
  filename = "D2IVsArea_under_curve_to_50cm.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Wikarie/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

##################################################################################
######################################   D2I #################################


D2IVPeak
D2IVDepth_to_peak
D2IVDepth_to_2.5MPa
D2IVArea_under_curve_to_2.5MPa
D2IVArea_under_curve_to_50cm

D2I_plots_Reg <-
  ggarrange(
    D2IVPeak,
    D2IVDepth_to_peak,
    D2IVDepth_to_2.5MPa,
    D2IVArea_under_curve_to_2.5MPa,
    D2IVArea_under_curve_to_50cm,
    
    
    #labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
    ncol = 2,
    nrow = 3
  ) 

D2I_plots_Reg_1 <-
  annotate_figure(D2I_plots_Reg, top = text_grob(
    "Top soil mapper D2I vs Penetrometer parameters. 
    Site: Lameroo",
    color = "Black",
    face = "bold",
    size = 14
  ))
D2I_plots_Reg_1
ggexport(D2I_plots_Reg_1, filename = "X:/Therese_Jackie/smart_farms/sites/Wikarie/Analysis/plots_regression/D2I_plots_Reg.png")





