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
  dplyr::select("X0.25":"Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.72.5cm.") 


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






#################################################################################
### Plots
##################################################################################
names(Coomandook)
#Lets try ... based on the correlation matrix - not that its very good!
  


##################################################################################
### 1a. 
##################################################################################

names(Coomandook)
EMShallow_Mean <- ggplot(Coomandook, aes(x=`EM05m`, y=`mean.resistance.value.for.the.profile`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Coomandook EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Shallow)", 
    y = "Mean \nresistance"     )
ggsave(
  device = "png",
  filename = "EMShallow_Mean.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 


EMShallow_Area_profile <- ggplot(Coomandook, aes(x=`EM05m`, y=`total`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Coomandook EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Shallow)", 
    y = "Area \nprofile"     )

EMShallow_Area_0_50 <- ggplot(Coomandook, aes(x=`EM05m`, y=`X0.50`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Coomandook EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Shallow)", 
    y = "Area \n0-50cm"     )

EMShallow_Depth_peak <- ggplot(Coomandook, aes(x=`EM05m`, y=`location.in.the.profile.of.first.peak..up.to.50cm.`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Coomandook EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Shallow)", 
    y = "Depth to peak \nup to 50cm"     )

EMShallow_Depth_exceeds <- ggplot(Coomandook, aes(x=`EM05m`, y=`The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.72.5cm`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Coomandook EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Shallow)", 
    y = "Depth exceeds \n2.5MPa"     )

EMShallow_Mean
EMShallow_Area_profile
EMShallow_Area_0_50
EMShallow_Depth_peak
EMShallow_Depth_exceeds
################################################################################
### Group the plots






Eca_plots_Reg <-
  ggarrange(
    EMShallow_Mean,
    EMShallow_Area_profile,
    EMShallow_Area_0_50,
    EMShallow_Depth_peak,
    EMShallow_Depth_exceeds,
    
    
    #labels = c("ECa 1", "ECa 2", "ECa 3", "ECa 4"),
    ncol = 2,
    nrow = 3
  ) 

Eca_plots_Reg_Reg_1 <-
  annotate_figure(Eca_plots_Reg, top = text_grob(
    "EM vs Penetrometer. 
    Site: Coomandook",
    color = "Black",
    face = "bold",
    size = 14
  ))
Eca_plots_Reg_Reg_1
ggexport(Eca_plots_Reg_Reg_1, filename = "X:/Therese_Jackie/smart_farms/sites/Coomandook/Analysis/plots_regression/EM_Shallow_plots_Reg.png")

