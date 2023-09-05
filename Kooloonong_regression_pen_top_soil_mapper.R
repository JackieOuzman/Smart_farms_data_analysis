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

Kooloonong <- read.csv("X:/Therese_Jackie/smart_farms/sites/Kooloonong/Kooloonong penetrometer readings 2022 for analysis.csv", skip=2)
                                         
names(Kooloonong)

##################################################################################
### edit the dataset - I am just looking at pen readings 
##################################################################################
for_Pen_correlation <- Kooloonong %>% 
  dplyr::select("X0.25":"Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.")

for_Pen_EM_correlation <- Kooloonong %>% 
  dplyr::select("X0.25":"ECa_4"      )
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
  


##################################################################################
### 1a. 
##################################################################################

names(Kooloonong)
Eca1_Max <- ggplot(Kooloonong, aes(x=`ECa_1`, y=`max..Peak..resistance.value.for.the.profile`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Kooloonong EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Shallow)", 
    y = "Max \nresistance"     )
ggsave(
  device = "png",
  filename = "Eca1_Max.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 


Eca2_Max <- ggplot(Kooloonong, aes(x=`ECa_2`, y=`max..Peak..resistance.value.for.the.profile`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Kooloonong EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Shallow)", 
    y = "Max \nresistance"     )

Eca3_Max <- ggplot(Kooloonong, aes(x=`ECa_3`, y=`max..Peak..resistance.value.for.the.profile`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Kooloonong EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Shallow)", 
    y = "Max \nresistance"     )

Eca4_Max <- ggplot(Kooloonong, aes(x=`ECa_4`, y=`max..Peak..resistance.value.for.the.profile`)) + 
  geom_point(alpha =0.5, size=0.3)+
  theme_bw()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Kooloonong EM vs penetrometer ",
    #subtitle = "",
    x = "", 
    #x = "EM mS/m (depth Shallow)", 
    y = "Max \nresistance"     )

Eca1_Max
Eca2_Max
Eca3_Max
Eca4_Max
################################################################################
### Group the plots






Eca_plots_Reg <-
  ggarrange(
    Eca1_Max,
    Eca2_Max,
    Eca3_Max,
    Eca4_Max,
    
    
    #labels = c("ECa 1", "ECa 2", "ECa 3", "ECa 4"),
    ncol = 2,
    nrow = 2
  ) 

Eca_plots_Reg_Reg_1 <-
  annotate_figure(Eca_plots_Reg, top = text_grob(
    "Top soil mapper vs Max Penetrometer. 
    Site: Kooloonong",
    color = "Black",
    face = "bold",
    size = 14
  ))
Eca_plots_Reg_Reg_1
ggexport(Eca_plots_Reg_Reg_1, filename = "X:/Therese_Jackie/smart_farms/sites/Kooloonong/Analysis/plots_regression/EM_Shallow_plots_Reg.png")

##################################################################################
######################################   EM DEEP #################################

### Most of the EM deep readings are negative I wont use them
#