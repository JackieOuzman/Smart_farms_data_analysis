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

Charlies <- read.csv("X:/Therese_Jackie/smart_farms/sites/Bute_Charlie/Bute_Site_Charlie_EM_PEN_for _analysis.csv", skip=2)
                    
names(Charlies)

##################################################################################
### edit the dataset - I am just looking at pen readings 
##################################################################################
for_Pen_correlation <- Charlies %>% 
  dplyr::select("X0.25":"Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.75cm.")

for_Pen_EM_correlation <- Charlies %>% 
  dplyr::select("X0.25":"Deep", -"X" , -"Sam_ID", -"If_Inside_rip_area"     )
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
          file = "X:/Therese_Jackie/smart_farms/sites/Bute_Charlie/Analysis/correltion_matrix_pen_em_flat.csv",
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
#1. "mean.resistance.value.for.the.profile" vs "Shallow"  
#2. "total" vs "Shallow"  
#3. "X0.50" vs "shallow"  
   



#1. "mean.resistance.value.for.the.profile" vs "Deep"  
#2. "X0.50" vs "Deep"  


##################################################################################
### 1a. 
##################################################################################

EMShallowVMean <- ggplot(Charlies, aes(x=`Shallow`, y=`mean.resistance.value.for.the.profile`)) + 
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(#title = "Charlies EM vs penetrometer ",
               #subtitle = "",
               #x = "EM mS/m (Shallow)", 
               x = "",
               y = "Mean \nresistance"     )
ggsave(
  device = "png",
  filename = "EMShallowVMean.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Bute_Charlie/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



#https://link.springer.com/article/10.1023/b:prag.0000040807.18932.80
#Sandy soils less than 10 - but this is not calibrated

EMShallowVMean_sand <-Charlies %>% filter(Shallow < 10) %>% 
  ggplot(aes(x=`Shallow`, y=`mean.resistance.value.for.the.profile`)) + 
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(#title = "Charlies EM vs penetrometer - Sand only",
       #subtitle = "",
       x = "", 
       #x = "EM mS/m (depth Shallow)", 
       y = "Mean \nresistance"
  )+
  theme(panel.background = element_rect(fill = "#BFD5E3"))





ggsave(
  device = "png",
  filename = "EMShallowVMean_sand_lowEM_Values.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Bute_Charlie/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



##################################################################################
### 2a. "total" vs "Shallow" 
##################################################################################
EM_Shallow_total <- ggplot(Charlies, aes(x=`Shallow`, y=`total`)) + 
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(#title = "Charlies EM vs penetrometer ",
       #subtitle = "",
       x = "",
       #x = "EM mS/m (depth Shallow)",
       y = "Max \nresistance"     )
ggsave(
  device = "png",
  filename = "EM_Shallow_total.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Bute_Charlie/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



################################################################################
### 2b. "total" vs "Shallow"  SAND


EMShallow_total_sand <-Charlies %>% filter(Shallow < 10) %>% 
  ggplot( aes(x=`Shallow`, y=`total`)) + 
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(#title = "Charlies EM vs penetrometer sand ",
       #subtitle = "",
       x = "",
       #x = "EM mS/m (depth Shallow)",
       y = "Max \nresistance"     )+
  theme(panel.background = element_rect(fill = "#BFD5E3"))

ggsave(
  device = "png",
  filename = "EMShallow_total_sand.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Bute_Charlie/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



##################################################################################
### #3a. "X0.50" vs "Shallow"  
##################################################################################

names(Charlies)
EMShallow_Area0_50 <- ggplot(Charlies, aes(x=`Shallow`, y=`X0.50`)) + 
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  
  labs(#title = "Charlies EM vs penetrometer ",
       #subtitle = "",
       x = "", 
       #x = "EM mS/m (depth Shallow)", 
       y = "Depth of peak \nresistance"     )
ggsave(
  device = "png",
  filename = "EMShallow_Area0_50.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Bute_Charlie/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 



################################################################################
### #3b. " "X0.50" vs "Shallow"  "    SAND


EMShallow_Area0_50_sand <-Charlies %>% filter(Shallow < 10) %>% 
  ggplot( aes(x=`Shallow`, y=`X0.50`)) + 
  geom_point()+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  labs(#title = "Charlies EM vs penetrometer sand ",
       #subtitle = "",
       #x = "EM mS/m (depth Shallow)", 
       x = "", 
       y = "Depth of peak \nresistance"     )+
  theme(panel.background = element_rect(fill = "#BFD5E3"))

ggsave(
  device = "png",
  filename = "EMShallow_Area0_50_sand.png",
  path= "X:/Therese_Jackie/smart_farms/sites/Bute_Charlie/Analysis/plots_regression/",
  width=8.62,
  height = 6.28,
  dpi=600
) 

EMShallowVMean
EMShallowVMean_sand
EM_Shallow_total
EMShallow_total_sand
EMShallow_Area0_50
EMShallow_Area0_50_sand

################################################################################
### Group the plots

#1
EMShallowVMean
EMShallowVMean_sand

#2
EM_Shallow_total
EMShallow_total_sand

#3
EMShallow_Area0_50
EMShallow_Area0_50_sand



EMShallow_plots_Reg <-
  ggarrange(
    EMShallowVMean,
    EMShallowVMean_sand,
    EM_Shallow_total,
    EMShallow_total_sand,
    EMShallow_Area0_50,
    EMShallow_Area0_50_sand,
    
    
    #labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
    ncol = 2,
    nrow = 3
  ) 

EMShallow_plots_Reg_1 <-
  annotate_figure(EMShallow_plots_Reg, top = text_grob(
    "EM38 Shallow vs Penetrometer parameters. 
    Site: Bute Charlie",
    color = "Black",
    face = "bold",
    size = 14
  ))
EMShallow_plots_Reg_1
ggexport(EMShallow_plots_Reg_1, filename = "X:/Therese_Jackie/smart_farms/sites/Bute_Charlie/Analysis/plots_regression/EM_Shallow_plots_Reg.png")
