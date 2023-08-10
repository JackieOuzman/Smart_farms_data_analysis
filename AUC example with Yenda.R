
#install.packages("MESS")
library(MESS)
library(tidyverse)
library(readxl)
library(dplyr)

################################################################################
### example https://stackoverflow.com/questions/69424572/how-to-calculate-the-auc-of-a-graph-in-r ###
################################################################################
x <- seq(0,3, by=0.1)
y <- x^2
auc(x,y, from = 0.1, to = 2, type = "spline")
plot(x,y)

################################################################################
###Yenda data
################################################################################

Field_PR_data_Yenda <- read_excel("X:/Therese_Jackie/smart_farms/Rodrigos files Area Under Curve/Field_PR data_Yenda.xlsx", 
                                  sheet = "test data to import into R")
plot(Field_PR_data_Yenda$Native, Field_PR_data_Yenda$depth)
x1 <- Field_PR_data_Yenda$Native
y1 <- Field_PR_data_Yenda$depth

################################################################################
###filter to only keep depth between 0-25
################################################################################
Yenda_0to25 <-  Field_PR_data_Yenda   %>% 
  filter(depth <=25)
str(Yenda_0to25)
Yenda_0to25$depth <- as.double(Yenda_0to25$depth)


##  plot ###
ggplot(Yenda_0to25, aes(x=Native, 
                        depth
                        )) + 
  
  geom_path(linejoin = "round")+
  scale_y_reverse()+
  xlim(0, 6)+
  ylim(75, 0) 


### calculate the AUC using spline or linear ###
auc(Yenda_0to25$Native, Yenda_0to25$depth, 
     type = "spline")

auc(Yenda_0to25$Native, Yenda_0to25$depth, 
    type = "linear")

### not much in it either 31 using spline or 30 using linear
