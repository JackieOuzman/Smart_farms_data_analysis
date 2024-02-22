
library(tidyverse)
library(readxl)
library(dplyr)
#install.packages("PerformanceAnalytics")

library(ggplot2)
library(plotly)

library(stringr)
library(ggpubr)

#################################################################################
### import dataset  
##################################################################################

# "X:\Therese_Jackie\smart_farms\sites\Walpeup\strip_analysis_YLD_EM\Trial-0_E-W-strip_yld2021_western_noRip.csv"
# "X:\Therese_Jackie\smart_farms\sites\Walpeup\strip_analysis_YLD_EM\Trial-1_E-W-strip_yld2021_eastern_noRip.csv"

# "X:\Therese_Jackie\smart_farms\sites\Walpeup\strip_analysis_YLD_EM\Trial-0_E-W-strip_yld2022_western_noRip.csv"
# "X:\Therese_Jackie\smart_farms\sites\Walpeup\strip_analysis_YLD_EM\Trial-0_E-W-strip_yld2023_western_noRip.csv"

# "X:\Therese_Jackie\smart_farms\sites\Walpeup\strip_analysis_YLD_EM\Trial-1_E-W-strip_yld2022_eastern_noRip.csv"
# "X:\Therese_Jackie\smart_farms\sites\Walpeup\strip_analysis_YLD_EM\Trial-1_E-W-strip_yld2023_eastern_noRip.csv"

Walpeup_2021_West_0 <- read.csv("X:/Therese_Jackie/smart_farms/sites/Walpeup/strip_analysis_YLD_EM/Trial-0_E-W-strip_yld2021_western_noRip.csv") %>% 
  mutate(year = 2021,
         strip_name = "west",
         Trial_Pts_ID = paste0(TrialID,"_",PointID, "_",strip_name))

Walpeup_2021_east_1 <- read.csv("X:/Therese_Jackie/smart_farms/sites/Walpeup/strip_analysis_YLD_EM/Trial-1_E-W-strip_yld2021_eastern_noRip.csv") %>% 
  mutate(year = 2021,
         strip_name = "east",
         Trial_Pts_ID = paste0(TrialID,"_",PointID, "_",strip_name))

########################################################
Walpeup_2022_West_0 <- read.csv("X:/Therese_Jackie/smart_farms/sites/Walpeup/strip_analysis_YLD_EM/Trial-0_E-W-strip_yld2022_western_noRip.csv") %>% 
  mutate(year = 2022,
         strip_name = "west",
         Trial_Pts_ID = paste0(TrialID,"_",PointID, "_",strip_name))

Walpeup_2022_east_1 <- read.csv("X:/Therese_Jackie/smart_farms/sites/Walpeup/strip_analysis_YLD_EM/Trial-1_E-W-strip_yld2022_eastern_noRip.csv") %>% 
  mutate(year = 2022,
         strip_name = "east",
         Trial_Pts_ID = paste0(TrialID,"_",PointID, "_",strip_name))

########################################################
Walpeup_2023_West_0 <- read.csv("X:/Therese_Jackie/smart_farms/sites/Walpeup/strip_analysis_YLD_EM/Trial-0_E-W-strip_yld2023_western_noRip.csv") %>% 
  mutate(year = 2023,
         strip_name = "west",
         Trial_Pts_ID = paste0(TrialID,"_",PointID, "_",strip_name))

Walpeup_2023_east_1 <- read.csv("X:/Therese_Jackie/smart_farms/sites/Walpeup/strip_analysis_YLD_EM/Trial-1_E-W-strip_yld2023_eastern_noRip.csv") %>% 
  mutate(year = 2023,
         strip_name = "east",
         Trial_Pts_ID = paste0(TrialID,"_",PointID, "_",strip_name))


################################################################################
Walpeup_yld_strips <- rbind(Walpeup_2021_West_0,
                            Walpeup_2021_east_1,
                            Walpeup_2022_West_0,
                            Walpeup_2022_east_1,
                            Walpeup_2023_West_0,
                            Walpeup_2023_east_1)

names(Walpeup_yld_strips)
#drop 

#Walpeup_yld_strips <- Walpeup_yld_strips %>% select(-treat_diff, -av_treat_dif, -p_value, -RI, TrialID,  - PointID)


Walpeup_yld_strips <- Walpeup_yld_strips %>% select(
  Trial_Pts_ID, 
  year,
  strip_name,
  Strip.Value ,
  E.W_mean
  )

Walpeup_yld_strips <- Walpeup_yld_strips %>% rename(
  no_rip = Strip.Value,
  rip_mean = E.W_mean
)


#get one clm for yield
Walpeup_yld_strips_longer <- Walpeup_yld_strips %>% pivot_longer(
  cols = no_rip:rip_mean,
  names_to = "strip",
  values_to = "yield"
)

rm(list=ls()[ls()!= "Walpeup_yld_strips_longer"])

###############################################################################
######## EM data with pt coordinates
EM <- read.csv("X:/Therese_Jackie/smart_farms/sites/Walpeup/strip_analysis_YLD_EM/strip_analysis_pts_withEM.csv") %>% 
mutate(Trial_Pts_ID = paste0(TrialID,"_",PointID))

#### I need one value for the em either side of the strip.
## EM values on each side of the strip
names(EM)
unique(EM$Strip_Name)
EM_west_east <- EM %>% filter(Strip_Name == "E Strip" | Strip_Name == "W Strip")

EM_west_east_summaries <- EM_west_east %>% 
  group_by(Trial_Pts_ID) %>% 
  summarise(EM_05_mean = mean(EM_05m_PRE),
            EM_1_mean = mean(EM_1m_PRED))
EM_west_east_summaries <- ungroup(EM_west_east_summaries)

EM_strip <- EM %>% filter(Strip_Name == "Strip" )

EM_Walp <- left_join(EM_strip,EM_west_east_summaries)


#### up to here ####




names(EM)

Walpeup_EM <- EM %>% select(
  Trial_Pts_ID, 
  POINT_X,
  POINT_Y,
  EM_1m_PRED,
  EM_05m_PRE 
)


##############################################################################
str(Walpeup_yld_strips_longer)
str(Walpeup_EM)


Walpeup_yld_EM_strips <- full_join(Walpeup_yld_strips_longer, Walpeup_EM)

names(Walpeup_yld_EM_strips)



rm(list=ls()[ls()!= "Walpeup_yld_EM_strips"])


###############################################################################
##1. is there a yield response for each year to ripping

names(Walpeup_yld_EM_strips)


summary_yld_for_strip <- Walpeup_yld_EM_strips %>% 
  group_by(year, strip_name, )