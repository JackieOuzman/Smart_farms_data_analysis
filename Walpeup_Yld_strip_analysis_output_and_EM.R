
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
         Trial_Pts_ID = paste0(TrialID,"_",PointID))

Walpeup_2021_east_1 <- read.csv("X:/Therese_Jackie/smart_farms/sites/Walpeup/strip_analysis_YLD_EM/Trial-1_E-W-strip_yld2021_eastern_noRip.csv") %>% 
  mutate(year = 2021,
         strip_name = "east",
         Trial_Pts_ID = paste0(TrialID,"_",PointID))

########################################################
Walpeup_2022_West_0 <- read.csv("X:/Therese_Jackie/smart_farms/sites/Walpeup/strip_analysis_YLD_EM/Trial-0_E-W-strip_yld2022_western_noRip.csv") %>% 
  mutate(year = 2022,
         strip_name = "west",
         Trial_Pts_ID = paste0(TrialID,"_",PointID))

Walpeup_2022_east_1 <- read.csv("X:/Therese_Jackie/smart_farms/sites/Walpeup/strip_analysis_YLD_EM/Trial-1_E-W-strip_yld2022_eastern_noRip.csv") %>% 
  mutate(year = 2022,
         strip_name = "east",
         Trial_Pts_ID = paste0(TrialID,"_",PointID))

########################################################
Walpeup_2023_West_0 <- read.csv("X:/Therese_Jackie/smart_farms/sites/Walpeup/strip_analysis_YLD_EM/Trial-0_E-W-strip_yld2023_western_noRip.csv") %>% 
  mutate(year = 2023,
         strip_name = "west",
         Trial_Pts_ID = paste0(TrialID,"_",PointID))

Walpeup_2023_east_1 <- read.csv("X:/Therese_Jackie/smart_farms/sites/Walpeup/strip_analysis_YLD_EM/Trial-1_E-W-strip_yld2023_eastern_noRip.csv") %>% 
  mutate(year = 2023,
         strip_name = "east",
         Trial_Pts_ID = paste0(TrialID,"_",PointID))


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

names(EM_west_east)

EM_west_east <- EM_west_east %>% 
  select(Trial_Pts_ID, EM_1m_PRED, EM_05m_PRE, DistOnLine)


EM_west_east_summaries <- EM_west_east %>% 
  group_by(Trial_Pts_ID) %>% 
  summarise(EM_05_mean = mean(EM_05m_PRE),
            EM_1_mean = mean(EM_1m_PRED))
EM_west_east_summaries <- ungroup(EM_west_east_summaries)

EM_strip <- EM %>% filter(Strip_Name == "Strip" )
names(EM_strip)
EM_strip <- EM_strip %>% select(Trial_Pts_ID, DistOnLine, POINT_X, POINT_Y,  
                                EM_1m_PRED, EM_05m_PRE)

EM_Walpeup <- left_join(EM_strip,EM_west_east_summaries)

names(EM_Walpeup)
EM_Walpeup <- EM_Walpeup %>% 
  rename(POINT_X_strip = POINT_X,
         POINT_Y_strip = POINT_Y,
         EM_1m_Strip = EM_1m_PRED,
         EM_05m_Strip = EM_05m_PRE,
         EM_1m_Mean_nonStrip = EM_1_mean,
         EM_05m_Mean_nonStrip = EM_05_mean
         )


rm(EM, EM_strip, EM_west_east, EM_west_east_summaries)



names(EM_Walpeup)

#get one clm for EM1m
EM_Walpeup_longer_1m <- EM_Walpeup %>% select(Trial_Pts_ID, DistOnLine,POINT_X_strip, POINT_Y_strip, 
                                              EM_1m_Strip,
                                              EM_1m_Mean_nonStrip)

EM_Walpeup_longer_1m <- EM_Walpeup_longer_1m %>% rename(no_rip = EM_1m_Strip,
                                                        rip_mean =EM_1m_Mean_nonStrip)


str(EM_Walpeup_longer_1m)

EM_Walpeup_longer_1m_longer <- EM_Walpeup_longer_1m %>% pivot_longer(
  cols =  no_rip:rip_mean          ,
  names_to = "strip",
  values_to = "EM_1m"
)


#######
names(EM_Walpeup)
EM_Walpeup_longer_05m <- EM_Walpeup %>% select(Trial_Pts_ID, DistOnLine,POINT_X_strip, POINT_Y_strip, 
                                               EM_05m_Strip,
                                               EM_05m_Mean_nonStrip)

EM_Walpeup_longer_05m <- EM_Walpeup_longer_05m %>% rename(no_rip = EM_05m_Strip,
                                                        rip_mean =EM_05m_Mean_nonStrip)


str(EM_Walpeup_longer_05m)

EM_Walpeup_longer_05m <- EM_Walpeup_longer_05m %>% pivot_longer(
  cols =  no_rip:rip_mean          ,
  names_to = "strip",
  values_to = "EM_05m"
)

#################

str(EM_Walpeup_longer_05m)
str(EM_Walpeup_longer_1m_longer)

EM_Walpeup_long <- left_join(EM_Walpeup_longer_05m, EM_Walpeup_longer_1m_longer)

########################
rm(EM_Walpeup, EM_Walpeup_longer_05m, EM_Walpeup_longer_1m)


##############################################################################
str(Walpeup_yld_strips_longer)
str(EM_Walpeup_long)


Walpeup_yld_EM_strips <- full_join(Walpeup_yld_strips_longer, EM_Walpeup_long)

names(Walpeup_yld_EM_strips)



rm(list=ls()[ls()!= "Walpeup_yld_EM_strips"])


###############################################################################
##1. is there a yield response for each year to ripping

str(Walpeup_yld_EM_strips)



summary_yld_for_strip <- Walpeup_yld_EM_strips %>% 
  group_by(year, strip_name,strip ) %>% 
  summarise(av_yld =mean(yield, na.rm = TRUE))
summary_yld_for_strip <- ungroup(summary_yld_for_strip)

str(summary_yld_for_strip)

Walpeup_yld_EM_strips$strip <- factor(Walpeup_yld_EM_strips$strip, 
                                      levels = c(no_rip, rip_mean))




ggplot(data = summary_yld_for_strip, aes(x= as.factor(year), y= av_yld, fill = strip))+
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap(.~strip_name)+
  theme_bw()+
  labs(title="Average Yield response for strip \n per year",
       subtitle = "Average yield for no rip and average yield resposne for either side of no rip strip", 
       x ="Year", y = "Yield t/ha")+ 
  theme(legend.position="bottom")+
  scale_fill_manual(name = "", labels = c("No ripping", "Ripping"), values=c('lightgrey','black'))




################################################################################
#2. Is there a yield response to ripping based on soil type
## In am not sure if the EM has not been calibrated by takening soil sampling so a low EM value could mean a number of things.
## Here I will assume low EM value (mS/m) is a sand
##add a class for sand



str(Walpeup_yld_EM_strips)
Walpeup_yld_EM_strips <- Walpeup_yld_EM_strips %>% 
  mutate(
    Sand_surface = case_when(
      EM_05m < 15 ~ "Sandy at surface",
      EM_05m > 24 ~ "Not sandy at surface"))

Walpeup_yld_EM_strips <- Walpeup_yld_EM_strips %>% 
  mutate(
    Sand_deeper = case_when(
      EM_1m < 60 ~ "Sandy deeper",
      EM_1m > 65.5 ~ "Not sandy deeper"))


Summary_Soil_type_surface_Walpeup_yld_EM_strips <- Walpeup_yld_EM_strips %>% 
  group_by(year, strip_name,strip, Sand_surface ) %>% 
  summarise(av_yld =mean(yield, na.rm = TRUE))
Summary_Soil_type_surface_Walpeup_yld_EM_strips <- ungroup(Summary_Soil_type_surface_Walpeup_yld_EM_strips)

str(Summary_Soil_type_surface_Walpeup_yld_EM_strips)


#### PLOT surface
Summary_Soil_type_surface_Walpeup_yld_EM_strips %>% 
  filter(!is.na(Sand_surface)) %>% 
ggplot(aes(x= as.factor(year), y= av_yld, fill = strip))+
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap(Sand_surface~strip_name)+
  theme_bw()+
  labs(title="Average Yield response for strip \n per year",
       subtitle = "Average yield for no rip and average yield resposne for either side of no rip strip", 
       x ="Year", y = "Yield t/ha")+ 
  theme(legend.position="bottom")+
  scale_fill_manual(name = "", labels = c("No ripping", "Ripping"), values=c('lightgrey','black'))

  

###############################################################################
str(Walpeup_yld_EM_strips)

Summary_Soil_type_deeper_Walpeup_yld_EM_strips <- Walpeup_yld_EM_strips %>% 
  group_by(year, strip_name,strip, Sand_deeper ) %>% 
  summarise(av_yld =mean(yield, na.rm = TRUE))
Summary_Soil_type_deeper_Walpeup_yld_EM_strips <- ungroup(Summary_Soil_type_deeper_Walpeup_yld_EM_strips)



#### PLOT deeper
str(Summary_Soil_type_deeper_Walpeup_yld_EM_strips)

Summary_Soil_type_deeper_Walpeup_yld_EM_strips %>% 
  filter(!is.na(Sand_deeper)) %>% 
  ggplot(aes(x= as.factor(year), y= av_yld, fill = strip))+
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap(Sand_deeper~strip_name)+
  theme_bw()+
  labs(title="Average Yield response for strip \n per year",
       subtitle = "Average yield for no rip and average yield resposne for either side of no rip strip", 
       x ="Year", y = "Yield t/ha")+ 
  theme(legend.position="bottom")+
  scale_fill_manual(name = "", labels = c("No ripping", "Ripping"), values=c('lightgrey','black'))



#########################################################################################

#Yield gain with EM value

str(Walpeup_yld_EM_strips)
## make it wider...WEST STRIP - shallow

Walpeup_yld_EM_strips_West_wide <- Walpeup_yld_EM_strips %>% 
  filter(strip_name == "west") %>% 
  pivot_wider(names_from = strip,
              values_from = c(yield, EM_05m, EM_1m)
  )

str(Walpeup_yld_EM_strips_West_wide)

Walpeup_yld_EM_strips_West_wide <- Walpeup_yld_EM_strips_West_wide %>% 
  mutate(Yld_diff = yield_rip_mean - yield_no_rip) %>% 
  select(-yield_no_rip, -yield_rip_mean)
str(Walpeup_yld_EM_strips_West_wide)

Walpeup_yld_EM_strips_West_wide %>% 
  filter(EM_05m_rip_mean>1) %>% 
  ggplot(aes(x= EM_05m_rip_mean , y=Yld_diff, colour = Sand_surface))+
  geom_point()+
  facet_wrap(.~year)+
  labs(title="Yield difference ripping - non ripping \n No rip strip on the western side of block",
       subtitle = "Yield for no rip and average yield resposne for either side of no rip strip", 
       x ="EM shallow", y = "Yield diference (t/ha)",
       caption = "Shallow EM value used to define sand; <15 = sandy, >24 = not sandy" )+ 
  theme_bw()+
  theme(legend.position="bottom")


## make it wider...EAST STRIP - shallow

Walpeup_yld_EM_strips_east_wide <- Walpeup_yld_EM_strips %>% 
  filter(strip_name == "east") %>% 
  pivot_wider(names_from = strip,
              values_from = c(yield, EM_05m, EM_1m)
  )

str(Walpeup_yld_EM_strips_east_wide)

Walpeup_yld_EM_strips_east_wide <- Walpeup_yld_EM_strips_east_wide %>% 
  mutate(Yld_diff = yield_rip_mean - yield_no_rip) %>% 
  select(-yield_no_rip, -yield_rip_mean)
str(Walpeup_yld_EM_strips_east_wide)

Walpeup_yld_EM_strips_east_wide %>% 
  filter(EM_05m_rip_mean>1) %>% 
  ggplot(aes(x= EM_05m_rip_mean , y=Yld_diff, colour = Sand_surface))+
  geom_point()+
  facet_wrap(.~year)+
  labs(title="Yield difference ripping - non ripping \n No rip strip on the eastern side of block",
       subtitle = "Yield for no rip and average yield resposne for either side of no rip strip", 
       x ="EM shallow", y = "Yield diference (t/ha)",
       caption = "Shallow EM value used to define sand; <15 = sandy, >24 = not sandy" )+ 
  theme_bw()+
  theme(legend.position="bottom")



#########################################################################################

##########################################################################################

#Yield gain with EM value

str(Walpeup_yld_EM_strips)
## make it wider...WEST STRIP - deep

Walpeup_yld_EM_strips_West_wide <- Walpeup_yld_EM_strips %>% 
  filter(strip_name == "west") %>% 
  pivot_wider(names_from = strip,
              values_from = c(yield, EM_05m, EM_1m)
  )

str(Walpeup_yld_EM_strips_West_wide)

Walpeup_yld_EM_strips_West_wide <- Walpeup_yld_EM_strips_West_wide %>% 
  mutate(Yld_diff = yield_rip_mean - yield_no_rip) %>% 
  select(-yield_no_rip, -yield_rip_mean)
str(Walpeup_yld_EM_strips_West_wide)

Walpeup_yld_EM_strips_West_wide %>% 
  filter(EM_1m_rip_mean>1) %>% 
  ggplot(aes(x= EM_1m_rip_mean , y=Yld_diff, colour = Sand_deeper))+
  geom_point()+
  facet_wrap(.~year)+
  labs(title="Yield difference ripping - non ripping \n No rip strip on the western side of block",
       subtitle = "Yield for no rip and average yield resposne for either side of no rip strip", 
       x ="EM Deep", y = "Yield diference (t/ha)",
       caption = "Deep EM value used to define sand; <605 = sandy, >65.5 = not sandy" )+ 
  theme_bw()+
  theme(legend.position="bottom")


## make it wider...EAST STRIP - deep

Walpeup_yld_EM_strips_east_wide <- Walpeup_yld_EM_strips %>% 
  filter(strip_name == "east") %>% 
  pivot_wider(names_from = strip,
              values_from = c(yield, EM_05m, EM_1m)
  )

str(Walpeup_yld_EM_strips_east_wide)

Walpeup_yld_EM_strips_east_wide <- Walpeup_yld_EM_strips_east_wide %>% 
  mutate(Yld_diff = yield_rip_mean - yield_no_rip) %>% 
  select(-yield_no_rip, -yield_rip_mean)
str(Walpeup_yld_EM_strips_east_wide)

Walpeup_yld_EM_strips_east_wide %>% 
  filter(EM_1m_rip_mean>1) %>% 
  ggplot(aes(x= EM_1m_rip_mean , y=Yld_diff, colour = Sand_deeper    ))+
  geom_point()+
  facet_wrap(.~year)+
  labs(title="Yield difference ripping - non ripping \n No rip strip on the eastern side of block",
       subtitle = "Yield for no rip and average yield resposne for either side of no rip strip", 
       x ="EM Deep", y = "Yield diference (t/ha)",
       caption = "Deep EM value used to define sand; <60 = sandy, >65.5 = not sandy" )+ 
  theme_bw()+
  theme(legend.position="bottom")
