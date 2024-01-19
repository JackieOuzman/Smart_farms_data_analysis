## MERGE THE REGRESSION DATA FOR ALL SITES



Coomandook <- read.csv("X:/Therese_Jackie/smart_farms/sites/Coomandook/Coomandook_pen_vs_em_2023.csv", skip=2)

################################################################################
Coomandook <- Coomandook %>% rename(
  Peak_Resistance = "max..Peak..resistance.value.up.to.50cm"    , #a
  Depth_to_peak = "location.in.the.profile.of.first.peak..up.to.50cm."   , #b 
  Depth_to_2.5MPa ="The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.50cm"  , #c
  Area_under_curve_to_2.5MPa ="Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.", #d
  Area_under_curve_to_50cm ="X0.50" #e
)

Coomandook <- Coomandook %>% mutate(site = "Coomandook")
################################################################################

Boordaroo <- read.csv("X:/Therese_Jackie/smart_farms/sites/Boordaroo/Boordaroo_pen_2023_for_analysis.csv", skip=2)

Boordaroo <- Boordaroo %>% rename(
  Peak_Resistance = "max..Peak..resistance.value.up.to.50cm"    , #a
  Depth_to_peak = "location.in.the.profile.of.first.peak..up.to.50cm."   , #b 
  Depth_to_2.5MPa ="The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.50cm"  , #c
  Area_under_curve_to_2.5MPa ="Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.", #d
  Area_under_curve_to_50cm ="X0.50" #e
)

Boordaroo <- Boordaroo %>% mutate(site = "Boordaroo")
################################################################################


merge_step1 <- rbind(Boordaroo,Coomandook )
