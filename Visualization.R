#Packages I'm using
library("plyr")
library("ggplot2")
library(tidyverse)
library(urbnmapr)
require(caTools)

#Datasets Import
serious_violations <- read.csv("C:\\Users\\USER\\Documents\\College\\F19\\Data_Analytics\\Assignments\\Final project\\Datasets\\SDWA_downloads\\SDWA_SERIOUS_VIOLATORS.csv")
water_systems <- read.csv("C:\\Users\\USER\\Documents\\College\\F19\\Data_Analytics\\Assignments\\Final project\\Datasets\\SDWA_downloads\\SDWA_PUB_WATER_SYSTEMS.csv")

#Clean water systems
water_systems_clean <- water_systems[which(water_systems$IS_TRIBAL != "Y"),]
#Merge water systems and serious violations
water_serious_merge <- merge(water_systems_clean,serious_violations,by = c("PWSID","FISCAL_YEAR"))

#count states with serious violations
state_count <- plyr::count(water_serious_merge, vars = "STATE.x" )

#Run this to get value of final_state_count
test_count <- state_count
test_count1 <- state_count
test_count2 <- test_count[!(test_count$STATE.x %in% state.abb),]
test_count1[!(test_count1$STATE.x %in% test_count2$STATE.x),]
join(test_count,test_count1, by = "STATE.x" )
test_count1
test_count2$STATE.x
test_count3 <- test_count1[!(test_count1$STATE.x %in% test_count2$STATE.x),]
str(test_count3)
final_state_count <- test_count3
rm(test_count)

#Plotting serious violations using urbanmapr
names(final_state_count)[names(final_state_count) == "STATE.x"] <- "state_abbv"
spatial_data <- left_join(get_urbn_map(map = "states", sf = TRUE),
                          final_state_count,
                          by = "state_abbv")
print(final_state_count)
ggplot() +
  geom_sf(spatial_data,
          mapping = aes(fill = freq),
          size = 0.5, color = "#ffffff") +
  labs(title = "Serious violations between
       2011 and 2019", fill =" ") + scale_fill_gradient(low = "#ffd1d1", high = "#ff0000")+ theme(legend.position="bottom",
                                                                                       legend.text = element_text(size = 5))