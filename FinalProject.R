
# Final Project -----------------------------------------------------------
#Kate Broeksmit
install.packages(c("dplyr", "lubridate"))
install.packages("ggplot2")
install.packages("geojsonsf")
install.packages("sf")
library(dplyr)
library(lubridate)
library(ggplot2)
library(geojsonsf)
#load Tigris and patchwork Package to help with mapping census data
install.packages(c("tigris", "patchwork", "tidyverse"))
library(tigris)
library(patchwork)
library(tidyverse)
library(tidycensus)
setwd("/Users/katebroeksmit/Desktop/Final-Project")

install.packages("tidycensus")

census_api_key("9fc7a703387fd39bcf6551fc8b47e7326f02417c", install = TRUE)

dc_income <- get_acs(geography = "tract", variables = "B19013_001", #code for median income 
                     state = "DC", year=2019, geometry=TRUE)
plot(dc_income)
#make plot 

ggplot(dc_income)+geom_sf(aes(fill=estimate), color="white", linewidth=0.2)+
                          scale_fill_viridis_c(label=scales::comma)+
  labs(title = "Median Household Income by Census Tract",
       subtitle = "Washington, D.C.",
       fill = "Median Income ($)") +
    theme_void()

View(load_variables(year = 2019, dataset = "acs5"))
#create data frame to look at the variables from Census data 
v19 <- load_variables(2019, "acs5", cache = TRUE)
view(v19)

#get census data for race
dc_race <- get_acs(geography="tract", variables = c(
  White = "B02001_002",
  Black = "B02001_003",
  Asian = "B02001_005",
  Hispanic = "B03003_003",
  Other = "B02001_007"),
  state="DC",
  year = 2019,
  geometry = TRUE)

#create data frame that highlights the predominant race
dc_predominant_race <- dc_race %>%
  group_by(GEOID) %>%
  slice_max(estimate, n = 1) %>%
  ungroup()

#map the predominant races per census tract
ggplot(dc_predominant_race)+
  geom_sf(aes(fill = variable), color = "white", linewidth = 0.2)+
  scale_fill_brewer()+
  labs(title= "Dominant Race by Census Tract - 2019",
       subtitle = "Washington D.C.",
       fill="Race")+
  theme_void()

#load Heat Sensitivity Exposure Index
Heat_Sens_Index <- read.csv("Heat_Sensitivity-Exposure_Index.csv")

unique(Heat_Sens_Index)

DC_Heat_data <- rename(Heat_Sens_Index, GEOID = GEO_ID)
DC_Heat_data$GEOID2 <- gsub("1400000US","",DC_Heat_data$GEOID)

head(DC_Heat_data)

#join heat data to DC data
DC_fulldata <- full_join(dc_income, # left table
                         DC_Heat_data, # right table
                         by=c("GEOID"="GEOID2")) 
library(sf)

DC_INT <- st_intersection(dc_income, DC_Heat_data)
unique(DC_INT)

DC_Heat_data
#adress the fact that census plots are different 
DC_Heat <- geojson_sf("/users/katebroeksmit/Downloads/Heat_Sensitivity_Exposure_Index.geojson")
DC_HeatP <- st_transform(DC_Heat, crs = 4269)
DC_INT <- st_intersection(dc_income, DC_HeatP)

#Data Visualizations for DC Heat Data
#Heat Index
ggplot(DC_INT)+ geom_sf(aes(fill = HEI))+
  scale_fill_gradient(low = "white", high = "red")+
  coord_sf(datum = NA)+
  labs(title= "Heat Exposure Index",
       subtitle = "Washington D.C.")+theme_void()

#Tree Cover 
ggplot(DC_INT)+
  geom_sf(aes(fill = P_TREECOVER))+
  scale_fill_gradient(low = "white", high = "forestgreen")+
  coord_sf(datum = NA)+
  labs(title= "Percent Tree Cover",
       subtitle = "Washington D.C.")+theme_void()

ggplot(DC_INT)+
  geom_sf(aes(fill = P_IMPSURF))+
  scale_fill_gradient(low = "white", high = "black")+
  coord_sf(datum = NA)+
  labs(title= "Impervious Surfaces",
       subtitle = "Washington D.C.")+theme_void()


#Looking at Income and Tree Cover
ggplot(DC_INT, aes(x=estimate, y=P_TREECOVER))+
  geom_point(color = "forestgreen", alpha = 0.6, size = 2)+
  geom_smooth(method = "lm", color = "darkgreen", se = TRUE)+
  scale_x_continuous(labels = scales::dollar_format())+
  labs(title="Percent Tree Cover based on Median Income",
       subtitle="Washington D.C.",
       x="Median Income",
       y="Percent Tree Cover")+
  theme_classic()

#looking at Income and Impervious Surfaces
ggplot(DC_INT, aes(x=estimate, y=P_IMPSURF))+
  geom_point(color = "black", alpha = 0.6, size = 2)+
  geom_smooth(method = "lm", color = "red", se = TRUE)+
  scale_x_continuous(labels = scales::dollar_format())+
  labs(title="Percent Impervious Surfaces",
       subtitle="Washington D.C.",
       x="Median Income",
       y="Percent Impervious Surfaces")+
  theme_classic()

#join heat and race data
class(dc_predominant_race)
#make sure each frame uses same Datum/spatial projection
st_crs(dc_predominant_race)
st_crs(DC_HeatP)
#change the GEOIDs to match between Race and Heat data
DC_HeatP <- DC_HeatP %>%
  mutate(GEO_ID = gsub("1400000US", "", GEO_ID))
         
#join the data frames by GEOID
DC_Race_INT <- dc_predominant_race %>%
  left_join(st_drop_geometry(DC_HeatP), by = c("GEOID" = "GEO_ID"))
#look at the elements -- make sure everything transfered properly
unique(DC_Race_INT$variable)
unique(DC_Race_INT$HEI)

#Make data visualzation that looks at Census Tracts 
#that are Predomintley White or Black
#first filter, then create the plot
#create box plot for Air Temp
DC_Race_INT %>%
  filter(variable %in% c("White", "Black")) %>%
  ggplot(aes(x=variable, y=AIRTEMP_MEAN, fill=variable))+
  geom_boxplot(alpha=0.7)+
  scale_fill_manual(values = c("White"="steelblue", "Black"="darkorange"))+
                      labs(title="Comparing Mean Air Temperatures",
                           subtitle = "Mean Air Temperature in Predominantly White vs. Predominantly Black Census Tracts", 
                           x= NULL,
                           y="Mean Air Temperature")+
                             theme_classic()
#create box plot for Treecover 
DC_Race_INT %>%
  filter(variable %in% c("White", "Black")) %>%
  ggplot(aes(x=variable, y=P_TREECOVER, fill=variable))+
  geom_boxplot(alpha=0.7)+
  scale_fill_manual(values = c("White"="darkgreen", "Black"="darkgreen"))+
  labs(title="Comparing Percent Tree Cover",
       subtitle = "Tree Cover in Predominantly White vs. Predominantly Black Census Tracts", 
       x= NULL,
       y="Percent Tree Cover")+
  theme_classic()

#Moving back to DC full data, can use the % POC column to continue to examine
#how diffrent variables impact diffrent communites differently 

#make graph of HEI based on percent POC
ggplot(DC_INT, aes(x=cut_number(P_POC,4), y=HEI))+
  geom_boxplot(fill="royalblue", alpha=0.7)+
  scale_x_discrete(labels = c("Low", "Mid-Low", "Middle", "Mid-High", "High")) +
  labs(title = "Heat Exposure Index by % People of Color",
       subtitle = "Washington D.C. Census Tracts",
       x="Percent People of Color (Quartiles)",
       y= "Heat Exposure Index (HEI)")+
  theme_minimal()

#make graph of Air Temp based on % POC
ggplot(DC_INT, aes(x=cut_number(P_POC,4), y=AIRTEMP_MEAN))+
  geom_boxplot(fill="darkorange", alpha=0.7)+
  scale_x_discrete(labels = c("Low", "Mid-Low", "Middle", "Mid-High", "High")) +
  labs(title = "Mean Air Temperature by % People of Color",
       subtitle = "Washington D.C. Census Tracts",
       x="Percent People of Color (Quartiles)",
       y= "Mean Air Temperature (Celsius))")+
  theme_minimal()

#scatter plot of % POC with Air Temp
ggplot(DC_INT, aes(x=P_POC, y=AIRTEMP_MEAN))+
  geom_point(color="darkorange2", alpha=0.7)+
  geom_smooth(method = "lm", color = "red", se = TRUE)+ 
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Mean Air Temperature",
       subtitle = "Washington D.C. Census Tracts",
       x="Percent People of Color",
       y= "Heat Exposure Index (HEI)")+
  theme_classic()
  
  
# DC Parks ---------------------------------------------------------

getwd()
list.files()
list.files("/Users")

#DC parks and rec 
DC_Parks <- geojson_sf("/users/katebroeksmit/Downloads/Parks_and_Recreation_Areas.geojson")
#change the datum
DC_ParksP <- st_transform(DC_Parks, crs = 4269)
head(DC_Parks)
ggplot()+
  geom_sf(data=DC_ParksP, fill="forestgreen", alpha=0.6)+
  theme_void()

#DC national parks 
DC_NatlParks <- geojson_sf("/users/katebroeksmit/Downloads/National_Parks.geojson")
DC_NatlParksP <- st_transform(DC_NatlParks, crs = 4269)
#make plot of parks 
head(DC_NatlParksP)
ggplot()+
  geom_sf(data=DC_NatlParksP, fill="forestgreen", alpha=0.6)+
  theme_void()

data(DC_NatlParksP)



#join the data frames 
colnames(DC_NatlParksP)
colnames(DC_ParksP)
library(sf)
#join by global ID?
allParks <- st_join(DC_NatlParksP, DC_ParksP, join = st_intersects)
#didn't join properly, the DC parks are not showing up
ggplot()+
  geom_sf(data=allParks, fill="forestgreen", alpha=0.6)+
  theme_void()

               
                            

