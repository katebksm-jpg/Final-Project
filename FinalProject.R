
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
install.packages(c("tigris", "patchwork", "tidyverse"))
install.packages("tidycensus")
library(tigris)
library(patchwork)
library(tidyverse)
library(tidycensus)
setwd("/Users/katebroeksmit/Desktop/Final-Project")



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
Heat_Sens_Index <- read.csv("/Users/katebroeksmit/Desktop/ENVST Final Project Files/Heat_Sensitivity-Exposure_Index.csv")
unique(Heat_Sens_Index)

DC_Heat_data <- rename(Heat_Sens_Index, GEOID = GEO_ID)
DC_Heat_data$GEOID2 <- gsub("1400000US","",DC_Heat_data$GEOID)

head(DC_Heat_data)

library(sf)

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
       subtitle = "Washington D.C.",
       caption = "HEI is calculated using Air Temperatures (50%), 
      lack of tree canopy (25%), and impervious surfaces (25%)",
       fill="HEI (0-1)")+theme_void()+
  theme(plot.caption = element_text(hjust = 0.5, margin = margin(t=10)),
        plot.margin = margin(10, 10, 30, 10))

#plot the three elements that make up HEI

#Mean Air Temp
#first convert °C to F 
DC_INT$Airtemp_Mean_F <- DC_INT$AIRTEMP_MEAN * 9/5 + 32


ggplot(DC_INT)+ geom_sf(aes(fill = Airtemp_Mean_F))+
  scale_fill_gradient(low = "white", high = "red", name="Ambient Air Temperature (°F)")+
  coord_sf(datum = NA)+
  labs(title= "Washington D.C. Air Temperature (°F)",
       subtitle = "Mean Ambient Air Temperature, Data From August 2018") +
  theme_void()

#Tree Cover 
ggplot(DC_INT)+
  geom_sf(aes(fill = P_TREECOVER))+
  scale_fill_gradient(low = "white", high = "forestgreen", name="% Tree Cover")+
  coord_sf(datum = NA)+
  labs(title= "Percent Tree Cover",
       subtitle = "Washington D.C.")+theme_void()

#Impervious Surfaces
ggplot(DC_INT)+
  geom_sf(aes(fill = P_IMPSURF))+
  scale_fill_gradient(low = "white", high = "black", name="% impervious surface")+
  coord_sf(datum = NA)+
  labs(title= "Impervious Surfaces",
       subtitle = "Washington D.C.")+theme_void()

#look at relationships between elements 

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
#change the GEOIDs so that they are formatted the same between Race and Heat data
DC_HeatP <- DC_HeatP %>%
  mutate(GEO_ID = gsub("1400000US", "", GEO_ID))
         
#join the data frames by GEOID
DC_Race_INT <- dc_predominant_race %>%
  left_join(st_drop_geometry(DC_HeatP), by = c("GEOID" = "GEO_ID"))
#look at the elements -- make sure everything transferred properly
unique(DC_Race_INT$variable)
unique(DC_Race_INT$HEI)

#work on data visualizations with this new data frame
#Make data visualization that looks at Census Tracts 
#that are Predominant White or Black
#first filter, then create the plot
DC_Race_INT %>%
  filter(variable %in% c("White", "Black")) %>%
  ggplot(aes(x=variable, y=AIRTEMP_MEAN, fill=variable))+
  geom_boxplot(alpha=0.7)+
  scale_fill_manual(values = c("White"="steelblue", "Black"="darkorange"))+
                      labs(title="Comparing Mean Air Temperatures",
                           subtitle = "Mean Air Temperature in Predominantly White vs. Black Census Tracts", 
                           x= NULL,
                           y="Mean Air Temperature")+
                             theme_classic()

#create box plot for Tree Cover 
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
#how different variables impact different communites differently 


#make graph of Air Temp based on % POC
ggplot(DC_INT, aes(x=cut_number(P_POC,4), y=Airtemp_Mean_F))+
  geom_boxplot(fill="darkorange", alpha=0.7)+
  scale_x_discrete(labels = c("0–34%", "34–63%", "63–92%", "92–100%")) +
  labs(title = "Mean Air Temperature (August 2018) by % People of Color",
       subtitle = "Washington D.C. Census Tracts",
       x="Percent People of Color",
       y= "Mean Air Temperature (°F)")+
  theme_minimal()

#HEI box plot
ggplot(DC_INT, aes(x=cut_number(P_POC,4), y=HEI))+
  geom_boxplot(fill="red3", alpha=0.7)+
  stat_summary(fun = mean, geom = "point", color="gray")+
  scale_x_discrete(labels = c("0–34%", "34–63%", "63–92%", "92–100%"))+
  labs(title = "Heat Exposure Index by % People of Color",
       subtitle = "Demographics from Washington D.C. Census Tracts",
       caption = "Gray Dot Reprsents Mean HEI",
       x="Percent People of Color",
       y= "HEI (0-1)")+
  theme_minimal()


#check the values of the bins to reformat labels
quantile(DC_INT$P_POC, probs = seq(0, 1, 0.25), na.rm = TRUE)

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


#% POC map
ggplot(DC_INT)+
  geom_sf(aes(fill=cut_number(P_POC,4)), color="white", size=0.2)+
  labs(title= "Percent People of Color in DC Census Tracts",
       subtitle="2015-2019 Census Data",
       fill = "% POC",
       caption = "% POC is defined as Percent who did not identify as 'Not Hispanic or
       Latino, White Alone")+
    theme_void()+
  theme(plot.caption=element_text(hjust = 0))
  

# DC Parks ---------------------------------------------------------

getwd()
list.files()
list.files("/Users")

#DC parks and rec 
DC_Parks <- geojson_sf("/users/katebroeksmit/Downloads/Parks_and_Recreation_Areas.geojson")
#change the datum
DC_ParksP <- st_transform(DC_Parks, crs = 4269)
head(DC_Parks)

ggplot() +
  geom_sf(data = DC_ParksP, fill = "forestgreen", alpha = 0.6) +
  theme_void()

#DC national parks 
DC_NatlParks <- geojson_sf("/users/katebroeksmit/Downloads/National_Parks.geojson")
DC_NatlParksP <- st_transform(DC_NatlParks, crs = 4269)
#make plot of parks 
head(DC_NatlParksP)
plot(DC_NatlParks$geometry)
plot(DC_ParksP$geometry, add=TRUE)

#join the data frames 
colnames(DC_NatlParksP)
colnames(DC_ParksP)

library(sf)
#join park data frames
allParks <- bind_rows(DC_NatlParksP, # left table
                      DC_ParksP)


plot(allParks$geometry, col="forestgreen", border=NA)


#create map of HEI and Parks together
ggplot()+
  geom_sf(data=DC_INT, aes(fill=HEI), color=NA)+
  scale_fill_gradient(low = "white", high = "red", name = "HEI (0-1)")+
  geom_sf(data=allParks, fill="forestgreen", alpha=0.5, color=NA)+
  coord_sf(datum=NA)+
  labs(
    title="Heat Exposure Index and Parks",
    subtitle = "Washington D.C.",
    caption="Green areas: Parks, Red Areas: Higher HEI")+
  theme_void()

#create map of race based on census tracts and parks 
st_crs(dc_predominant_race)
st_crs(allParks)
#in same 
#first map predominant race 
ggplot()+
  geom_sf(data=dc_predominant_race, aes(fill=variable), color="white", 
          linewidth=0.2, alpha=0.6)+
  scale_fill_manual(values = c("White" = "#5B7C99", "Black" = "#F28E2B", "Asian"="#59A14F", 
                    "Hispanic"= "#E15759",  "Other"= "#B07AA1"), name="Dominant Race")+
  #add the parks
  geom_sf(data=allParks, fill="forestgreen", alpha=0.8, color=NA)+
  coord_sf(datum=NA)+
  labs(
    title="Dominant Race by Census Tract and Park Location",
    subtitle = "Washington D.C. - 2019",
    caption="Green areas: Parks, Color: Dominant Race per Census Tract")+
  theme_void()+
  theme(plot.caption.position = "plot")

#income and parks
ggplot(dc_income)+geom_sf(aes(fill=estimate), color="white", linewidth=0.2, alpha=0.6)+
  scale_fill_viridis_c(option="plasma", label=scales::comma)+
  geom_sf(data=allParks, fill="forestgreen", alpha=0.8, color=NA)+
  coord_sf(datum=NA)+
  labs(
    title="Income and Park Location",
    subtitle = "Washington D.C. - 2019",
    caption="Green areas: Parks, Color: Income")+
  theme_void()