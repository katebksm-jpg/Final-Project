
# Final Project -----------------------------------------------------------
#Kate Broeksmit
install.packages(c("dplyr", "lubridate"))
install.packages("ggplot2")
library(dplyr)
library(lubridate)
library(ggplot2)
#load Tigris and patchwork Package to help with mapping census data
install.packages(c("tigris", "patchwork", "tidyverse"))
library(tigris)
library(patchwork)
library(tidyverse)
library(tidycensus)
install.packages("tidycensus")
census_api_key("9fc7a703387fd39bcf6551fc8b47e7326f02417c", install = TRUE)

dc_income <- get_acs(geography = "tract", variables = "B19013_001", #code for median income 
                     state = "DC", year=2019, geometry=TRUE)

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
Heat_Sens_Index <- read.csv("/cloud/project/Heat_Sensitivity-Exposure_Index.csv")

ggplot(data = Heat_Sens_Index, aes(fill = Heat_Sens_Index$HSEI)) + 
  geom_sf()
