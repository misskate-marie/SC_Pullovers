# loading
library(dplyr)
library(tidycensus)
library(sf)
library(readr)
library(tigris)
library(ggplot2)
library(scales)
library(viridis)
library(censusapi)
library(rgdal)
library(devtools)

# scpullovers .csv
scpullovers <- read_csv("SC-clean.csv")
scpullovers <- filter(scpullovers, lat!=0, lon!=0)
glimpse(scpullovers)

# Census key
census_api_key(file("key.rda"), overwrite = TRUE, install = TRUE)

# Tigris and tracts
options(tigris_class = "sf")
charleston <- tracts(state="SC", county="Charleston", cb=T)

# api key to determine FIPS
charleston_towns <- county_subdivisions(state="SC", county="Charleston", cb=T)
northcharleston_city <- filter(charleston_towns, NAME=="North Charleston")

# scpullovers_spatial with api key
scpullovers_spatial <- scpullovers %>%
  st_as_sf(coords=c("lon", "lat"),
           crs = "+proj=longlat") %>%
  st_transform(crs=st_crs(charleston))

# Join two datasets
points_in <- st_join(charleston, scpullovers_spatial, left=T)

# group by and summarise
by_tract <- points_in %>%
  group_by(GEOID) %>%
  summarise(total=n())

# View by_tract
head(by_tract)

# ggplot of North Charleston Police
ggplot(by_tract) +
  geom_sf(aes(fill = total), color=NA) +
  geom_sf(data=northcharleston_city, fill=NA, color="black") +
  coord_sf(datum=NA) +
  labs(title = "Total traffic stops by North Charleston police",
       subtitle = "From 2005 to 2016",
       caption = "Source: Stanford Open Policing Data",
       fill = "Total stops") +
  scale_fill_viridis(option="magma", direction=-1) +
  NULL

