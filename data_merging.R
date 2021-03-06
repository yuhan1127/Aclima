library(sp)
library(lattice)
library(data.table)
library(gstat)
library(tidycensus)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(spNNGP)

census_api_key("ea4f942d5bb621e8b0f85b5bacdc4864cbd0ef26",install=TRUE)
readRenviron("~/.Renviron")
options(tigris_use_cache = TRUE)


# ACLIMA DATA
aclima<-fread("/Users/why/Desktop/uw/course/2021 winter/Aclima/Aclima_0114.csv")
aclima_no2<-aclima[modality=="no2"&metric=="mean"]

## check negative value
nrow(aclima_no2[aclima_no2$value<0,])
## replace negative value with small positive value 
## (!!!should use detection limit!!!)
aclima_no2$value<-with(aclima_no2,ifelse(value>0,value,0.001))
## check outlier (small percentage, keep those datapoints)
summary(aclima_no2$value)
## convert to spatial data frame
coordinates(aclima_no2)<-c("lon","lat")

# ACS DATA
source("acs_variables.R")
alameda<-st_as_sf(alameda,sf_column_name ="geometry")

# DATA MAPPING: Map air pollutant coordinate to block group (WGS84->NAD83)
aclima_coord<-sf::st_as_sf(aclima_no2, coords=c("lon","lat"), crs=4326)
aclima_coord= st_set_crs(aclima_coord, 4326)
alameda_4326<- st_transform(alameda,crs=4326)
int <- sf::st_intersects(aclima_coord, alameda_4326)
ind<-sapply(int,FUN=function(x){
  length(x)==0
})

# DATA MERGING: merge aclima and acs
aclima_merge<-aclima_coord[!ind,]
aclima_merge$geoid <- as.character(alameda_4326$GEOID[unlist(int)])
data_merge<-merge(as.data.frame(aclima_merge),
                         as.data.frame(alameda),
                         by.x="geoid",by.y="GEOID")

colnames(data_merge)[8]<-"geometry_aclima"
colnames(data_merge)[24]<-"geometry_acs"

## transform merged data from data frame object to sf object 
data_merge<-sf::st_as_sf(data_merge, sf_column_name=c("geometry_aclima"))
st_geometry(data_merge)

## data_merge is ready for model fitting!!

