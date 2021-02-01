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
aclima<-fread("Aclima_0114.csv")
aclima_no2<-aclima[modality=="no2"&metric=="mean"]
aclima_remove<-aclima_no2[aclima_no2$value<100&aclima_no2$value>0,]

## check negative value
nrow(aclima_no2[aclima_no2$value<0,])
## check outlier
summary(aclima_no2$value)
aclima_remove<-aclima_no2[aclima_no2$value<50&aclima_no2$value>0,]
## convert to spatial data frame
coordiates(aclima_remove)<-c("lon","lat")
## heatmap
spplot(aclima_remove, "value", colorkey = TRUE)


# ACS DATA
alameda<- get_acs(state = "CA", county = "Alameda", geography = "block group",
                  variables = "B19013_001",geometry = TRUE)
## check block group level range
block<-as.numeric(str_sub(alameda$NAME,13,13))
## check census tract level range
tract<-as.numeric(str_sub(alameda$NAME,29,32))
## check block group 0, census tract 9900 
alameda_block0<-subset(alameda,str_sub(NAME,13,13)==0)
##Note: no estimate and no geometry
##check census tract 9819,9820,9832,9900
alameda_tract<-subset(alameda,str_sub(NAME,29,32) %in% c("9819","9820","9832","9900"))
##Note: no estimate and geometry for census tract 9980

## Set up palette
pal1 <- brewer.pal(7, "OrRd") 
pal2 <- brewer.pal(7, "Greens")
## overlay aclima data on acs bloch group data
plot(alameda[c("estimate","geometry")], breaks = "quantile", nbreaks = 7,pal=pal1,reset=FALSE)
aclima_coord<-sf::st_as_sf(aclima_remove, coords=c("lon","lat"), crs=4326)
aclima_coord= st_set_crs(aclima_coord, 4326)
plot(aclima_coord["value"], add=TRUE,breaks = "quantile", nbreaks = 7,
     pal=pal2, reset=FALSE, pch=16, cex=0.3)


# DATA MAPPING: Map air pollutant coordinate to block group (WGS84->NAD83)
alameda_4326<- st_transform(alameda,crs=4326)
int <- sf::st_intersects(aclima_coord, alameda_4326)

##check missing value
identical(length(int),length(unlist(int)))
##Note: some coordinates can not be mapped to certain block group (around 124 points,0.13%)
## locate coordinates that failed to find their correspond block group
ind<-sapply(int,FUN=function(x){
  length(x)==0
})
aclima_coord_missing<-aclima_coord[ind,]
plot(alameda[c("geometry")])
plot(aclima_coord_missing["value"],breaks = "quantile", nbreaks = 7,
     pal=pal2,add=TRUE, reset=FALSE, pch=16, cex=1)
## it turns out those data points fall right on (slightly outside) the boundary of alameda county. 
## Also further validate the accuracy of air pollution-block group mapping 
## Current approach is to remove those points.
## For further adjustment 
## 1) calculate distance between data point and block group, assign data points to the nearest block group
##    ***distance metric****
## 2) easy to map coordinates to big block group. (like livermore area)

# DATA MERGING: merge aclima and acs
aclima_merge<-aclima_coord[!ind,]
aclima_merge$geoid <- as.character(alameda_4326$GEOID[unlist(int)])
data_merge_income<-merge(as.data.frame(aclima_merge),
                         as.data.frame(alameda),
                         by.x="geoid",by.y="GEOID")
nrow(data_merge_income) ## 94126
table(is.na(data_merge_income$estimate))
table(data_merge_income$geoid[is.na(data_merge_income$estimate)])
missing_geoid<-data_merge_income$geoid[is.na(data_merge_income$estimate)]
alameda%>%filter(GEOID %in% missing_geoid)%>%group_by(estimate)%>%count()
##Note: missingness is from missing demographics estimates of acs data in the first place

## only keep observation without estimate missingness 
data_merge_income<-subset(data_merge_income,!is.na(estimate))
## transform data from data frame object to sf object 
data_merge_income<-sf::st_as_sf(data_merge_income, sf_column_name=c("geometry.x"))
st_geometry(data_merge_income) <- "geometry.x"



## further step
#1. if int=missing->block_group=missing @
#2. locate coordinates that failed to find their correspond block group. check the reason 
# Reason:  instrument error, fall outside alameda county (more possible) @
#3. figure out how to deal with data points falling on boundary (low priority)
#-----after checking the  mismatched data points------
#1. ses variable ->decide which variable to use and 
#   add them into aclima data as predictor columns, like block group column (index=int) @
#   sol1) use geoid as index to merge aclima with other acs data @
#   sol2) use int, if the order of block group index of acs datasets are the same
#2.log transformed @
#3. fit linear regression and check residual, expect spatial pattern @
#4. variagram
#5. spnngp
#-----after fitting kriging model----------
#1. how to deal with negative value (paper https://pubs.acs.org/doi/10.1021/es00082a001)
#2. outlier -> current approach is removing value>=100, which is not proper
#3. fit model on the original scale and compare it with log-transformed model



# first fit linear model on median income
xyplot(log(value) ~ estimate, data_merge_income)
data_merge_income$log_no2<-log(data_merge_income$value)
model.lm <- lm(log_no2 ~ estimate, data_merge_income)
data_merge_income$fitted.s <- predict(model.lm, data_merge_income)-
  mean(predict(model.lm,data_merge_income),na.rm=TRUE)

plot(data_merge_income[c("value","residuals")],
     breaks = "quantile", nbreaks = 7,
     pal=pal2, pch=16, cex=0.5)

# The figure reveal that although median income predictor explains a large part of variablity, the residuals
# do not appear spatially unstructured or white noise: residuals with a similar value occur regularly
# close to another


