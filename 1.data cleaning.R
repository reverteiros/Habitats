
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats/Data correct")

library("tidyverse")
library("ggplot2")
library("dplyr")
library(sf)
library(reshape2)
library(sp)
library(raster)
library(ggmap)
library(mapr)
library(rgbif)
library(dismo)



############ Data cleaning


Bees <- read.csv("Data_bees.csv",header = T, sep = ";") %>%
  dplyr::filter(!is.na(SPEC_ID)) %>%
  transform(., Year = substr(DAT2, 1, 4), Month = substr(DAT2, 5, 6), Day = substr(DAT2, 7, 8))%>%
  dplyr::filter(Year==2020) 



######### filter dataset

data_few_columns <- dplyr::select(Bees,SPEC.TAXPRIO,SPEC.GEN,SEX,N,TOPO,DAT2)


# #### Sampling repetitions. There are some sites we only have 2 repetitions. I removed them
# 
# samplings_crude <- data_few_columns %>%
#   dplyr::group_by(TOPO) %>%
#   dplyr::summarize(sampling_events=n_distinct(DAT2),N_n=sum(N)) 
# 
# # check how many bees were collected per site as a function of how many samplings were conducted. The one with only 2 samplings is very low
# ggplot(samplings_crude, aes(y=N_n,x=sampling_events)) +
#   geom_point() +
#   theme_classic()
# 
# # remove sites only sampled 2 times
# samplings <- data_few_columns %>%
#   dplyr::group_by(TOPO) %>%
#   dplyr::summarize(sampling_events=n_distinct(DAT2),N_n=sum(N)) %>%
#   dplyr::filter(sampling_events>2) %>%
#   dplyr::select(-c(sampling_events, N_n))




#################### decide the sites that are not too close between them and to the border with France. Also remove the only site that is forest

Coordinates <- read.csv("sites_decided.csv",header = T, sep = ";") %>%
  dplyr::filter(distance=="ok")%>%
  dplyr::select(-c(distance,why)) %>%
  dplyr::mutate(Y=Lati) %>%
  dplyr::mutate(X=Long) 


data_clean <- data_few_columns%>%
  # inner_join(samplings,by=c("TOPO")) %>%
  inner_join(Coordinates,by=c("TOPO")) 

sum(data_clean$N)#4957

# separate bombus and other wild bees. They may have different trends

data_clean_bombus <- data_clean %>%
  dplyr::filter(SPEC.GEN=="Bombus")

data_clean_NO_bombus <- data_clean %>%
  dplyr::filter(SPEC.GEN!="Bombus")


# summarise the dataset per plot - abundance, richness


summarised <- data_clean %>%
  dplyr::group_by(TOPO) %>%
  dplyr::summarize(Richness=n_distinct(SPEC.TAXPRIO),Abundance=sum(N))

summarised_bombus <- data_clean_bombus %>%
  dplyr::group_by(TOPO) %>%
  dplyr::summarize(Richness=n_distinct(SPEC.TAXPRIO),Abundance=sum(N)) 

summarised_NO_bombus <- data_clean_NO_bombus %>%
  dplyr::group_by(TOPO) %>%
  dplyr::summarize(Richness=n_distinct(SPEC.TAXPRIO),Abundance=sum(N)) 




############ plot sites on space


coordinates(Coordinates)<-c("X","Y")
crs(Coordinates) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

## Mapa interactiu
names(Coordinates) <- "name"  #required for mapr
map_leaflet(Coordinates, lon = "X", lat = "Y")



## Mapa google maps
##plot(gmap(Coordinates, type = "satellite"))
##points(Mercator(Coordinates), col = "red", pch = 20, cex = database$Honeybees/15)

##database$Honeybee_visit_rate <- database$Honeybees/database$OVERALL_Flowers*1000




############## obtain distances in m and km between pairs of sites

species_sf <- st_as_sf(Coordinates, coords = c("Long", "Lati"), crs = 4326)


distances <- st_distance(species_sf)
matrixdist <- as.matrix(distances)
d.dist<-as.dist(matrixdist)
matrixdist <- as.matrix(d.dist)
matrixdist[lower.tri(matrixdist)] <-NA

rows <- melt(matrixdist) %>%
  dplyr::filter(!is.na(value)) %>%
  mutate(value_km=(value/1000)) %>%
  dplyr::filter(value_km!=0) 


hist(rows$value_km)
mean(rows$value_km)
min(rows$value_km)
max(rows$value_km)


# Code to decide if the sites that are close could be all kept. In the cluster analysis sites that are close tend to be together, so better decide some and not all points
# 
# summarised_dist_abundance <- dist(summarised$Abundance) 
# matrixdistabund <- as.matrix(summarised_dist_abundance) 
# matrixdistabund[lower.tri(matrixdistabund)] <-NA
# rowsabund <- melt(matrixdistabund) %>%
#   dplyr::filter(!is.na(value)) %>%
#   mutate(abundance=value) %>%
#   dplyr::select(-value)
# 
# 
# summarised_dist_richness <- dist(summarised$Richness) 
# matrixdistrich <- as.matrix(summarised_dist_richness) 
# matrixdistrich[lower.tri(matrixdistrich)] <-NA
# rowsrich <- melt(matrixdistrich) %>%
#   dplyr::filter(!is.na(value)) %>%
#   mutate(richness=value) %>%
#   dplyr::select(-value)
# 
# datafinal <- rows %>%
#   left_join(rowsabund,by=c("Var1","Var2"))%>%
#   left_join(rowsrich,by=c("Var1","Var2"))
# 
# ggplot(datafinal,aes(x=value,y=abundance)) +
#   geom_jitter()+
#   coord_cartesian(xlim = c(0, 1000), ylim = c(0, 150)) +
#   theme_bw()
# 
# ggplot(datafinal,aes(x=value,y=richness)) +
#   geom_jitter()+
#   coord_cartesian(xlim = c(0, 20500), ylim = c(0, 40)) +
#   theme_bw()
# 
# 
# database1_betadiv_with_sites <- data_clean %>%
#   dplyr::inner_join(Coordinates,by="TOPO")%>%
#   group_by(TOPO,SPEC.TAXPRIO) %>% 
#   dplyr::summarize(Abundance=sum(N)) %>% 
#   tidyr::spread(SPEC.TAXPRIO, Abundance) %>%
#   ungroup()
# 
# database1_betadiv_with_sites[is.na(database1_betadiv_with_sites)] <- 0 # the above generates NA, transform to 0
# 
# database1_betadiv <- database1_betadiv_with_sites %>%
#   dplyr::select(-TOPO) 
# 
# 
# tpollinators<-t(database1_betadiv)
# 
# # Ward Hierarchical Clustering with Bootstrapped p values
# 
# library(pvclust)
# 
# fit <- pvclust(tpollinators, method.hclust="ward",method.dist="euclidean")
# 
# plot(fit) 
# 
# pvrect(fit, alpha=.95)
# 

