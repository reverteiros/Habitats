
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats/Data correct")

library("tidyverse")
library("ggplot2")
library("dplyr")




############ Data cleaning


Bees <- read.csv("Data_bees_simplified.csv",header = T, sep = ";") %>%
  dplyr::filter(!is.na(SPEC_ID)) %>%
  transform(., Year = substr(DAT2, 1, 4), Month = substr(DAT2, 5, 6), Day = substr(DAT2, 7, 8))%>%
  dplyr::filter(Year==2020) 

  dplyr::group_by() %>%
  dplyr::summarize(N=n_distinct(SPEC.GEN))



######### filter dataset

data_few_columns <- select(Bees,SPEC.TAXPRIO,SPEC.GEN,SEX,N,TOPO,LATI,LONG,DAT2)


#### Sampling repetitions

samplings <- data_few_columns %>%
  dplyr::group_by(TOPO) %>%
  dplyr::summarize(sampling_events=n_distinct(DAT2),N_n=sum(N)) %>%
  dplyr::filter(sampling_events>2) %>%
  dplyr::select(-c(sampling_events, N_n))

# ggplot(samplings, aes(y=N_n,x=sampling_events)) + 
#   geom_point() + 
#   theme_classic() 


# remove sites only sampled 1 or 2 times

data_clean <- data_few_columns%>%
  inner_join(samplings,by=c("TOPO")) 

sum(data_clean$N)#6347



# separate bombus and other wild bees. They may have different trends

data_clean_bombus <- data_clean %>%
  dplyr::filter(SPEC.GEN=="Bombus")

data_clean_NO_bombus <- data_clean %>%
  dplyr::filter(SPEC.GEN!="Bombus")


# summarise the dataset per plot - abundance, richness, originality index


summarised <- data_clean %>%
  dplyr::group_by(TOPO) %>%
  dplyr::summarize(Richness=n_distinct(SPEC.TAXPRIO),Abundance=sum(N)) 

summarised_bombus <- data_clean_bombus %>%
  dplyr::group_by(TOPO) %>%
  dplyr::summarize(Richness=n_distinct(SPEC.TAXPRIO),Abundance=sum(N)) 

summarised_NO_bombus <- data_clean_NO_bombus %>%
  dplyr::group_by(TOPO) %>%
  dplyr::summarize(Richness=n_distinct(SPEC.TAXPRIO),Abundance=sum(N)) 


# ############ plot sites on space
# 
# points_sites <- select(data_total_filtered2,SPEC.TAX,SPEC.GEN,SPEC.GR2,SEX,N,TOPO,LATI,LONG,DAT2) %>%
#   dplyr::group_by(TOPO) %>%
#   dplyr::summarize(latitude=mean(LATI),longitude=mean(LONG))
# 
# ggplot(points_sites, aes(x = longitude, y = latitude)) + 
#   geom_point() +
#   theme_bw() +
#   theme(legend.position = "top")+
#   coord_fixed()
# 
# 
# species_sf <- st_as_sf(data_total_filtered, coords = c("longitude", "latitude"), crs = 4326)
# 
# distances <- st_distance(species_sf)
# matrixdist <- as.matrix(distances)
# d.dist<-as.dist(matrixdist)
# matrixdist <- as.matrix(d.dist)
# matrixdist[lower.tri(matrixdist)] <-NA
# 
# rows <- melt(matrixdist) %>%
#   dplyr::filter(!is.na(value)) %>%
#   mutate(value_km=(value/1000)) %>%
#   dplyr::filter(value_km<1) %>%
#   dplyr::filter(value_km>0)
# 
# 
# hist(rows$value_km)
# table(rows$value_km)

