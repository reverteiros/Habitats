
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats/data")

library('ggpubr')
library("SpatialTools")
library("iNEXT")
library("BiodiversityR")
library("rgdal")
library("ape")
library("vegan")
library("geosphere")
library("sf")
library("SpatialTools")
library("betapart")
library("pvclust")
library("mapr")
library("rgbif")
library("dismo")
library("mapplots")
library("corrplot")
library("MuMIn")
library("caret")
library("pbkrtest")
library("car")
library("ggmap")
library("gstat")
library("tidyverse")
library("ggplot2")
library("pheatmap")
library("reshape2")
library("reshape") 
library("SoDA")
library("dplyr")
library("bipartite")
library("ggplot2")


############ Data cleaning


DataAlexreese <- read.csv("Data-AlexReese.csv",header = T, sep = ";") %>%
  dplyr::group_by(TOPO) %>%
  dplyr::summarize(N=sum(N),samplings=n_distinct(DAT2))
DataEulalieruelle <- read.csv("Data-Eulalie.csv",header = T, sep = ";")%>%
  dplyr::group_by(TOPO) %>%
  dplyr::summarize(N=sum(N),samplings=n_distinct(DAT2))
DataMartinloockx <- read.csv("Data-Loockx.csv",header = T, sep = ";")%>%
  dplyr::group_by(TOPO) %>%
  dplyr::summarize(N=sum(N),samplings=n_distinct(DAT2))
DataAlexlefebre <- read.csv("DataAlexLefebvre.csv",header = T, sep = ";")%>%
  dplyr::group_by(TOPO) %>%
  dplyr::summarize(N=sum(N),samplings=n_distinct(DAT2))
DataWilliam <- read.csv("Data-William.csv",header = T, sep = ";")%>%
  dplyr::group_by(TOPO) %>%
  dplyr::summarize(N=sum(N),samplings=n_distinct(DAT2))
DataMathilde <- read.csv("Data-Mathilde.csv",header = T, sep = ";")%>%
  dplyr::group_by(TOPO) %>%
  dplyr::summarize(N=sum(N),samplings=n_distinct(DAT2))



DataAlexreese <- read.csv ( "Data-AlexReese.csv",header = T, sep = ";") %>% mutate(Student="AlexReese")
DataEulalieruelle <- read.csv ( "Data-Eulalie.csv",header = T, sep = ";") %>% mutate(Student="Eulalie")
DataMartinloockx <- read.csv ( "Data-Loockx.csv",header = T, sep = ";") %>% mutate(Student="Martinloockx") 
DataAlexlefebre <- read.csv ( "DataAlexLefebvre.csv",header = T, sep = ";") %>% mutate(Student="Alexlefebre") 
DataWilliam <- read.csv ( "Data-William.csv",header = T, sep = ";") %>% mutate(Student="William")%>%
  dplyr::filter(DAT2 > 20190520)
DataMathilde <- read.csv ( "Data-Mathilde.csv",header = T, sep = ";") %>% mutate(Student="Mathilde")%>%
  dplyr::filter(DAT2 > 20180520) %>%
  mutate(STAT.CODE = stat_memo)
DataMathilde$HAB1 <-  as.logical(DataMathilde$HAB1)
DataWilliam$SRC <-  as.integer(DataWilliam$SRC)


sum(DataAlexreese$N)+sum(DataEulalieruelle$N)+sum(DataMartinloockx$N)+sum(DataAlexlefebre$N)
#6362 bees


data_total <- bind_rows(DataAlexreese, DataEulalieruelle, DataMartinloockx, DataAlexlefebre)

data_total_sp <- data_total %>%
  dplyr::group_by(SPEC.TAX) %>%
  dplyr::summarize(N=sum(N))


data_total <- data_total %>% filter(SPEC.TAXPRIO != "Andrena  sp.")
data_total <- data_total %>% filter(SPEC.TAXPRIO != "Lasioglossum  sp.")
data_total <- data_total %>% filter(SPEC.TAXPRIO != "Osmia latreillei")

data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("Bombus pascuorum floralis")]<-"Bombus pascuorum"
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("Bombus pascuorum floralis")]<-"Bombus pascuorum"
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("Bombus terrestris terrestris")]<-"Terrestribombus sp."
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("Bombus terrestris terrestris")]<-"Terrestribombus sp."
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("Bombus terrestris")]<-"Terrestribombus sp."
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("Bombus lucorum")]<-"Terrestribombus sp."
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("Bombus hypnorum ericetorum")]<-"Bombus hypnorum"
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("Bombus hortorum hortorum")]<-"Bombus hortorum"
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("Bombus (Bombus)  sp.")]<-"Terrestribombus sp."
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("Bombus campestris campestris")]<-"Bombus campestris"
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("Chalicodoma ericetorum")]<-"Megachile ericetorum"
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("Hoplosmia spinulosa")]<-"Osmia spinulosa"
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("osmia spinulosa")]<-"Osmia spinulosa"
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("Halictus confusus")]<-"Seladonia confusa"
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("Halictus tumulorum")]<-"Seladonia tumulorum"
data_total$SPEC.TAXPRIO[data_total$SPEC.TAXPRIO  %in% c("halictus tumulorum")]<-"Seladonia tumulorum"

data_total_sp <- data_total %>%
  dplyr::group_by(SPEC.TAXPRIO) %>%
  dplyr::summarize(N=sum(N))
# 151 sp total. 


######### filter dataset

data_total_filtered <- select(data_total,SPEC.TAXPRIO,SPEC.GEN,SPEC.GR2,SEX,STAT.CODE,N,TOPO,LATI,LONG,Student,DAT2)


#### Sampling repetitions

data_total_samplings <- data_total_filtered %>%
  dplyr::group_by(STAT.CODE,TOPO,Student) %>%
  dplyr::summarize(sampling_events=n_distinct(DAT2)) %>%
  dplyr::filter(sampling_events>2) %>%
  dplyr::select(-sampling_events)

# remove sites only sampled 1 or 2 times

data_total_filtered2 <- data_total_filtered%>%
  inner_join(data_total_samplings,by=c("TOPO","STAT.CODE","Student"))

sum(data_total_filtered2$N)#6347


#### Check number of bees collected per sampling per student

data_total_students <- data_total_filtered %>%
  #transform(., Year = substr(DAT2, 1, 4), Month = substr(DAT2, 5, 6), Day = substr(DAT2, 7, 8))%>%
  dplyr::group_by(STAT.CODE,TOPO,DAT2,Student) %>%
  #dplyr::summarize(Abundance=sum(N),ricnhess=n_distinct(SPEC.TAXPRIO))
  dplyr::summarize(abundance=sum(N))%>%
  inner_join(data_total_samplings,by=c("TOPO","STAT.CODE","Student"))
  

ggplot(data_total_students, aes(y=abundance, x=Student)) + 
  geom_violin(alpha=0.5) + 
  theme_classic() 




############### traits bees

data_total_sp2 <- data_total_filtered2 %>%
  dplyr::group_by(SPEC.TAXPRIO) %>%
  dplyr::summarize(N=sum(N))
# 151 sp 


Beetraits <- read.csv("Traits_bees.csv",header = T, sep = ";") %>%
  mutate(SPEC.TAXPRIO = ï..espece) %>%
  dplyr::select(-ï..espece)

data_total_with_traits <- data_total_filtered2 %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") 


ggplot(data_total_with_traits, aes(y=N,x=Endangered)) + 
  geom_point() + 
  theme_classic() 



############ plot sites on space

data_total_filtered <- select(data_total,SPEC.TAXPRIO,SPEC.GEN,SPEC.GR2,SEX,STAT.CODE,N,TOPO,LATI,LONG,Student,DAT2) %>%
  dplyr::group_by(TOPO) %>%
  dplyr::filter(Student!="Mathilde")%>%
  dplyr::summarize(latitude=mean(LATI),longitude=mean(LONG))%>%
  dplyr::filter(!is.na(latitude))

ggplot(data_total_filtered, aes(x = longitude, y = latitude)) + 
  geom_point() +
  theme_bw() +
  theme(legend.position = "top")+
  coord_fixed()




################ different habitats for each site

habitats <- read.csv("listetype.csv",header = T, sep = ";") %>%
  mutate(STAT.CODE=STATCODE)  %>%
  select(STAT.CODE,type)

data_total_with_habitats <- data_total_filtered2 %>%
  left_join(habitats,by="STAT.CODE")%>%
  dplyr::group_by(STAT.CODE,type) %>%
  dplyr::summarize(Abundance=sum(N),richness=n_distinct(SPEC.TAXPRIO))


ggplot(data_total_with_habitats, aes(x = type, y = Abundance)) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "top")




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


################## Buffers



