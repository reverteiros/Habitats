
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats/Data correct")

source("data cleaning.R")

dataclean
summarised




################## Landscape

landcover250 <- read.csv("Landcover_250.csv",header = T, sep = ";")%>%
  mutate(TOPO = Site)%>%
  semi_join(summarised,by=c("TOPO")) %>%
  mutate(Bare_Water=Bare+Water) %>%
  mutate(Leaves = BroadLV + NeedlLV) %>%
  mutate(Open = WOpen + NOpen) %>%
  dplyr::select(-c("Site","Bare","Water","BroadLV","NeedlLV","WOpen","NOpen"))

summarised_250 <- summarised %>%
  semi_join(landcover250,by=c("TOPO")) 

summarised_bombus_250 <- summarised_bombus %>%
  semi_join(landcover250,by=c("TOPO")) 

summarised_NO_bombus_250 <- summarised_NO_bombus %>%
  semi_join(landcover250,by=c("TOPO")) 


landcover500 <- read.csv("Landcover_500.csv",header = T, sep = ";")%>%
  mutate(TOPO = Site)%>%
  semi_join(summarised,by=c("TOPO")) %>%
  mutate(Bare_Water=Bare+Water) %>%
  mutate(Leaves = BroadLV + NeedlLV) %>%
  mutate(Open = WOpen + NOpen) %>%
  dplyr::select(-c("Site","Bare","Water","BroadLV","NeedlLV","WOpen","NOpen"))

summarised_500 <- summarised %>%
  semi_join(landcover500,by=c("TOPO")) 

summarised_bombus_500 <- summarised_bombus %>%
  semi_join(landcover500,by=c("TOPO")) 

summarised_NO_bombus_500 <- summarised_NO_bombus %>%
  semi_join(landcover500,by=c("TOPO")) 


landcover1000 <- read.csv("Landcover_1000.csv",header = T, sep = ";")%>%
  mutate(TOPO = Site)%>%
  semi_join(summarised,by=c("TOPO")) %>%
  mutate(Bare_Water=Bare+Water) %>%
  mutate(Leaves = BroadLV + NeedlLV) %>%
  mutate(Open = WOpen + NOpen) %>%
  dplyr::select(-c("Site","Bare","Water","BroadLV","NeedlLV","WOpen","NOpen"))


summarised_1000 <- summarised %>%
  semi_join(landcover1000,by=c("TOPO")) 

summarised_bombus_1000 <- summarised_bombus %>%
  semi_join(landcover1000,by=c("TOPO")) 

summarised_NO_bombus_1000 <- summarised_NO_bombus %>%
  semi_join(landcover1000,by=c("TOPO")) 




################ effect landscape CCA

landcover250_without_topo <-  landcover250 %>%
  dplyr::select(-TOPO)

landcover1000_without_topo <-  landcover1000 %>%
  dplyr::select(-TOPO)

vare.cca <- cca(landcover250_without_topo, summarised_250[,2:3])
vare.cca
plot(vare.cca)

vare.cca <- cca(landcover250_without_topo, summarised_bombus_250[,2:3])
vare.cca
plot(vare.cca)

vare.cca <- cca(landcover250_without_topo, summarised_NO_bombus_250[,2:3])
vare.cca
plot(vare.cca)

vare.cca <- cca(landcover1000_without_topo, summarised_bombus_1000[,2:3])
vare.cca
plot(vare.cca)

vare.cca <- cca(landcover1000_without_topo, summarised_NO_bombus_1000[,2:3])
vare.cca
plot(vare.cca)

# abundance and richness of species is not very affected by landscape


####################### GLM landscape vs bees

summarised_all_250 <- summarised %>%
  left_join(landcover250,by=c("TOPO")) 

summarised_bombus_landscape_250 <- summarised_bombus %>%
  left_join(landcover250,by=c("TOPO")) 

summarised_NO_bombus_250 <- summarised_NO_bombus %>%
  left_join(landcover250,by=c("TOPO")) 


fit <- glm(Richness ~ Open + Plowed + MGramin + Bare_Water + Leaves + COpen, family=poisson, data=summarised_all_250)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Richness ~ Open + Plowed + MGramin + Bare_Water + Leaves + COpen, data=summarised_all_250)
summary(fit)

fit <- glm(Richness ~ Open + Plowed + MGramin + Bare_Water + Leaves + COpen, family=poisson, data=summarised_bombus_landscape_250)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
summary(fit)

fit <- glm(Richness ~ Open + Plowed + MGramin + Bare_Water + Leaves + COpen, family=poisson, data=summarised_NO_bombus_250)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Richness ~ Open + Plowed + MGramin + Bare_Water + Leaves + COpen, data=summarised_NO_bombus_250)
summary(fit)

summarised_bombus_landscape_1000 <- summarised_bombus %>%
  left_join(landcover1000,by=c("TOPO")) 

fit <- glm(Richness ~ Open + Plowed + MGramin + Bare_Water + Leaves + COpen, family=poisson, data=summarised_bombus_landscape_1000)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
summary(fit)


######################### Mantel of landscape composition vs betadiv

data_betadiv <- data_clean %>%
  group_by(TOPO, SPEC.TAXPRIO) %>% 
  dplyr::summarize(Abundance=sum(N)) %>% 
  tidyr::spread(SPEC.TAXPRIO, Abundance) %>%
  ungroup() %>%
  dplyr::select(-TOPO)

data_betadiv[is.na(data_betadiv)] <- 0 # the above generates NA, transform to 0



database1_betadiv_matrix <- as.matrix(database1_betadiv)
database1_betadiv_matrix[database1_betadiv_matrix>0] <- 1 #convert the quantitative matrix into zeros and ones

qualitative_betadiv<-beta.pair(database1_betadiv_matrix, index.family="sor") 


####################here the dataset of composition of landscape

altdist <- dist(altitude_vector$Altitude) #calculate the distance in altitude in metres between sites

mantel(qualitative_betadiv$beta.sor, altdist, method = "pearson", permutations = 9999, na.rm = FALSE)



