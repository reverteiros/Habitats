
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats/Data correct")

library(AER)
library(readxl)
library(MASS)
# run first code 1. data cleaning


################## Landscape

landcover250 <- read_excel("Landcover.xlsx", sheet = "250m")%>%
  mutate(TOPO = Site)%>%
  semi_join(summarised,by=c("TOPO")) %>%
  mutate(Agriculture=Plowed+MGramin) %>%
  mutate(Forest = BroadLV + NeedlLV) %>%
  mutate(Open = WOpen + NOpen) %>%
  dplyr::select(-c("Site","Plowed","MGramin","BroadLV","NeedlLV","WOpen","NOpen"))

summarised_250 <- summarised %>%
  semi_join(landcover250,by=c("TOPO")) 

summarised_bombus_250 <- summarised_bombus %>%
  semi_join(landcover250,by=c("TOPO")) 

summarised_NO_bombus_250 <- summarised_NO_bombus %>%
  semi_join(landcover250,by=c("TOPO")) 


landcover500 <- read_excel("Landcover.xlsx", sheet = "500m")%>%
  mutate(TOPO = Site)%>%
  semi_join(summarised,by=c("TOPO")) %>%
  mutate(Agriculture=Plowed+MGramin) %>%
  mutate(Forest = BroadLV + NeedlLV) %>%
  mutate(Open = WOpen + NOpen) %>%
  dplyr::select(-c("Site","Plowed","MGramin","BroadLV","NeedlLV","WOpen","NOpen"))

summarised_500 <- summarised %>%
  semi_join(landcover500,by=c("TOPO")) 

summarised_bombus_500 <- summarised_bombus %>%
  semi_join(landcover500,by=c("TOPO")) 

summarised_NO_bombus_500 <- summarised_NO_bombus %>%
  semi_join(landcover500,by=c("TOPO")) 


landcover1000 <- read_excel("Landcover.xlsx", sheet = "1000m")%>%
  mutate(TOPO = Site)%>%
  semi_join(summarised,by=c("TOPO")) %>%
  mutate(Agriculture=Plowed+MGramin) %>%
  mutate(Forest = BroadLV + NeedlLV) %>%
  mutate(Open = WOpen + NOpen) %>%
  dplyr::select(-c("Site","Plowed","MGramin","BroadLV","NeedlLV","WOpen","NOpen"))


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


hist(summarised_all_250$Agriculture)
hist(summarised_all_250$Forest)

fit <- glm(Richness ~ Bare + Agriculture + Artif + Open + COpen + Water, family=poisson, data=summarised_all_250)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Richness ~ Bare + Agriculture + Artif + Open + COpen + Water, data=summarised_all_250)
summary(fit)


fit <- glm(Abundance ~ Bare + Agriculture + Artif + Open + COpen + Water, family=poisson, data=summarised_all_250)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Abundance ~ Bare + Agriculture + Artif + Open + COpen + Water, data=summarised_all_250)
summary(fit)


fit <- glm(Richness ~ Bare + Agriculture + Artif + Open + COpen + Water, family=poisson, data=summarised_bombus_landscape_250)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
summary(fit)

fit <- glm(Abundance ~ Bare + Agriculture + Artif + Open + COpen + Water, family=poisson, data=summarised_bombus_landscape_250)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Abundance ~ Bare + Agriculture + Artif + Open + COpen + Water, data=summarised_bombus_landscape_250)
summary(fit)


fit <- glm(Richness ~ Bare + Agriculture + Artif + Open + COpen + Water, family=poisson, data=summarised_NO_bombus_250)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Richness ~ Bare + Agriculture + Artif + Open + COpen + Water, data=summarised_NO_bombus_250)
summary(fit)

fit <- glm(Abundance ~ Bare + Agriculture + Artif + Open + COpen + Water, family=poisson, data=summarised_NO_bombus_250)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Abundance ~ Bare + Agriculture + Artif + Open + COpen + Water, data=summarised_NO_bombus_250)
summary(fit)


summarised_all_landscape_1000 <- summarised %>%
  left_join(landcover1000,by=c("TOPO")) 

summarised_bombus_landscape_1000 <- summarised_bombus %>%
  left_join(landcover250,by=c("TOPO")) 

summarised_NO_bombus_landscape_1000 <- summarised_NO_bombus %>%
  left_join(landcover250,by=c("TOPO"))  

fit <- glm(Richness ~ Bare + Agriculture + Artif + Open + COpen + Water, family=poisson, data=summarised_all_landscape_1000)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Richness ~ Bare + Agriculture + Artif + Open + COpen + Water, data=summarised_all_landscape_1000)
summary(fit)

fit <- glm(Abundance ~ Bare + Agriculture + Artif + Open + COpen + Water, family=poisson, data=summarised_all_landscape_1000)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Abundance ~ Bare + Agriculture + Artif + Open + COpen + Water, data=summarised_all_landscape_1000)
summary(fit)


fit <- glm(Richness ~ Bare + Agriculture + Artif + Open + COpen + Water, family=poisson, data=summarised_bombus_landscape_1000)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
summary(fit)

fit <- glm(Abundance ~ Bare + Agriculture + Artif + Open + COpen + Water, family=poisson, data=summarised_bombus_landscape_1000)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Abundance ~ Bare + Agriculture + Artif + Open + COpen + Water, data=summarised_bombus_landscape_1000)
summary(fit)


fit <- glm(Richness ~ Bare + Agriculture + Artif + Open + COpen + Water, family=poisson, data=summarised_NO_bombus_landscape_1000)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Richness ~ Bare + Agriculture + Artif + Open + COpen + Water, data=summarised_NO_bombus_landscape_1000)
summary(fit)

fit <- glm(Abundance ~ Bare + Agriculture + Artif + Open + COpen + Water, family=poisson, data=summarised_NO_bombus_landscape_1000)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Abundance ~ Bare + Agriculture + Artif + Open + COpen + Water, data=summarised_NO_bombus_landscape_1000)
summary(fit)


######################### Mantel of landscape composition vs betadiv


######## All data - 250m radius
data_betadiv <- data_clean %>%
  group_by(TOPO, SPEC.TAXPRIO) %>% 
  dplyr::summarize(Abundance=sum(N)) %>% 
  tidyr::spread(SPEC.TAXPRIO, Abundance) %>%
  ungroup() %>%
  dplyr::select(-TOPO)

data_betadiv[is.na(data_betadiv)] <- 0 # the above generates NA, transform to 0
database1_betadiv_matrix <- as.matrix(data_betadiv)

qualitative_betadiv<-bray.part(database1_betadiv_matrix) 

landcover250_topoless <- landcover250 %>%
  dplyr::select(-c("TOPO"))
landscapedist <- dist(landcover250_topoless) 

mantel(qualitative_betadiv$bray, landscapedist, method = "pearson", permutations = 9999, na.rm = FALSE)


######## All data - 1000m radius
data_betadiv <- data_clean %>%
  group_by(TOPO, SPEC.TAXPRIO) %>% 
  dplyr::summarize(Abundance=sum(N)) %>% 
  tidyr::spread(SPEC.TAXPRIO, Abundance) %>%
  ungroup() %>%
  dplyr::select(-TOPO)

data_betadiv[is.na(data_betadiv)] <- 0 # the above generates NA, transform to 0
database1_betadiv_matrix <- as.matrix(data_betadiv)

qualitative_betadiv<-bray.part(database1_betadiv_matrix) 

landcover250_topoless <- landcover1000 %>%
  dplyr::select(-c("TOPO"))
landscapedist <- dist(landcover250_topoless) 

mantel(qualitative_betadiv$bray, landscapedist, method = "pearson", permutations = 9999, na.rm = FALSE)


######## bombus - 250m radius
data_betadiv <- data_clean_bombus %>%
  group_by(TOPO, SPEC.TAXPRIO) %>% 
  dplyr::summarize(Abundance=sum(N)) %>% 
  tidyr::spread(SPEC.TAXPRIO, Abundance) %>%
  ungroup() %>%
  dplyr::select(-TOPO)

data_betadiv[is.na(data_betadiv)] <- 0 # the above generates NA, transform to 0
database1_betadiv_matrix <- as.matrix(data_betadiv)

qualitative_betadiv<-bray.part(database1_betadiv_matrix) 

landcover250_topoless <- landcover250 %>%
  dplyr::select(-c("TOPO"))
landscapedist <- dist(landcover250_topoless) 

mantel(qualitative_betadiv$bray, landscapedist, method = "pearson", permutations = 9999, na.rm = FALSE)


######## bombus - 1000m radius
data_betadiv <- data_clean_bombus %>%
  group_by(TOPO, SPEC.TAXPRIO) %>% 
  dplyr::summarize(Abundance=sum(N)) %>% 
  tidyr::spread(SPEC.TAXPRIO, Abundance) %>%
  ungroup() %>%
  dplyr::select(-TOPO)

data_betadiv[is.na(data_betadiv)] <- 0 # the above generates NA, transform to 0
database1_betadiv_matrix <- as.matrix(data_betadiv)

qualitative_betadiv<-bray.part(database1_betadiv_matrix) 

landcover250_topoless <- landcover1000 %>%
  dplyr::select(-c("TOPO"))
landscapedist <- dist(landcover250_topoless) 

mantel(qualitative_betadiv$bray, landscapedist, method = "pearson", permutations = 9999, na.rm = FALSE)


######## no-bombus - 250m radius
data_betadiv <- data_clean_NO_bombus %>%
  group_by(TOPO, SPEC.TAXPRIO) %>% 
  dplyr::summarize(Abundance=sum(N)) %>% 
  tidyr::spread(SPEC.TAXPRIO, Abundance) %>%
  ungroup() %>%
  dplyr::select(-TOPO)

data_betadiv[is.na(data_betadiv)] <- 0 # the above generates NA, transform to 0
database1_betadiv_matrix <- as.matrix(data_betadiv)

qualitative_betadiv<-bray.part(database1_betadiv_matrix) 

landcover250_topoless <- landcover250 %>%
  dplyr::select(-c("TOPO"))
landscapedist <- dist(landcover250_topoless) 

mantel(qualitative_betadiv$bray, landscapedist, method = "pearson", permutations = 9999, na.rm = FALSE)


######## no-bombus - 1000m radius
data_betadiv <- data_clean_NO_bombus %>%
  group_by(TOPO, SPEC.TAXPRIO) %>% 
  dplyr::summarize(Abundance=sum(N)) %>% 
  tidyr::spread(SPEC.TAXPRIO, Abundance) %>%
  ungroup() %>%
  dplyr::select(-TOPO)

data_betadiv[is.na(data_betadiv)] <- 0 # the above generates NA, transform to 0
database1_betadiv_matrix <- as.matrix(data_betadiv)

qualitative_betadiv<-bray.part(database1_betadiv_matrix) 

landcover250_topoless <- landcover1000 %>%
  dplyr::select(-c("TOPO"))
landscapedist <- dist(landcover250_topoless) 

mantel(qualitative_betadiv$bray, landscapedist, method = "pearson", permutations = 9999, na.rm = FALSE)


