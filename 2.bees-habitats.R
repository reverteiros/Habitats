
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats/Data correct")

library("tidyverse")
library("ggplot2")
library("dplyr")
library(MuMIn)
library(caret)
library(lme4)
library(lmerTest)
library(AER)
library(vegan3d)
library(vegan)
library(SpatialTools)
library(betapart)
library(PERMANOVA)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggExtra)
library(grid)
library(multcomp)
library(ape)
library(rgdal)
library(ggmap)
library(gstat)



# run first code 1. data cleaning


################ plots abundance-richness per habitat

sites <- read.csv("sites_decided.csv",header = T, sep = ";")%>%
  dplyr::filter(distance=="ok")


habitats <- read.csv("habitats.csv",header = T, sep = ";") %>%
  mutate(TOPO=Official.name)  %>%
  dplyr::select(TOPO,Habitat)%>%
  dplyr::slice(1:112) %>%
  dplyr::inner_join(sites,by="TOPO")

habitats2 <- read.csv("habitats.csv",header = T, sep = ";") %>%
  mutate(TOPO=Official.name)  %>%
  dplyr::select(TOPO,Habitat)%>%
  dplyr::slice(1:112) %>%
  dplyr::inner_join(sites,by="TOPO")%>%
  dplyr::group_by(Habitat)%>%
  dplyr::summarize(sites=n_distinct(TOPO)) 



summarised_with_habitats <- summarised %>%
  inner_join(habitats,by="TOPO")

summarised_bombus_with_habitats <- summarised_bombus %>%
  inner_join(habitats,by="TOPO")

summarised_NO_bombus_with_habitats <- summarised_NO_bombus %>%
  inner_join(habitats,by="TOPO")

################## spatial autocorrelation

habitats3 <- read.csv("habitats.csv",header = T, sep = ";") %>%
  mutate(TOPO=Official.name)  %>%
  dplyr::slice(1:112) %>%
  dplyr::inner_join(sites,by="TOPO") %>%
  dplyr::mutate(Y=Lati) %>%
  dplyr::mutate(X=Long) 
  

summarised_with_habitats3 <- summarised %>%
  inner_join(habitats3,by="TOPO")

zone.dists <- as.matrix(dist(cbind(summarised_with_habitats3$X, summarised_with_habitats3$Y)))
zone.dists.inv <- 1/zone.dists
diag(zone.dists.inv) <- 0

Moran.I(summarised_with_habitats3$Richness, zone.dists.inv)#
Moran.I(summarised_with_habitats3$Abundance, zone.dists.inv)#

coords <- SpatialPoints(summarised_with_habitats3[, c("X", "Y")], proj4string = CRS("+proj=longlat"))
plots <- SpatialPointsDataFrame(coords, summarised_with_habitats3)
ddll <- spTransform(plots, CRS("+proj=longlat"))
pts <- as.data.frame(coordinates(ddll))
names(pts) <- c("lon", "lat")

print(bubble(plots, "Richness", maxsize = 5,key.entries = 20*(1:5),col="blue"))
print(bubble(plots, "Abundance", maxsize = 5,key.entries = 20*(1:5),col="blue"))


summarised_bombus_with_habitats3 <- summarised_bombus %>%
  inner_join(habitats3,by="TOPO")

zone.dists <- as.matrix(dist(cbind(summarised_bombus_with_habitats3$X, summarised_bombus_with_habitats3$Y)))
zone.dists.inv <- 1/zone.dists
diag(zone.dists.inv) <- 0

Moran.I(summarised_bombus_with_habitats3$Richness, zone.dists.inv)#
Moran.I(summarised_bombus_with_habitats3$Abundance, zone.dists.inv)

coords <- SpatialPoints(summarised_bombus_with_habitats3[, c("X", "Y")], proj4string = CRS("+proj=longlat"))
plots <- SpatialPointsDataFrame(coords, summarised_bombus_with_habitats3)
ddll <- spTransform(plots, CRS("+proj=longlat"))
pts <- as.data.frame(coordinates(ddll))
names(pts) <- c("lon", "lat")

print(bubble(plots, "Richness", maxsize = 5,key.entries = 5*(1:5),col="blue"))
print(bubble(plots, "Abundance", maxsize = 5,key.entries = 20*(1:5),col="blue"))


summarised_NO_bombus_with_habitats3 <- summarised_NO_bombus %>%
  inner_join(habitats3,by="TOPO")

zone.dists <- as.matrix(dist(cbind(summarised_NO_bombus_with_habitats3$X, summarised_NO_bombus_with_habitats3$Y)))
zone.dists.inv <- 1/zone.dists
diag(zone.dists.inv) <- 0

Moran.I(summarised_NO_bombus_with_habitats3$Richness, zone.dists.inv)#
Moran.I(summarised_NO_bombus_with_habitats3$Abundance, zone.dists.inv)#

coords <- SpatialPoints(summarised_NO_bombus_with_habitats3[, c("X", "Y")], proj4string = CRS("+proj=longlat"))
plots <- SpatialPointsDataFrame(coords, summarised_NO_bombus_with_habitats3)
ddll <- spTransform(plots, CRS("+proj=longlat"))
pts <- as.data.frame(coordinates(ddll))
names(pts) <- c("lon", "lat")

print(bubble(plots, "Richness", maxsize = 5,key.entries = 10*(1:5),col="blue"))
print(bubble(plots, "Abundance", maxsize = 5,key.entries = 20*(1:5),col="blue"))



colours <- c("blue","red","green","yellow","orange")
ggplot(summarised_with_habitats, aes(x = X, y = Y)) + 
  geom_point(aes(colour=Habitat,size=4)) +
  scale_color_manual(values=colours)+
  theme_classic() 

#########################################################



a <- ggplot(summarised_with_habitats, aes(x = Habitat, y = Abundance)) + 
  geom_boxplot(colour=) +
  theme_classic() +
  labs(x = "")+
  labs(title ="Entire community")

b <- ggplot(summarised_bombus_with_habitats, aes(x = Habitat, y = Abundance)) + 
  geom_boxplot() +
  theme_classic() +
  labs(y = "")+
  labs(x = "")+
  labs(title ="Only Bombus")

c <- ggplot(summarised_NO_bombus_with_habitats, aes(x = Habitat, y = Abundance)) + 
  geom_boxplot() +
  theme_classic() +
  labs(y = "")+
  labs(x = "")+
  labs(title ="Excluding Bombus")

d <- ggplot(summarised_with_habitats, aes(x = Habitat, y = Richness)) + 
  geom_boxplot() +
  theme_classic() 

e <- ggplot(summarised_bombus_with_habitats, aes(x = Habitat, y = Richness)) + 
  geom_boxplot() +
  theme_classic() +
  labs(y = "")


f <- ggplot(summarised_NO_bombus_with_habitats, aes(x = Habitat, y = Richness)) + 
  geom_boxplot() +
  theme_classic()+
  labs(y = "")


ggarrange(a,b,c,d,e,f, ncol = 3, nrow = 2,labels = c("(a)", "(b)","(c)","(d)","(e)","(f)"))



################ comparison between habitats abundance-richness-etc

# Abundance - all data
summarised_with_habitats$Habitat <- factor(summarised_with_habitats$Habitat)
fit <- glm(Abundance~Habitat, family=poisson, data=summarised_with_habitats)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Abundance~Habitat, data=summarised_with_habitats)
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))

# richness - all data
fit <- glm(Richness~Habitat, family=poisson, data=summarised_with_habitats)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Richness~Habitat, data=summarised_with_habitats)
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))

# abundance - bombus
summarised_bombus_with_habitats$Habitat <- factor(summarised_bombus_with_habitats$Habitat)
fit <- glm(Abundance~Habitat, family=poisson, data=summarised_bombus_with_habitats)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Abundance~Habitat, data=summarised_bombus_with_habitats)
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))

# richness - bombus
fit <- glm(Richness~Habitat, family=poisson, data=summarised_bombus_with_habitats)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))

# abundance - data without bombus
summarised_NO_bombus_with_habitats$Habitat <- factor(summarised_NO_bombus_with_habitats$Habitat)
fit <- glm(Abundance~Habitat, family=poisson, data=summarised_NO_bombus_with_habitats)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Abundance~Habitat, data=summarised_NO_bombus_with_habitats)
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))

# richness - data without bombus
summarised_bombus_with_habitats$Habitat <- factor(summarised_bombus_with_habitats$Habitat)
fit <- glm(Richness~Habitat, family=poisson, data=summarised_NO_bombus_with_habitats)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Richness~Habitat, data=summarised_NO_bombus_with_habitats)
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))


################## comparison beta-diversity between habitats


# NMDS. Stress is 0.28, so not super useful

data_betadiv <- data_clean %>%
  group_by(TOPO, SPEC.TAXPRIO) %>% 
  dplyr::summarize(Abundance=sum(N)) %>% 
  tidyr::spread(SPEC.TAXPRIO, Abundance) %>%
  ungroup() %>%
  dplyr::select(-TOPO)

data_betadiv[is.na(data_betadiv)] <- 0 # the above generates NA, transform to 0


example_NMDS=metaMDS(data_betadiv,k=2,trymax=100)
stressplot(example_NMDS)
plot(example_NMDS)
ordiplot(example_NMDS,type="n")
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",cex=1.25,air=0.01)

treat=summarised_bombus_with_habitats$Habitat
ordiplot(example_NMDS,type="n")
ordihull(example_NMDS,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=1.25)
