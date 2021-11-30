
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats/Data correct")

library("tidyverse")
library("ggplot2")
library("dplyr")
library(MuMIn)
library(caret)
library(lme4)
library(lmerTest)
library(AER)
library(vegan)
library(SpatialTools)
library(betapart)
library(PERMANOVA)


data_clean
summarised

################ plots abundance-richness per habitat

habitats <- read.csv("habitats.csv",header = T, sep = ";") %>%
  mutate(TOPO=Official.name)  %>%
  dplyr::select(TOPO,Habitat)%>%
  dplyr::slice(1:112)

summarised_with_habitats <- summarised %>%
  left_join(habitats,by="TOPO")

summarised_bombus_with_habitats <- summarised_bombus %>%
  left_join(habitats,by="TOPO")

summarised_NO_bombus_with_habitats <- summarised_NO_bombus %>%
  left_join(habitats,by="TOPO")


ggplot(summarised_with_habitats, aes(x = Habitat, y = Abundance)) + 
  geom_boxplot() +
  theme_classic() 

ggplot(summarised_with_habitats, aes(x = Habitat, y = Richness)) + 
  geom_boxplot() +
  theme_classic() 

ggplot(summarised_bombus_with_habitats, aes(x = Habitat, y = Abundance)) + 
  geom_boxplot() +
  theme_classic() 

ggplot(summarised_bombus_with_habitats, aes(x = Habitat, y = Richness)) + 
  geom_boxplot() +
  theme_classic() 

ggplot(summarised_NO_bombus_with_habitats, aes(x = Habitat, y = Abundance)) + 
  geom_boxplot() +
  theme_classic() 

ggplot(summarised_NO_bombus_with_habitats, aes(x = Habitat, y = Richness)) + 
  geom_boxplot() +
  theme_classic() 





# + phylogenetic diversity
# + originality index






################ comparison between habitats abundance-richness-etc


fit <- glm(Abundance~Habitat, family=poisson, data=summarised_with_habitats)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Abundance~Habitat, data=summarised_with_habitats)
summary(fit)


fit <- glm(Richness~Habitat, family=poisson, data=summarised_with_habitats)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Richness~Habitat, data=summarised_with_habitats)
summary(fit)


fit <- glm(Abundance~Habitat, family=poisson, data=summarised_bombus_with_habitats)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Abundance~Habitat, data=summarised_bombus_with_habitats)
summary(fit)


fit <- glm(Richness~Habitat, family=poisson, data=summarised_bombus_with_habitats)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
summary(fit)


fit <- glm(Abundance~Habitat, family=poisson, data=summarised_NO_bombus_with_habitats)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Abundance~Habitat, data=summarised_NO_bombus_with_habitats)
summary(fit)


fit <- glm(Richness~Habitat, family=poisson, data=summarised_NO_bombus_with_habitats)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(Richness~Habitat, data=summarised_NO_bombus_with_habitats)
summary(fit)


# + posthoc test to compare



################## comparison beta-diversity between habitats

# betadiv

data_betadiv <- data_clean %>%
  group_by(TOPO, SPEC.TAXPRIO) %>% 
  dplyr::summarize(Abundance=sum(N)) 
  


database1_betadiv_with_sites <- data_clean %>%
  group_by(Join_ID, Taxon) %>% 
  dplyr::summarize(Abundance=sum(Abundance)) %>% 
  tidyr::spread(Taxon, Abundance) %>%
  ungroup()

library(vegan3d)
library(vegan)

dis<- vegdist(data_betadiv, method="bray", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = FALSE)

m<-monoMDS(dis, k = 3,  threshold = 0.8, maxit = 200, weakties = TRUE, stress = 1,
           scaling = TRUE, pc = TRUE, smin = 1e-4, sfgrmin = 1e-7,sratmax=0.99999) 
mplot(m)
stressplot(m)

#####couleur 
couleurtype <- select(type,type, couleur)
couleur <- select(couleurtype, couleur)
couleur <- distinct(couleur)

#####NMDS3D
ordiplot3d( lty.hide = 10,ax.col = NA,type = "h",angle = 30,m,pch = 19,display="sites",col=type$couleur, grid = FALSE)
legend("topright", legend = paste('Type', c('Bois', 'Bord de route', 'Carri?re', 'Friche', 'Parc', 'Prairie', 'Terril')), pch = 16, col=couleur$couleur, cex=1, inset=c(0.02), border= NA)




# nmds

