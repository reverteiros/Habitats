
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats/Data correct")

library("tidyverse")
library("ggplot2")
library("dplyr")


############### traits bees

Beetraits <- read.csv("Traits_bees.csv",header = T, sep = ";") %>%
  mutate(SPEC.TAXPRIO=Species)%>%
  dplyr::select(c(SPEC.TAXPRIO,Endangered,Statut_IUCN_Belgium)) 

data_endangered_richness <- data_clean %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::group_by(Habitat,Endangered,SPEC.TAXPRIO,Statut_IUCN_Belgium) %>%
  dplyr::summarize(Abundance=sum(N)) 


a <- ggplot(data_endangered_richness,aes(Habitat,fill=Endangered)) +
  geom_bar(position = "fill")
  
b <- ggplot(data_endangered_richness,aes(Habitat,fill=Endangered))  +
  geom_bar(position = "stack")
  
c <- ggplot(data_endangered_richness,aes(Habitat,fill=Statut_IUCN_Belgium)) +
  geom_bar(position = "fill")

d <- ggplot(data_endangered_richness,aes(Habitat,fill=Statut_IUCN_Belgium)) +
  geom_bar(position = "stack")

ggarrange(a,b,c,d, ncol = 2, nrow = 2,labels = c("(a)", "(b)","(c)","(d)"))



Beetraits <- read.csv("Traits_bees.csv",header = T, sep = ";") %>%
  mutate(SPEC.TAXPRIO=Species)%>%
  dplyr::select(c(SPEC.TAXPRIO,Endangered,Statut_IUCN_Belgium)) 

data_endangered_richness <- data_clean %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::group_by(Endangered) %>%
  dplyr::summarize(richness=n_distinct(SPEC.TAXPRIO),abundance=sum(N)) 



data_endangered_species <- data_clean %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::filter(Endangered=="Yes") %>%
  dplyr::group_by(Statut_IUCN_Belgium,SPEC.TAXPRIO,Habitat) %>%
  dplyr::summarize(abundance=sum(N)) 

write.table(data_endangered_species,"C:\\Users\\535388\\OneDrive - UMONS\\R folders\\Habitats\\data_endangered_species.txt")


data_endangered_habitats <- data_clean %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::filter(Endangered=="Yes") %>%
  dplyr::group_by(Statut_IUCN_Belgium,SPEC.TAXPRIO) %>%
  dplyr::summarize(habitats=n_distinct(Habitat))

write.table(datameasuresflowers,"C:\\Users\\535388\\OneDrive - UMONS\\R folders\\floral_traits\\Table_plant_sp.txt")



###### compare the number of endangered species between habitats

data_endangered_species <- data_clean %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::group_by(Habitat,TOPO,Endangered) %>%
  dplyr::summarize(abundance=sum(N),richness=n_distinct(SPEC.TAXPRIO))%>%
  ungroup %>%
  complete(TOPO,Endangered, fill = list(count = 0)) %>%
  dplyr::filter(Endangered=="Yes") %>%
  mutate_all(~replace(., is.na(.), 0))%>%
  dplyr::select(-Habitat)%>%
  left_join(habitats,by="TOPO") 


# Abundance - all data
data_endangered_species$Habitat <- factor(data_endangered_species$Habitat)
fit <- glm(abundance~Habitat, family=poisson, data=data_endangered_species)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(abundance~Habitat, data=data_endangered_species)
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))

# richness - all data
fit <- glm(richness~Habitat, family=poisson, data=data_endangered_species)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))


a <- ggplot(data_endangered_species,aes(x=Habitat,y=abundance)) +
  geom_boxplot(aes(fill=Habitat)) +
  theme_classic()+ 
  theme(legend.position = "none")

b <- ggplot(data_endangered_species,aes(x=Habitat,y=richness)) +
  geom_boxplot(aes(fill=Habitat)) +
  theme_classic()+ 
  theme(legend.position = "none")

ggarrange(a,b, ncol = 2, nrow = 1,labels = c("(a)", "(b)"))





########## Landscape composition and endangered species. 

#run script 3 up to line 74


vare.cca <- cca(landcover250_without_topo, data_endangered_species[,3:4])
vare.cca
plot(vare.cca)

vare.cca <- cca(landcover1000_without_topo, data_endangered_species[,3:4])
vare.cca
plot(vare.cca)



endangered_250 <- data_endangered_species %>%
  left_join(landcover250,by=c("TOPO")) 

endangered_1000 <- data_endangered_species %>%
  left_join(landcover1000,by=c("TOPO")) 

plot(endangered_250$Agriculture~endangered_250$Forest)


fit <- glm(richness ~ Bare + Artif + Open + COpen + Water + Agriculture, family=poisson, data=endangered_250)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
summary(fit)

fit <- glm(abundance ~ Bare + Artif + Open + COpen + Water + Agriculture, family=poisson, data=endangered_250)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(abundance ~ Bare + Agriculture + Artif + Open + COpen + Water, data=endangered_250)
summary(fit)




fit <- glm(richness ~ Bare + Artif + Open + COpen + Water + Agriculture, family=poisson, data=endangered_1000)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
summary(fit)

fit <- glm(abundance ~ Bare + Artif + Open + COpen + Water + Agriculture, family=poisson, data=endangered_1000)
car::vif(fit)
hist(resid(fit))
dispersiontest(fit,trafo = NULL, alternative = "greater")
fit <- glm.nb(abundance ~ Bare + Agriculture + Artif + Open + COpen + Water, data=endangered_1000)
summary(fit)
