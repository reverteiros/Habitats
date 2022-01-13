library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

# Datasets

setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats/Data correct")

data_clean

Data_Traits <- read.csv("Traits_bees.csv",header = T, sep = ";")

sites <- read.csv("sites_decided.csv",header = T, sep = ";")%>%
  dplyr::filter(distance=="ok")

habitats <- read.csv("habitats.csv",header = T, sep = ";") %>%
  mutate(TOPO=Official.name)  %>%
  dplyr::select(TOPO,Habitat)%>%
  dplyr::slice(1:112) %>%
  dplyr::inner_join(sites,by="TOPO")

DataHabitat = left_join(data_clean, habitats, by = c("TOPO"))
DataComplete = left_join(DataHabitat, Data_Traits, by = c("SPEC.TAXPRIO"= "Species"))


# Site x Species

SitesEspeces = DataComplete %>% 
  group_by (TOPO, SPEC.TAXPRIO) %>% 
  summarise("Somme" = sum(N)) %>% 
  pivot_wider( names_from = "SPEC.TAXPRIO",values_from = "Somme") %>% 
  replace(is.na(.), 0) %>% 
  #select(order(colnames(.))) %>% 
  relocate(TOPO)


# Indval 

library(labdsv)
tabindval = left_join(SitesEspeces, Data_Sites, by = c("TOPO"= "Official name"))
iva = indval(tabindval[,2:144], tabindval$Habitat) # Indval needs a matrix sites x species and a vector of habitat corresponding to each site.

#hist(iva$indval$Bois)
hist(iva$indval$Carrière)
hist(iva$indval$Parc)
hist(iva$indval$Prairie)
hist(iva$indval$Rudéral)
hist(iva$indval$Terril)

# indval result is a list of objects :
#relfrq = fidelity (number of sites in the group where species is present/number of sites in the group)
#relabu = total abundance in group/grand total abundance (linked to specificity)
#indval = indicator value (IndVal) of each species
#maxcls = cluster where the species has highest IndVal
#indcls = highest IndVal of the species
#pval = permutational p-value of IndVal

pval.adj <- p.adjust(iva$pval, method = "holm") # Correction of p values for multiple testing. Is it necessary ???

# Table of the significant indicator species (no correction)

gr <- iva$maxcls[iva$pval <= 0.05]
iv <- iva$indcls[iva$pval <= 0.05]
pv <- iva$pval[iva$pval <= 0.05]
fr <- apply(tabindval[,2:144] > 0, 2, sum)[iva$pval <= 0.05]
fidg <- data.frame(group = gr,indval = iv,pvalue = pv,freq = fr)
fidg <- fidg[order(fidg$group, -fidg$indval), ]
fidg

# Table of the significant indicator species (with correction)

gr <- iva$maxcls[pval.adj <= 0.05]
iv <- iva$indcls[pval.adj <= 0.05]
pv <- iva$pval[pval.adj <= 0.05]
fr <- apply(tabindval[,2:144] > 0, 2, sum)[pval.adj <= 0.05]
fidg <- data.frame(group = gr,indval = iv,pvalue = pv,freq = fr)
fidg <- fidg[order(fidg$group, -fidg$indval), ]
fidg
