# Datasets

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

# Datasets

setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats/Data correct")


# run first code 1. data cleaning


data_clean_4thcorner <- data_clean %>%
  dplyr::filter(SPEC.TAXPRIO != "Andrena propinqua",SPEC.TAXPRIO != "Nomada facilis",SPEC.TAXPRIO != "Andrena albofasciata")

Sites <- unique(data_clean_4thcorner$TOPO)
Species <- unique(data_clean_4thcorner$SPEC.TAXPRIO)


# Matrix R Environment
landcover250withoutsites <- landcover250 %>%
  dplyr::select(-TOPO)

landcover250withoutsites$Artif <- as.numeric(landcover250withoutsites$Artif)
rownames(landcover250withoutsites) <- Sites



# Matrix L Species and sites

SitesEspeces = data_clean_4thcorner %>% 
  group_by (TOPO, SPEC.TAXPRIO) %>% 
  summarise("Somme" = sum(N)) %>% 
  pivot_wider( names_from = "SPEC.TAXPRIO",values_from = "Somme") %>% 
  replace(is.na(.), 0) %>% 
  dplyr::select(order(colnames(.))) %>% 
  relocate(TOPO)


a =SitesEspeces$TOPO
SitesEspeces=SitesEspeces[,-1]
rownames(SitesEspeces)=a


# Matrix Q Species and traits
Traits <- read.csv("Traits_bees.csv",header = T, sep = ";")
Traits = Traits[Traits$Species%in%Species,]
Traits = Traits[,c("Species","ITD","STI","Nest_position","N_months_active","Mean_flying_month","Sociality_old")]
#Traits = Traits[,c("Species","ITD","STI","N_months_active","Mean_flying_month")]
a =Traits$Species
Traits=Traits[,-1]
rownames(Traits)=a

Traits$Sociality=as.factor(Traits$Sociality)
Traits$Nest_position=as.factor(Traits$Nest_position)

# 4th corner analysis (Legendre's version)

# Reference papers :
# First version of the analysis in this paper (useful to understand the second paper) :
# http://biol09.biol.umontreal.ca/numecol/Reprints/4th-corner_paper.pdf
# current version of the analysis used here (explanation of the different models):
# https://www.researchgate.net/publication/250076886_Testing_the_species_traits_environment_relationships_The_fourth-corner_problem_revisited


#traits<-as.data.frame(apply(X = traits[,c(1:3)],FUN = function(x)as.factor(x), MARGIN = 2))
#rownames(traits)= a


# The code :
library("ade4")
#The fourtrhcorner function is broken. Open and run WorkingFourthCorner

fourth <- fourthcorner(
  tabR = landcover250withoutsites, # Data on landscape (rows= stations, columns = environmental variables)
  tabL = SitesEspeces, # table species (columns) x stations (rows)
  tabQ = Traits, # traits (species = rows, columns = traits)
  modeltype = 6, # kind of permutations of columns/rows applied to tabL. This is the best version (explained in reference paper 2)
  p.adjust.method.G = "none",
  p.adjust.method.D = "none", 
  nrepet = 999) # number of permutations. Should be really high, the higher the better

# Correction for multiple testing, here using FDR
fourth.adj <- p.adjust.4thcorner(
  fourth,
  p.adjust.method.G = "fdr",
  p.adjust.method.D = "fdr",
  p.adjust.D = "global")

# Plot
plot(fourth, alpha = 0.05, stat = "D2")#without correction
plot(fourth.adj, alpha = 0.05, stat = "D2")#with correction

# Three stats can be computed :
# D2 = correlation 
# D = homogeneity of each category (for qualitative variables) 
# G is an  anova like stat for qualitative variables