setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats")
source("data cleaning.R")
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats")
source("bees-landscape.R")


############### traits bees

setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats/Data correct")

Beetraits <- read.csv("Traits_bees.csv",header = T, sep = ";") %>%
  select(-c(Nesting_old, Mean_flying_month,Sociality)) %>%
  mutate(SPEC.TAX=Species) %>%
  select(-Species)

data_with_landscape_with_traits <- data_clean_with_landscape %>%
  left_join(Beetraits,by="SPEC.TAX") 


ggplot(data_with_landscape_with_traits, aes(x=Endangered)) + 
  geom_bar(position="stack") + 
  theme_classic() 

ggplot(data_with_landscape_with_traits, aes(x=Nesting_material)) + 
  geom_bar() + 
  theme_classic() 

ggplot(data_with_landscape_with_traits, aes(x=Sociality_old)) + 
  geom_bar() + 
  theme_classic() 

ggplot(data_with_landscape_with_traits, aes(x=Flying_period)) + 
  geom_bar() + 
  theme_classic() 

ggplot(data_with_landscape_with_traits, aes(y=ITD)) + 
  geom_boxplot() + 
  theme_classic() 

ggplot(data_with_landscape_with_traits, aes(y=STI)) + 
  geom_boxplot() + 
  theme_classic() 

ggplot(data_with_landscape_with_traits, aes(y=SCI)) + 
  geom_boxplot() + 
  theme_classic() 



habitats <- read.csv("habitats.csv",header = T, sep = ";") %>%
  mutate(TOPO=Official.name)  %>%
  select(TOPO,Habitat)%>%
  dplyr::slice(1:112)

data_habitats_traits <- data_clean %>%
  left_join(Beetraits,by="SPEC.TAX") %>%
  left_join(habitats,by="TOPO")%>%
#  dplyr::group_by(Habitat,Endangered) %>%
#  dplyr::summarize(Richness=n_distinct(SPEC.TAX),Abundance=sum(N))%>%
  dplyr::group_by(Endangered,SPEC.TAX) %>%
  dplyr::summarize(Habitats_appear=n_distinct(Habitat),Abundance_total = sum(N))%>%
  dplyr::filter(Endangered=="Almost")
  


ggplot(data_habitats_traits, aes(x = Habitat, y = Abundance)) + 
  geom_col() +
  theme_classic() 

ggplot(data_habitats_traits, aes(x = Habitat, y = Richness)) + 
  geom_col() +
  theme_classic() 
