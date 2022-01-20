


############### traits bees

setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats/Data correct")

Beetraits <- read.csv("Traits_bees.csv",header = T, sep = ";") %>%
  dplyr::select(-c(Sociality,Nesting_material)) %>%
  mutate(SPEC.TAXPRIO=Species) %>%
  dplyr::select(-Species)

bombus <- data_clean_bombus %>%
  dplyr::filter(SPEC.TAXPRIO != "Andrena propinqua") %>%
  dplyr::group_by(TOPO,SPEC.TAXPRIO) %>%
  dplyr::summarize(Abundance = sum(N))

data_with_habitats_with_traits_richness_bombus <- bombus %>%
  # left_join(landcover250, by = "TOPO") %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  left_join(habitats,by="TOPO") 

data_with_habitats_with_traits_abundance_bombus <- bombus %>%
  # left_join(landcover250, by = "TOPO") %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  left_join(habitats,by="TOPO") %>%
  uncount(Abundance)

NO_bombus <- data_clean_NO_bombus %>%
  dplyr::filter(SPEC.TAXPRIO != "Andrena propinqua") %>%
  dplyr::group_by(TOPO,SPEC.TAXPRIO) %>%
  dplyr::summarize(Abundance = sum(N))

data_with_habitats_with_traits_richness_NO_bombus <- NO_bombus %>%
  # left_join(landcover250, by = "TOPO") %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  left_join(habitats,by="TOPO") 

data_with_habitats_with_traits_abundance_NO_bombus <- NO_bombus %>%
  # left_join(landcover250, by = "TOPO") %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  left_join(habitats,by="TOPO") %>%
  uncount(Abundance)


##### graphics with richness

ggplot(data_with_habitats_with_traits_richness, aes(x=Nest_position)) + 
  geom_bar() + 
  theme_classic() +
  facet_grid(. ~ Habitat)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data_with_habitats_with_traits_richness, aes(x=Lectism,y=Dummy_abundance)) + 
  geom_boxplot() + 
  theme_classic() +
  facet_grid(. ~ Habitat)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
ggplot(data_with_habitats_with_traits_richness, aes(x=Sociality_old,y=Dummy_abundance)) + 
  geom_boxplot() + 
  theme_classic() +
  facet_grid(. ~ Habitat)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data_with_habitats_with_traits_richness, aes(y=Mean_flying_month,y=Abundance)) + 
  geom_boxplot() + 
  theme_classic() +
  facet_grid(. ~ Habitat)

ggplot(data_with_habitats_with_traits_richness, aes(y=ITD,x=Habitat)) + 
  geom_boxplot() + 
  theme_classic() 

ggplot(data_with_habitats_with_traits_richness, aes(x=STI,y=Abundance)) + 
  geom_density() + 
  theme_classic() +
  facet_grid(. ~ Habitat)


##### graphics with abundance

ggplot(data_with_habitats_with_traits_richness, aes(x=Nest_position,y=Abundance)) + 
  geom_boxplot() + 
  theme_classic() +
  facet_grid(. ~ Habitat)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data_with_habitats_with_traits_richness, aes(x=Lectism,y=Abundance)) + 
  geom_boxplot() + 
  theme_classic() +
  facet_grid(. ~ Habitat)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data_with_habitats_with_traits_richness, aes(x=Sociality_old,y=Abundance)) + 
  geom_boxplot() + 
  theme_classic() +
  facet_grid(. ~ Habitat)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data_with_habitats_with_traits_abundance, aes(x=Mean_flying_month)) + 
  geom_bar() + 
  theme_classic() +
  facet_grid(. ~ Habitat)

ggplot(data_with_habitats_with_traits_abundance, aes(x=ITD)) + 
  geom_boxplot() + 
  theme_classic() 

ggplot(data_with_habitats_with_traits_abundance, aes(x=STI,y=Abundance)) + 
  geom_density() + 
  theme_classic() +
  facet_grid(. ~ Habitat)






library("gplots")
library("graphics")
library("vcd")
library(corrplot)

###### chi square test for sociality, nesting, lectism

### Nesting for bombus. Abundance
bombus_chi_abundance <- data_clean_bombus %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::group_by(Habitat,SPEC.TAXPRIO) %>%
  dplyr::summarize(Abundance = sum(N))

data_with_habitats_with_traits_richness_bombus <- bombus_chi_abundance %>%
  # left_join(landcover250, by = "TOPO") %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  dplyr::group_by(Nest_position,Habitat) %>%
  dplyr::summarize(Abundance = sum(Abundance)) %>%
  tidyr::spread(Nest_position, Abundance) 

data_with_habitats_with_traits_richness_bombus2 <- (data_with_habitats_with_traits_richness_bombus[,2:4])
dt <- as.table(as.matrix(data_with_habitats_with_traits_richness_bombus2))
class(dt)
rownames(dt) <- as.matrix(data_with_habitats_with_traits_richness_bombus[,1])

balloonplot((dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot((dt), shade = TRUE, las=2,
           main = "housetasks")

chisq <- chisq.test(t(dt))
chisq

chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)

corrplot(chisq$residuals, is.cor = FALSE)

contrib <- 100*chisq$residuals^2/chisq$statistic


### Nesting for bombus. richness

bombus_chi_richness <- data_clean_bombus %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::mutate(Dummy_abundance=1) %>%
  dplyr::group_by(Habitat,SPEC.TAXPRIO) %>%
  dplyr::summarize(richness = mean(Dummy_abundance))

data_with_habitats_with_traits_richness_bombus <- bombus_chi_richness %>%
  # left_join(landcover250, by = "TOPO") %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  dplyr::group_by(Nest_position,Habitat) %>%
  dplyr::summarize(richness = sum(richness)) %>%
  tidyr::spread(Nest_position, richness) 


data_with_habitats_with_traits_richness_bombus2 <- (data_with_habitats_with_traits_richness_bombus[,2:4])
dt <- as.table(as.matrix(data_with_habitats_with_traits_richness_bombus2))
class(dt)
rownames(dt) <- as.matrix(data_with_habitats_with_traits_richness_bombus[,1])

balloonplot((dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot((dt), shade = TRUE, las=2,
           main = "housetasks")

chisq <- chisq.test(t(dt))
chisq

chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)

corrplot(chisq$residuals, is.cor = FALSE)

contrib <- 100*chisq$residuals^2/chisq$statistic


### Sociality for bombus. Abundance

bombus_chi_abundance <- data_clean_bombus %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::group_by(Habitat,SPEC.TAXPRIO) %>%
  dplyr::summarize(Abundance = sum(N))

data_with_habitats_with_traits_richness_bombus <- bombus_chi_abundance %>%
  # left_join(landcover250, by = "TOPO") %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  dplyr::group_by(Sociality_old,Habitat) %>%
  dplyr::summarize(Abundance = sum(Abundance)) %>%
  tidyr::spread(Sociality_old, Abundance) 


data_with_habitats_with_traits_richness_bombus2 <- (data_with_habitats_with_traits_richness_bombus[,2:3])
dt <- as.table(as.matrix(data_with_habitats_with_traits_richness_bombus2))
class(dt)
rownames(dt) <- as.matrix(data_with_habitats_with_traits_richness_bombus[,1])

balloonplot((dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot((dt), shade = TRUE, las=2,
           main = "housetasks")

chisq <- chisq.test(t(dt))
chisq

chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)

corrplot(chisq$residuals, is.cor = FALSE)

contrib <- 100*chisq$residuals^2/chisq$statistic


### Sociality for bombus. richness

bombus_chi_richness <- data_clean_bombus %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::mutate(Dummy_abundance=1) %>%
  dplyr::group_by(Habitat,SPEC.TAXPRIO) %>%
  dplyr::summarize(richness = mean(Dummy_abundance))

data_with_habitats_with_traits_richness_bombus <- bombus_chi_richness %>%
  # left_join(landcover250, by = "TOPO") %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  dplyr::group_by(Sociality_old,Habitat) %>%
  dplyr::summarize(richness = sum(richness)) %>%
  tidyr::spread(Sociality_old, richness) 


data_with_habitats_with_traits_richness_bombus2 <- (data_with_habitats_with_traits_richness_bombus[,2:3])
dt <- as.table(as.matrix(data_with_habitats_with_traits_richness_bombus2))
class(dt)
rownames(dt) <- as.matrix(data_with_habitats_with_traits_richness_bombus[,1])

balloonplot((dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot((dt), shade = TRUE, las=2,
           main = "housetasks")

chisq <- chisq.test(t(dt))
chisq

chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)

corrplot(chisq$residuals, is.cor = FALSE)

contrib <- 100*chisq$residuals^2/chisq$statistic


### Lectism make no sense because all bombus are polylectic


################### Non bombus

### Nesting for NO bombus. Abundance
NO_bombus_chi_abundance <- data_clean_NO_bombus %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::group_by(Habitat,SPEC.TAXPRIO) %>%
  dplyr::summarize(Abundance = sum(N))

data_with_habitats_with_traits_richness_NO_bombus <- NO_bombus_chi_abundance %>%
  # left_join(landcover250, by = "TOPO") %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  dplyr::group_by(Nest_position,Habitat) %>%
  dplyr::summarize(Abundance = sum(Abundance)) %>%
  tidyr::spread(Nest_position, Abundance) 


data_with_habitats_with_traits_richness_NO_bombus2 <- (data_with_habitats_with_traits_richness_NO_bombus[,2:4])
dt <- as.table(as.matrix(data_with_habitats_with_traits_richness_NO_bombus2))
class(dt)
rownames(dt) <- as.matrix(data_with_habitats_with_traits_richness_NO_bombus[,1])

balloonplot((dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot((dt), shade = TRUE, las=2,
           main = "housetasks")

chisq <- chisq.test(t(dt))
chisq

chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)

corrplot(chisq$residuals, is.cor = FALSE)

contrib <- 100*chisq$residuals^2/chisq$statistic


### Nesting for NO bombus. richness
NO_bombus_chi_richness <- data_clean_NO_bombus  %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::mutate(Dummy_abundance=1) %>%
  dplyr::group_by(Habitat,SPEC.TAXPRIO) %>%
  dplyr::summarize(richness = mean(Dummy_abundance))

data_with_habitats_with_traits_richness_NO_bombus <- NO_bombus_chi_richness %>%
  # left_join(landcover250, by = "TOPO") %>%
  left_join(Beetraits,by="SPEC.TAXPRIO")%>%
  dplyr::group_by(Nest_position,Habitat) %>%
  dplyr::summarize(richness = sum(richness)) %>%
  tidyr::spread(Nest_position, richness) 


data_with_habitats_with_traits_richness_NO_bombus2 <- (data_with_habitats_with_traits_richness_NO_bombus[,2:4])
dt <- as.table(as.matrix(data_with_habitats_with_traits_richness_NO_bombus2))
class(dt)
rownames(dt) <- as.matrix(data_with_habitats_with_traits_richness_NO_bombus[,1])

balloonplot((dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot((dt), shade = TRUE, las=2,
           main = "housetasks")

chisq <- chisq.test(t(dt))
chisq

chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)

corrplot(chisq$residuals, is.cor = FALSE)

contrib <- 100*chisq$residuals^2/chisq$statistic


### Sociality for NO bombus. Abundance

NO_bombus_chi_abundance <- data_clean_NO_bombus %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::group_by(Habitat,SPEC.TAXPRIO) %>%
  dplyr::summarize(Abundance = sum(N))

data_with_habitats_with_traits_richness_NO_bombus <- NO_bombus_chi_abundance %>%
  # left_join(landcover250, by = "TOPO") %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  dplyr::group_by(Sociality_old,Habitat) %>%
  dplyr::summarize(Abundance = sum(Abundance)) %>%
  tidyr::spread(Sociality_old, Abundance) 


data_with_habitats_with_traits_richness_NO_bombus2 <- (data_with_habitats_with_traits_richness_NO_bombus[,2:5])
dt <- as.table(as.matrix(data_with_habitats_with_traits_richness_NO_bombus2))
class(dt)
rownames(dt) <- as.matrix(data_with_habitats_with_traits_richness_NO_bombus[,1])

balloonplot((dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot((dt), shade = TRUE, las=2,
           main = "housetasks")

chisq <- chisq.test(t(dt))
chisq

chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)

corrplot(chisq$residuals, is.cor = FALSE)

contrib <- 100*chisq$residuals^2/chisq$statistic


### Sociality for NO bombus. richness

NO_bombus_chi_richness <- data_clean_NO_bombus %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::mutate(Dummy_abundance=1) %>%
  dplyr::group_by(Habitat,SPEC.TAXPRIO) %>%
  dplyr::summarize(richness = mean(Dummy_abundance))

data_with_habitats_with_traits_richness_NO_bombus <- NO_bombus_chi_richness %>%
  # left_join(landcover250, by = "TOPO") %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  dplyr::group_by(Sociality_old,Habitat) %>%
  dplyr::summarize(richness = sum(richness)) %>%
  tidyr::spread(Sociality_old, richness) 


data_with_habitats_with_traits_richness_NO_bombus2 <- (data_with_habitats_with_traits_richness_NO_bombus[,2:5])
dt <- as.table(as.matrix(data_with_habitats_with_traits_richness_NO_bombus2))
class(dt)
rownames(dt) <- as.matrix(data_with_habitats_with_traits_richness_NO_bombus[,1])

balloonplot((dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot((dt), shade = TRUE, las=2,
           main = "housetasks")

chisq <- chisq.test(t(dt))
chisq

chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)

corrplot(chisq$residuals, is.cor = FALSE)

contrib <- 100*chisq$residuals^2/chisq$statistic


### Lectism for NO bombus. abundance

NO_bombus_chi_abundance <- data_clean_NO_bombus %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::group_by(Habitat,SPEC.TAXPRIO) %>%
  dplyr::summarize(Abundance = sum(N))

data_with_habitats_with_traits_richness_NO_bombus <- NO_bombus_chi_abundance %>%
  # left_join(landcover250, by = "TOPO") %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  dplyr::group_by(Lectism,Habitat) %>%
  dplyr::summarize(Abundance = sum(Abundance)) %>%
  tidyr::spread(Lectism, Abundance) 


data_with_habitats_with_traits_richness_NO_bombus2 <- (data_with_habitats_with_traits_richness_NO_bombus[,2:3])
dt <- as.table(as.matrix(data_with_habitats_with_traits_richness_NO_bombus2))
class(dt)
rownames(dt) <- as.matrix(data_with_habitats_with_traits_richness_NO_bombus[,1])

balloonplot((dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot((dt), shade = TRUE, las=2,
           main = "housetasks")

chisq <- chisq.test(t(dt))
chisq

chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)

corrplot(chisq$residuals, is.cor = FALSE)

contrib <- 100*chisq$residuals^2/chisq$statistic


### Lectism for NO bombus. richness

NO_bombus_chi_richness <- data_clean_NO_bombus %>%
  left_join(habitats,by="TOPO") %>%
  dplyr::mutate(Dummy_abundance=1) %>%
  dplyr::group_by(Habitat,SPEC.TAXPRIO) %>%
  dplyr::summarize(richness = mean(Dummy_abundance))

data_with_habitats_with_traits_richness_NO_bombus <- NO_bombus_chi_richness %>%
  # left_join(landcover250, by = "TOPO") %>%
  left_join(Beetraits,by="SPEC.TAXPRIO") %>%
  dplyr::group_by(Lectism,Habitat) %>%
  dplyr::summarize(richness = sum(richness)) %>%
  tidyr::spread(Lectism, richness) 


data_with_habitats_with_traits_richness_NO_bombus2 <- (data_with_habitats_with_traits_richness_NO_bombus[,2:3])
dt <- as.table(as.matrix(data_with_habitats_with_traits_richness_NO_bombus2))
class(dt)
rownames(dt) <- as.matrix(data_with_habitats_with_traits_richness_NO_bombus[,1])

balloonplot((dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
mosaicplot((dt), shade = TRUE, las=2,
           main = "housetasks")

chisq <- chisq.test(t(dt))
chisq

chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)

corrplot(chisq$residuals, is.cor = FALSE)

contrib <- 100*chisq$residuals^2/chisq$statistic







################### GLMM for numeric variables

### bombus. Abundance
data_with_habitats_with_traits_abundance_bombus$Habitat <- factor(data_with_habitats_with_traits_abundance_bombus$Habitat)
fit <- glmer(ITD~Habitat+(1|TOPO), family=Gamma, data=data_with_habitats_with_traits_abundance_bombus)
hist(resid(fit))
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))


data_with_habitats_with_traits_abundance_bombus$Habitat <- factor(data_with_habitats_with_traits_abundance_bombus$Habitat)
fit <- glmer(STI~Habitat+(1|TOPO), family=Gamma, data=data_with_habitats_with_traits_abundance_bombus)
hist(resid(fit))
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))


data_with_habitats_with_traits_abundance_bombus$Habitat <- factor(data_with_habitats_with_traits_abundance_bombus$Habitat)
fit <- glmer(Mean_flying_month~Habitat+(1|TOPO), family=Gamma, data=data_with_habitats_with_traits_abundance_bombus)
hist(resid(fit))
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))

### bombus. richness
data_with_habitats_with_traits_richness_bombus$Habitat <- factor(data_with_habitats_with_traits_richness_bombus$Habitat)
fit <- glmer(ITD~Habitat+(1|TOPO), family=Gamma, data=data_with_habitats_with_traits_richness_bombus)
hist(resid(fit))
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))


data_with_habitats_with_traits_richness_bombus$Habitat <- factor(data_with_habitats_with_traits_richness_bombus$Habitat)
fit <- glmer(STI~Habitat+(1|TOPO), family=Gamma, data=data_with_habitats_with_traits_richness_bombus)
hist(resid(fit))
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))


data_with_habitats_with_traits_richness_bombus$Habitat <- factor(data_with_habitats_with_traits_richness_bombus$Habitat)
fit <- glmer(Mean_flying_month~Habitat+(1|TOPO), family=Gamma, data=data_with_habitats_with_traits_richness_bombus)
hist(resid(fit))
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))


### NO bombus. Abundance
data_with_habitats_with_traits_abundance_NO_bombus$Habitat <- factor(data_with_habitats_with_traits_abundance_NO_bombus$Habitat)
fit <- glmer(ITD~Habitat+(1|TOPO), family=Gamma, data=data_with_habitats_with_traits_abundance_NO_bombus)
hist(resid(fit))
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))


data_with_habitats_with_traits_abundance_NO_bombus$Habitat <- factor(data_with_habitats_with_traits_abundance_NO_bombus$Habitat)
fit <- glmer(STI~Habitat+(1|TOPO), family=Gamma, data=data_with_habitats_with_traits_abundance_NO_bombus)
hist(resid(fit))
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))


data_with_habitats_with_traits_abundance_NO_bombus$Habitat <- factor(data_with_habitats_with_traits_abundance_NO_bombus$Habitat)
fit <- glmer(Mean_flying_month~Habitat+(1|TOPO), family=Gamma, data=data_with_habitats_with_traits_abundance_NO_bombus)
hist(resid(fit))
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))


### NO bombus. richness
data_with_habitats_with_traits_richness_NO_bombus$Habitat <- factor(data_with_habitats_with_traits_richness_NO_bombus$Habitat)
fit <- glmer(ITD~Habitat+(1|TOPO), family=Gamma, data=data_with_habitats_with_traits_richness_NO_bombus)
hist(resid(fit))
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))


data_with_habitats_with_traits_richness_NO_bombus$Habitat <- factor(data_with_habitats_with_traits_richness_NO_bombus$Habitat)
fit <- glmer(STI~Habitat+(1|TOPO), family=Gamma, data=data_with_habitats_with_traits_richness_NO_bombus)
hist(resid(fit))
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))


data_with_habitats_with_traits_richness_NO_bombus$Habitat <- factor(data_with_habitats_with_traits_richness_NO_bombus$Habitat)
fit <- glmer(Mean_flying_month~Habitat+(1|TOPO), family=Gamma, data=data_with_habitats_with_traits_richness_NO_bombus)
hist(resid(fit))
summary(fit)
summary(glht(fit, mcp(Habitat="Tukey")))


