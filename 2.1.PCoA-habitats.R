
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats/Data correct")

library(vegan)
library(data.table)




sites_esp <- data_clean %>%
  group_by(TOPO, SPEC.TAXPRIO) %>% 
  dplyr::summarize(Abundance=sum(N)) %>% 
  tidyr::spread(SPEC.TAXPRIO, Abundance) %>%
  ungroup() 

sites_esp[is.na(sites_esp)] <- 0 # the above generates NA, transform to 0

sites_esp_df <- as.data.frame(sites_esp)
rownames(sites_esp_df) <- sites_esp_df$TOPO

sites_esp_df_2 <- sites_esp_df %>%
  dplyr::select(-TOPO)


logesp <- log(sites_esp_df_2+1)  # Transformation log  

####fichier = s (sans abondances)
###since I have a lot of singletons it's better to work in presence absence while cotrolling the abundance
###for now, pool all sites

fichier = logesp                                # pour que les codes soient généralisés
label_objets = row.names(fichier)               # pour stocker les labels des objets 
label_variables = names(fichier)                # pour stocker les labels des variables 

# # Calcul d’une matrice de distance de simple conteingence (avec transformation racine) - absence-présence
# s1=designdist(fichier, method ="(a+d)/(a+b+c+d)", abcd= TRUE)
# ds1 = 1 - s1
# 
# # Calcul d’une matrice de distance de Jaccard (avec transformation racine) - absence-présence
# s7 =designdist(fichier, method ="(a)/(a+b+c)", abcd= TRUE)
# ds7 = 1 - s7
# 
# Calcul d’une matrice de distance de Bray-Curtis (avec transformation racine)
d14=sqrt(vegdist(fichier, method="bray"))	    # la transformation racine() permet d’avoir des propriétés métriques

# # Comparaison de DS1 et DS7
# plot(ds1, ds7, xlab="DS1 – Simple contingence - binaire", ylab="DS7 – Jaccard - binaire", col="orange")
# plot(ds1, d14, xlab="DS1 – Simple contingence - binaire", ylab="D14 – Bray-Curtis", col="orange")
# plot(ds7, d14, xlab="DS7 – Jaccard - binaire", ylab="D14 – Bray-Curtis", col="orange")
# 
# 
# # Calcul de deux methods de groupement (UPGMA et Ward) sur les deux matrices
# clust.average.ds7 <- hclust(ds7, method = "average")
# clust.ward.ds7 <- hclust(ds7, method = "ward")
# clust.average.d14 <- hclust(d14, method = "average")
# clust.ward.d14 <- hclust(d14, method = "ward")
# 
# par(mfrow = c(2, 2)) # plot multi-panneaux
# 
# plot(clust.average.ds7,
#      cex=0.7,                                  # pour modifier la taille des labels
#      main = "DS7 - groupement UPGMA ",          # titre principal
#      hang = -1,                                # étire les branches jusqu'en bas
# )
# plot(clust.ward.ds7, cex=0.7, main = "DS7 - groupement WARD ",  hang = -1)
# plot(clust.average.d14, cex=0.7, main = "D14 - groupement UPGMA ", hang = -1)
# plot(clust.ward.d14, cex=0.7, main = "D14 - groupement WARD ", hang = -1)
# 
# par(mfrow = c(1, 1)) # plot multi-panneaux
# 
# # groupement le plus convaincant
# 
# plot(clust.ward.d14, cex=0.7, main = "D14 - groupement WARD ",  hang = -1)
# rect.hclust(clust.ward.d14, 5)
# clust.ward.d14.5gr <- cutree(clust.ward.d14, 5)  

# Lancement de la PCoA 
pcoa_resu <- cmdscale(d14,
                      k=6,        # nombre d’axes retenus
                      add= TRUE,  # pour éviter les valeurs propres négatives
                      eig= TRUE)  # pour obtenir les valeurs propres

# contrairement à l’ACP (rda()) et à l’AFC (cca()), il n’y a pas de rapport résumé pour la PcoA.

# Calcul des valeurs propres
valeurs_propres = round(pcoa_resu$eig/sum(pcoa_resu$eig)*100, digits = 2)
valeurs_propres

# Histogramme des valeurs propres
barplot(valeurs_propres, main="Valeurs propres", col = "bisque", las=2)
abline(h=mean(valeurs_propres), col="red")	     # moyenne des valeurs propres
legend("topright", 
       "Moyenne des valeurs propres", 
       lwd=1, 
       col=2, 
       bty="n")

# graphique de base
# ordiplot(pcoa_resu)

# graphique amélioré pour les axes 1 et 2
# Label des axes avec les valeurs propres automatiques 
pcoa1label = paste('PCoA 1 : ', round(valeurs_propres[1],2), '%', sep = "")
pcoa2label = paste('PCoA 2 : ', round(valeurs_propres[2],2), '%', sep = "")

ordiplot(pcoa_resu, type="n", xlab =pcoa1label, ylab= pcoa2label )                    # graphique vide
abline(h=0, lty=3)			# axe horizontal (lty = type de la ligne)
abline(v=0, lty=3)			# axe vertical  (lty = type de la ligne)

# on définit des couleurs pour les groupes de stations
mescouleurs<- c("dodgerblue", "darkorchid", "gold", "chocolate3", "darkorange", "lightslategray", "olivedrab", "saddlebrown") 
palette(mescouleurs) 



############# Include the habitat as a grouping factor

habitats <- read.csv("habitats.csv",header = T, sep = ";") %>%
  mutate(TOPO=Official.name)  %>%
  dplyr::select(TOPO,Habitat,Number_category)%>%
  dplyr::slice(1:112) %>%
  dplyr::inner_join(sites,by="TOPO") %>%
  dplyr::select(-c(Lati,Long,why,distance,Habitat))

habitats_df <- as.data.frame(habitats)
rownames(habitats_df) <- habitats_df$TOPO

habitats_df_2 <- habitats_df %>%
  dplyr::select(-TOPO)

habitats_df_2$Number_category <- as.integer(habitats_df_2$Number_category)


######## Graph PCoA

pcoa1 = pcoa_resu$points[,1]
pcoa2 = pcoa_resu$points[,2]
points(pcoa1, pcoa2, col = habitats_df_2$Number_category, pch = 16, cex=1)


####### Site labels

# position à gauche ou à droite du symbole en fonction de la coordonnée
position <- ifelse( pcoa1 < 0, 2, 4)     
text(pcoa1, pcoa2, labels= label_objets, col="olivedrab", cex=0.9, pos=position)

# Lecture de données sites
sites=read.table('Sites.txt', h=T, sep="\t", row.names="Code_site")
attach(sites)
names(sites)
# "Wooded" "Altitude" "Alti_Category" "Y" "X" "XY"   





