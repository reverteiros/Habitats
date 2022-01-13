
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Habitats/Data correct")

library(vegan)
library(data.table)


esp_sites <- read.table('Mira_Esp.txt', h=T, sep="\t", row.names="Tax_code")
sites_esp <- transpose(esp_sites)

#redefine row and column names
rownames(sites_esp) <- colnames(esp_sites)
colnames(sites_esp) <- rownames(esp_sites)

logesp <- log(sites_esp+1)  # Transformation log  

####fichier = s (sans abondances)
###since I have a lot of singletons it's better to work in presence absence while cotrolling the abundance
###for now, pool all sites

fichier = logesp                                # pour que les codes soient généralisés
label_objets = row.names(fichier)               # pour stocker les labels des objets 
label_variables = names(fichier)                # pour stocker les labels des variables 

# Calcul d’une matrice de distance de simple conteingence (avec transformation racine) - absence-présence
s1=designdist(fichier, method ="(a+d)/(a+b+c+d)", abcd= TRUE)
ds1 = 1 - s1

# Calcul d’une matrice de distance de Jaccard (avec transformation racine) - absence-présence
s7 =designdist(fichier, method ="(a)/(a+b+c)", abcd= TRUE)
ds7 = 1 - s7

# Calcul d’une matrice de distance de Bray-Curtis (avec transformation racine)
d14=sqrt(vegdist(fichier, method="bray"))	    # la transformation racine() permet d’avoir des propriétés métriques

# Comparaison de DS1 et DS7
plot(ds1, ds7, xlab="DS1 – Simple contingence - binaire", ylab="DS7 – Jaccard - binaire", col="orange")
plot(ds1, d14, xlab="DS1 – Simple contingence - binaire", ylab="D14 – Bray-Curtis", col="orange")
plot(ds7, d14, xlab="DS7 – Jaccard - binaire", ylab="D14 – Bray-Curtis", col="orange")


# Calcul de deux methods de groupement (UPGMA et Ward) sur les deux matrices
clust.average.ds7 <- hclust(ds7, method = "average")
clust.ward.ds7 <- hclust(ds7, method = "ward")
clust.average.d14 <- hclust(d14, method = "average")
clust.ward.d14 <- hclust(d14, method = "ward")

par(mfrow = c(2, 2)) # plot multi-panneaux

plot(clust.average.ds7,
      cex=0.7,                                  # pour modifier la taille des labels
      main = "DS7 - groupement UPGMA ",          # titre principal
      hang = -1,                                # étire les branches jusqu'en bas
)
plot(clust.ward.ds7, cex=0.7, main = "DS7 - groupement WARD ",  hang = -1)
plot(clust.average.d14, cex=0.7, main = "D14 - groupement UPGMA ", hang = -1)
plot(clust.ward.d14, cex=0.7, main = "D14 - groupement WARD ", hang = -1)

par(mfrow = c(1, 1)) # plot multi-panneaux

# groupement le plus convaincant

plot(clust.ward.d14, cex=0.7, main = "D14 - groupement WARD ",  hang = -1)
rect.hclust(clust.ward.d14, 5)
clust.ward.d14.5gr <- cutree(clust.ward.d14, 5)  

# Lancement de la PCoA 
pcoa_resu <- cmdscale(d14,
k=6,                                            # nombre d’axes retenus
add= TRUE,                                      # pour éviter les valeurs propres négatives
eig= TRUE)                                      # pour obtenir les valeurs propres

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

# on dessine les symboles colorés
pcoa1 = pcoa_resu$points[,1]
pcoa2 = pcoa_resu$points[,2]
points(pcoa1, pcoa2, col = clust.ward.d14.5gr, pch = 16, cex=1)
# on ajoute les labels des sites
# position à gauche ou à droite du symbole en fonction de la coordonnée
position <- ifelse( pcoa1 < 0, 2, 4)     
text(pcoa1, pcoa2, labels= label_objets, col="olivedrab", cex=0.9, pos=position)

# Lecture de données sites
sites=read.table('Sites.txt', h=T, sep="\t", row.names="Code_site")
attach(sites)
names(sites)
# "Wooded" "Altitude" "Alti_Category" "Y" "X" "XY"   










# ecriture d’une nouvelle fonction pour éviter de répéter les ordres
histo<-function(vareco)
{
hist(vareco, breaks=20, prob=TRUE, xlab= substitute(vareco), col="lightgreen", main = "");
curve(dnorm(x, mean=mean(vareco), sd=sd(vareco)), col="darkblue", lwd=2, add=TRUE);
}

par(mfrow = c(2, 2)) # plot multi-panneaux
histo(Altitude);histo(X);histo(Y);histo(XY)
par(mfrow = c(1, 1)) # plot multi-panneaux

# Matrice de corrélation
correlation = cor(sites)
#View(correlation)

#install.packages("corrplot")
library(corrplot)
corrplot(correlation, method="circle")


# Cercle des correlations
# ===================
# On recupere les coordonnees des stations (lignes) sur les axes Dim1 et Dim2
coordolig = data.frame(scores(pcoa_resu))
# calcul de la correlation
correlation = data.frame(cor(sites, coordolig)) 
correlation

# representation du cercle des correlations
a <- seq(0,2*pi,length=100)
plot( cos(a), sin(a),
      type = 'l', lty = 3,
      xlab = 'PCoA1', ylab = 'PCoA2',
      main = "Correlation circle")
arrows(0,0, correlation$Dim1, correlation$Dim2, col='red')
text(correlation$Dim1, correlation$Dim2,rownames(correlation), col='blue')
abline(h=0, lty=3) # axe horizontal (lty = type de la ligne)
abline(v=0, lty=3) # axe vertical (lty = type de la ligne)

# Fichier explicatif a deux variables

sites_alt = data.frame(Altitude, Wooded, row.names = row.names(sites))

# calcul de la correlation
correlation = data.frame(cor(sites_alt, coordolig)) 
correlation

# representation du cercle des correlations
a <- seq(0,2*pi,length=100)
plot( cos(a), sin(a),
      type = 'l', lty = 3,
      xlab = 'PCoA1', ylab = 'PCoA2',
      main = "Correlation circle")
arrows(0,0, correlation$Dim1, correlation$Dim2, col='red')
text(correlation$Dim1, correlation$Dim2,rownames(correlation), col='blue')
abline(h=0, lty=3) # axe horizontal (lty = type de la ligne)
abline(v=0, lty=3) # axe vertical (lty = type de la ligne)

# realise une CAP sur le fichier especes et le fichier variables écologiques
# ==========================================================================

# On realise une CAP wavec altitude et wooded
# ================

# CAP sur les abondances 
cap=capscale(fichier ~ Altitude+Wooded, data=sites_alt,distance="bray", sqrt.dist=T, add=T, dfun=vegdist)
summary(cap)

# Calcul des valeurs propres
valeurs_propres = round((cap$CCA$eig/(sum(cap$CCA$eig)+sum(cap$CA$eig))*100), digits = 2)


# Label des axes avec les valeurs propres automatiques 
cap1label = paste('CAP 1 : ', round(valeurs_propres[1],2), '%', sep = "")
cap2label = paste('CAP 2 : ', round(valeurs_propres[2],2), '%', sep = "")

# on définit des couleurs pour les groupes de stations


# Graphique
#colvec <- c("dodgerblue", "darkorchid", "gold", "chocolate3", "darkorange", "lightslategray", "olivedrab", "saddlebrown") 
figure=ordiplot(cap, scaling=2, xlab=cap1label, ylab=cap2label, main="CAP")
points(figure, "sites", pch=16, col=clust.ward.d14.5gr, cex=1.5)
text(figure, "sites", 
     labels = rownames(fichier),        # label de stations sur le graphique
     col = clust.ward.d14.5gr,    # couleur du label (peut être un vecteur)
     pos = 3,                       # position du label
     cex = 0.6)                     # taille du label

# Le modèle complet est-il significatif ? 
anova(cap, perm=999)

# Les axes contraints sont-il significatifs ? 
anova(cap, by="axis", perm=999)

# Role marginal significatif des variables (compte tenu des autres) ?
anova(cap, by="margin", perm=999)


# realise une CAP sur le fichier especes et le fichier variables écologiques
# ==========================================================================

# On realise une CAP  avec altitude 
# ================

# CAP sur les abondances 
cap_alt=capscale(fichier ~ Altitude, data=sites_alt,distance="bray", sqrt.dist=T, add=T, dfun=vegdist)
summary(cap_alt)

# Calcul des valeurs propres
valeurs_propres_CCA = round((cap_alt$CCA$eig/(sum(cap_alt$CCA$eig)+sum(cap_alt$CA$eig))*100), digits = 2)
valeurs_propres_CA = round((cap_alt$CA$eig/(sum(cap_alt$CCA$eig)+sum(cap_alt$CA$eig))*100), digits = 2)


# Label des axes avec les valeurs propres automatiques 
cap1label = paste('CAP 1 : ', round(valeurs_propres_CCA[1],2), '%', sep = "")
cap2label = paste('MDS 1 : ', round(valeurs_propres_CA[1],2), '%', sep = "")

# on définit des couleurs pour les groupes de stations


# Graphique
#colvec <- c("dodgerblue", "darkorchid", "gold", "chocolate3", "darkorange", "lightslategray", "olivedrab", "saddlebrown") 
figure=ordiplot(cap_alt, scaling=2, xlab=cap1label, ylab=cap2label, main="CAP_Alt")
points(figure, "sites", pch=16, col=clust.ward.d14.5gr, cex=1.5)
text(figure, "sites", 
     labels = rownames(fichier),        # label de stations sur le graphique
     col = clust.ward.d14.5gr,    # couleur du label (peut être un vecteur)
     pos = 3,                       # position du label
     cex = 0.6)                     # taille du label
	 
ordisurf(cap_alt, Altitude, add=TRUE)

# Le modèle complet est-il significatif ? 
anova(cap_alt, perm=999)

# Les axes contraints sont-il significatifs ? 
anova(cap_alt, by="axis", perm=999)

# Role marginal significatif des variables (compte tenu des autres) ?
anova(cap_alt, by="margin", perm=999)







