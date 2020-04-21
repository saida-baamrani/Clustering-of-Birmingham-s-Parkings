data=read.csv(file="dataset.csv",header=TRUE, sep=",")

####Q1 : eliminer les valeurs abérantes 
dim(data)
sum(is.na(data))
sum(data[2]<0)
sum(data[3]<0)
IsDate <- function(mydate, date.format = "%Y-%m-%d %H:%M:%S") {
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}
sum(!IsDate(data[, 4]))
sum(data[3]>data[2])
data = subset(data, data[3]>0)
data = subset(data, data[3]<data[2])
# eliminer les parkings ayant pas assez de mesures (2 parkings)
boxplot(as.Date(LastUpdated)~SystemCodeNumber, data=data)
data=data[data$SystemCodeNumber != "BHMBRTARC01" & data$SystemCodeNumber != "NIA North",]

#### Q2: Statistiques descriptives

d=data
summary(d)
d$tauxcharges=1-d$Occupancy/d$Capacity # calcul de taux de charge 
d$week=format(as.Date(d$LastUpdated, format = "%Y-%m-%d"),"%W") # nombre de la semaine 
d$jour=weekdays(as.Date(d$LastUpdated, format = "%Y-%m-%d")) 
d$heure=as.POSIXct(d$LastUpdated)    
d$heure=round_date(d$heure, unit = "30 mins")
d$heure=format(d$heure, format='%H:%M')
d$tauxcharges=as.numeric(d$tauxcharges)
d$SystemCodeNumber=as.character(d$SystemCodeNumber)
d$Capacity=as.numeric(d$Capacity)
d$Occupancy=as.numeric(d$Occupancy)
d$LastUpdated=as.Date(d$LastUpdated, format = "%Y-%m-%d")

summary(d)
# nombre de mesure de chaque parking
barplot(summary(as.factor(d$SystemCodeNumber)))

# bar plot montarnt les jours de la semaines les plus charges
library(ggplot2)
p <-ggplot(d, aes(d$jour,d$tauxcharges))
p+geom_bar(stat = "identity")

####Q3 : Analyse du comportement hebdomadaire

#1) creation d'un objet contenat nous donnees regroupees par  SystemCodeNumber,week, heur et jour
dd=aggregate(d$tauxcharges, by=list(Category=d$SystemCodeNumber,d$week, d$heure ,d$jour), FUN=mean)

# creation d'une nouvelle dataframe P*S lignes et T colonnes
library(plyr)
dfr=as.data.frame(matrix(ncol=length(unique(d$heure))*length(unique(d$jour)),nrow=length(unique(d$SystemCodeNumber))*length(unique(d$week))))
row.names(dfr)= as.vector(t(outer(as.character(unique(d$SystemCodeNumber)),  as.character(unique(d$week)), paste, sep="-")))
colnames(dfr)=as.vector(t(outer(as.character(unique(dd$Group.4)),  as.character(unique(dd$Group.3)), paste, sep="-")))
head(dfr)

# remplissage de la nouvelle dataframe
for (i in seq(dim(dd)[1])){
  dfr[paste(as.character(dd[i,]$Category), as.character(dd[i,]$Group.2), sep = "-"),paste(as.character(dd[i,]$Group.4),as.character(dd[i,]$Group.3), sep = "-")] = dd[i,]$x 
}
head(dfr)
# remplacer les valeurs na .
library(zoo)
dfr=data.frame(t(na.aggregate(t(dfr))))
head(dfr)

# enlever les valeurs na restants (les parkings n'ayant aucune mesure dans toute une semaine)
dfr = na.omit(dfr) 

# chercher lenombre de clusters avec lequel lancer le Kmeans
inertie = numeric(30)
for (k in 1:30){
  KM = kmeans(dfr, k, nstart = 50)
  inertie[k] = KM$tot.withinss
}
plot(inertie, type="b")

#application du KMedoids
library(TSdist)
clus.four=KMedoids((as.matrix(dfr)),k=3,'fourier')
matplot(t(dfr), type = "l",col=clus.four)


# Hclust 

D <- dist(dfr, method = "euclidean")
H1 <- hclust(D, method="ward.D2")
H2 <- hclust(D, method ="complete")
par(mfrow = c(1, 2))
plot(as.hclust(H1),hang=-1, labels = FALSE, cex = 0.6)
rect.hclust(as.hclust(H1),k=3,border="red")
plot(as.hclust(H2),hang=-1, labels = FALSE, cex = 0.6)
rect.hclust(as.hclust(H2),k=3,border="blue")
par(mfrow = c(1, 2))
plot(rev(H1$height)[1:10], type="b", xlab = "ward.D2")
plot(rev(H2$height)[1:10], type="b", xlab = "complete")
par(mfrow = c(1, 2))
matplot(t(dfr), type = "l",col=cutree(H1, k = 3), xlab = "ward.D2")
matplot(t(dfr), type = "l",col=cutree(H2, k = 3), xlab = "complete")

####Q4 :  Analyse des parkings :

# creation d'une nouvelle dataframe contenant les parking en lignes et les jours des mesures en colonnes
dfr_jour=as.data.frame(matrix(ncol=length(unique(d$LastUpdated)),nrow=length(unique(d$SystemCodeNumber))))
row.names(dfr_jour)=unique(d$SystemCodeNumber)
colnames(dfr_jour)=unique(d$LastUpdated)
head(dfr_jour)

# remplissage de la nouvelle dataframe
for (i in seq(dim(d)[1])){
  c=as.character(d[i,]$SystemCodeNumber)
  lu=as.character(d[i,]$LastUpdated)
  occ=d[i,]$tauxcharges
  dfr_jour[c,lu]=sum(dfr_jour[c,lu],occ,na.rm = TRUE)
}

# remplacer les valeurs NA
dfr_jour=data.frame(t(na.aggregate(t(dfr_jour))))

# chercher lenombre de clusters avec lequel lancer le Kmeans
inertie_j = numeric(15)
for (k in 1:15){
  KM = kmeans(dfr_jour, k, nstart = 50)
  inertie_j[k] = KM$tot.withinss
}
plot(inertie_j, type="b")
#algorithm de Kmeans avec 3 clusters
KM1 = kmeans(dfr_jour,3, nstart = 50)
KM1
matplot(t(dfr_jour), type = "l",col=KM1$cluster)

# application de HCPC
library(FactoMineR)
res.pca.dfr = PCA(dfr_jour)
res.hcpc.dfr = HCPC(res.pca.dfr,order=FALSE)
matplot(t(dfr_jour), type = "l",col= as.numeric(res.hcpc.dfr$data.clust["clust"]$clust))









