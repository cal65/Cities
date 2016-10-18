library(ggmap)
library(ggrepel)
library(dbscan)
setwd("Documents/CAL/Real_Life/Cities/Data")
#Given dataframe B4, an output of the Merger file, explore different classifications

City_Imputed<-B4
for(i in 2:ncol(B4)){
	City_Imputed[is.na(City_Imputed[,i]), i] <- mean(City_Imputed[,i], na.rm = TRUE)
}
#replace missing data with parameter means

Scaled<-scale(City_Imputed[, !colnames(City_Imputed) %in% c('City', 'Country')]) 
#Scale all values to z scores
wss<- (nrow(Scaled)-1)*sum(apply(Scaled, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(Scaled, centers=i)$withinss)
#Examine within square

E$cluster <- kmeans(Scaled, 6)$cluster
library(ggmap)
mapWorld<-borders("world", colour="gray50", fill="gray50")
map<- ggplot(E) + mapWorld 
mapPoints <- map + geom_point(data=E, aes(x = long, y = lat, colour=factor(cluster)),alpha=0.8) + theme(legend.position='right') + ggtitle('Cities Clustering')
mapPoints

E$cluster2<- kmeans(Scaled[, c(2, 3, 4, 12, 13, 15, 18, 19)], 5)$cluster
map + geom_point(data=E, aes(x = long, y = lat, colour=factor(cluster2)),alpha=0.7, size=0.8) + theme(legend.position='right') + ggtitle('Cities Clustering') + scale_radius(range=c(.4,1), guide=F)+ scale_color_discrete("Cluster")

E$cluster5<-kmeans(Scaled[, c(2, 3, 4, 7, 14, 19, 20)], 4)$cluster
map + geom_point(data=E, aes(x = long, y = lat, colour=factor(cluster5)),alpha=0.8, size=1) + theme(legend.position='right') + ggtitle('Cities Clustering') + scale_radius(range=c(.4,1), guide=F) + scale_color_discrete("Cluster")

pc<-prcomp(Scaled)
E<-cbind(E, pc=pc$x[,1:2])

ggplot() + geom_point(data=E, aes(x=pc.PC1, y=pc.PC2), color='orange') + geom_text_repel(data=E, aes(x=pc.PC1, y=pc.PC2, label = City, size=0.23), color='purple', segment.color='white', box.padding = unit(0.5, "lines")) +scale_size_continuous(range=c(0,3), guide=FALSE)

#source('coords2continent.R')
#E$Continent<-coords2continent(subset(E, select=c('long', 'lat')))

colPerms <- combn(ncol(Scaled), 6)
#create all city metric permutations of size 6
dbscanResults<-vector("list", ncol(colPerms))
for (i in 1:ncol(colPerms)){
	dbscanResults[[i]] <- dbscan(Scaled[,colPerms[,i]], eps=1.5, minPts = 3)
}
#store dbscan results in list dbscanResults

which(sapply(dbscanResults, function(x) nrow(count(x$cluster)[-1,]))>3)->candidates
#the dbscan results that produced at least 3 clusters
which((sapply(dbscanResults, function(x) length(which(x$cluster==0)))) < 6) ->candidates2
#the dbscan results that produced less than 6 unclustered cities
best <- intersect(candidates2,candidates)
#the dbscan results with at least 3 clusters and less than 6 unclustered cities
for (i in 1:length(best)){
	map + geom_point(data=E, aes(x = long, y = lat, colour=factor(dbscanResults[[best[i]]]$cluster)),alpha=0.8, size=1) + theme(legend.position='right') + ggtitle('Cities Clustering') + scale_radius(range=c(.4,1), guide=F) + scale_color_discrete("Cluster")
}

#working on evaluating clusters

continents <- read.csv('Combos.csv')
conts <- continents$Combo1
source('../Calgorithm.r')
kmeansResults<-vector("list", ncol(colPerms))
calgResults<-numeric(ncol(colPerms))
ptm<-proc.time()
for (i in 1:ncol(colPerms)){
	kmeansResults[[i]] <- kmeans(Scaled[,colPerms[,i]], 4)
	calgResults[i] <- calgorithm(kmeansResults[[i]]$cluster, conts)
}
proc.time() - ptm

matrix(colnames(B3)[colPerms[,best]+2], nrow=6)

library(WeightedCluster)

ptm <- proc.time()
for (i in 1:100){
	kmeansResults[[i]] <- kmeans(Scaled[,colPerms[,i]], 6)
	calgResults[[i]] <- calgorithm(conts, kmeansResults[[i]]$cluster)
}

proc.time() - ptm
