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
E$cluster3<-kmeans(Scaled[, c(2, 3, 4, 7, 12, 13, 15, 18, 19)], 2)$cluster
map + geom_point(data=E, aes(x = long, y = lat, colour=factor(cluster3)),alpha=0.2, size=1) + theme(legend.position='right') + ggtitle('Cities Clustering') + scale_radius(range=c(.4,1), guide=F)
E$cluster4<-kmeans(Scaled[, c(2, 3, 4, 7, 9, 13, 14, 15, 17, 18, 19, 20)], 3)$cluster
map + geom_point(data=E, aes(x = long, y = lat, colour=factor(cluster4)),alpha=0.8, size=1) + theme(legend.position='right') + ggtitle('Cities Clustering') + scale_radius(range=c(.4,1), guide=F) + scale_color_discrete("Cluster")

E$cluster5<-kmeans(Scaled[, c(2, 3, 4, 7, 14, 19, 20)], 4)$cluster
map + geom_point(data=E, aes(x = long, y = lat, colour=factor(cluster5)),alpha=0.8, size=1) + theme(legend.position='right') + ggtitle('Cities Clustering') + scale_radius(range=c(.4,1), guide=F) + scale_color_discrete("Cluster")

pc<-prcomp(Scaled)
E<-cbind(E, pc=pc$x[,1:2])

ggplot() + geom_point(data=E, aes(x=pc.PC1, y=pc.PC2)) + geom_text_repel(data=E, aes(x=pc.PC1, y=pc.PC2, label = City, size=0.23), color='purple', segment.color='white', box.padding = unit(0.5, "lines")) +scale_size_continuous(range=c(0,3), guide=FALSE)

#source('coords2continent.R')
#E$Continent<-coords2continent(subset(E, select=c('long', 'lat')))

colPerms <- combn(ncol(Scaled), 5)
dbscanResults<-vector("list", ncol(colPerms))
for (i in 1:ncol(colPerms)){
	dbscanResults[[i]] <- dbscan(Scaled[, colPerms[,i]], eps=1.1, minPts = 3)
}
sapply(dbscanResults, function(x) length(unique(x$cluster)))

which(sapply(dbscanResults, function(x) nrow(count(x$cluster)[-1,]))>3)->candidates 
#the results that produced at least 3 clusters
which((sapply(dbscanResults, function(x) length(which(x$cluster==0)))) < 6) ->candidates2
#the results that produced less than 6 unclustered cities
E$dbscan11498 <- dbscanResults[[11498]]$cluster
map + geom_point(data=E, aes(x = long, y = lat, colour=factor(dbscan11498)),alpha=0.8, size=1) + theme(legend.position='right') + ggtitle('Cities Clustering') + scale_radius(range=c(.4,1), guide=F) + scale_color_discrete("Cluster")