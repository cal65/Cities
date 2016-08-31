setwd("/Users/christopherlee/Documents/CAL/Real_Life/Cities")
library(plyr)
Sys.setlocale(category = "LC_ALL", locale = "zh_CN.UTF-8")
airports<-read.csv('airports.csv', encoding='utf-8')
runways<-read.csv('runways.csv')

merge(subset(airports, type=="large_airport"), nrun, by.x='id', by.y='airport_ref')->merged_airports
merged_airports<-merged_airports[,c(1:7,8:11, 19)]
cities_runways<-ddply(merged_airports, .(municipality, iso_country, iso_region), summarize, total_runways=sum(runways))

write.csv(subset(cities_runways, municipality !=''), "cities_runways.csv", fileEncoding="utf-8", row.names=F)
library(ggplot2)
ggplot(merged_airports) + geom_point(aes(y=latitude_deg, x=longitude_deg, size=runways, color=continent), alpha=0.5)

B<-A[, -c(grep('Date', colnames(A)), grep('Notes', colnames(A)))]