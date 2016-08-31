setwd("~/Documents/CAL/Real_Life/Cities/")
library(plyr)
Sys.setlocale(category = "LC_ALL", locale = "zh_CN.UTF-8")
Starbucks<-read.csv("All_Starbucks_Locations_in_the_World.csv", encoding="UTF-8", stringsAsFactors=FALSE)
DD<-ddply(Starbucks, .(City, Country.Subdivision, Country), summarize, Starbucks=length(City))
DD<-DD[order(DD$Starbucks, decreasing=T),]
write.csv(DD, 'Starbucks_Summary.csv',  fileEncoding="UTF-8", row.names=F)