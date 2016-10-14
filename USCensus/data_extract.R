options(stringsasfactors=F)
setwd('/Users/christopherlee/Documents/CAL/Real_Life/Cities/USCensus')
library(reshape)
library(ggplot2)
library(plyr)

cmb<-read.csv('cmb.csv') #Census data of establishments, to ascertain restaurants per city
city_dict<-read.csv('list2.csv')
city_dict$CBSA.Code<-as.numeric(as.character(city_dict$CBSA.Code))

restaurants<- c(722511, 722513)
bars<-722410

df_restaurants<- ddply(subset(cmb, naics %in% restaurants), .(msa), summarize, establishments = sum(est))

#Associate all the zip codes with a major city
restaurant_count <- merge(df_restaurants, city_dict[,c(1,4)], by.x="msa", by.y="CBSA.Code")
#Because some zip codes map to multiple names, this following line reduces the list to just the first city
restaurant_df <- ddply(restaurant_count, .(msa), summarize, establishments = mean(establishments), city = head(Principal.City.Name, 1))
subset(B3, Country=="USA")$City -> USCities
subset(restaurant_count, msa %in% subset(city_dict, Principal.City.Name %in% USCities)$CBSA.Code)
(merge(subset(E, Country=="USA"), restaurant_count, by.x="City", by.y="Principal.City.Name", all.x=T))