options(stringsasfactors=F)
library(plyr)
library(maps)
library(rapportools)
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
setwd('/Users/christopherlee/Documents/CAL/Real_Life/Cities/')
A1<-read.csv("Average_daily_no._of_visits_to_top_5_Art_exhibitions.csv", TRUE, sep=",",dec=".", na.strings="NA", stringsAsFactors=FALSE, encoding='UTF-8')
colnames(A1)[2]<-'Art_exhibitions_visits'
A2<-read.csv("Cinema_Screens2.csv", TRUE, sep=",",dec=".", na.strings="NA", encoding='UTF-8', stringsAsFactors=FALSE )
colnames(A2)[2]<-'Cinema_Screens'
A3<-read.csv("Cinemas.csv", TRUE, sep=",",dec=".", na.strings="NA", encoding='UTF-8', stringsAsFactors=FALSE)
colnames(A3)[2]<-'Cinemas'
A4<-read.csv("Creative_Industries_Employment.csv", TRUE, sep=",",dec=".", na.strings="NA", encoding='UTF-8', stringsAsFactors=FALSE)
colnames(A4)[2]<-'Creative_Industries_Employment'
A5<-read.csv("Education_level-%_with_degree_level_or_higher.csv", TRUE, sep=",",dec=".", na.strings="NA", encoding='UTF-8', stringsAsFactors=FALSE)
colnames(A5)[2]<-'Educated'
A6<-read.csv("Foreign_born_population.csv", TRUE, sep=",",dec=".", na.strings="NA", encoding='UTF-8', stringsAsFactors=FALSE)
colnames(A6)[2]<-'Foreign_born'
A7<-read.csv("GDP_(ppp)(million).csv", TRUE, sep=",",dec=".", na.strings="NA", encoding='UTF-8', stringsAsFactors=FALSE)
colnames(A7)[2]<-'GDP'
A8<-read.csv("Geographical_area_size,_sq._km.csv", TRUE, sep=",",dec=".", na.strings="NA", encoding='UTF-8', stringsAsFactors=FALSE)
colnames(A8)[2]<-'Area'
A9<-read.csv("Major_Concert_Halls.csv", TRUE, sep=",",dec=".", na.strings="NA", encoding='UTF-8', stringsAsFactors=FALSE)
colnames(A9)[2]<-'Concert_Halls'
A10<-read.csv("Median_gross_weekly_earnings_(ppp).csv", TRUE, sep=",",dec=".", na.strings="NA", encoding='UTF-8', stringsAsFactors=FALSE)
colnames(A10)[2]<-'Median Weekly Earnings'
A11<-read.csv("Number_of_bars.csv", TRUE, sep=",",dec=".", na.strings="NA", encoding='UTF-8', stringsAsFactors=FALSE)
colnames(A11)[2]<-'Number_of_bars'
A12<-read.csv("Number_of_comedy_clubs.csv", TRUE, sep=",",dec=".", na.strings="NA", encoding='UTF-8', stringsAsFactors=FALSE)
colnames(A12)[2]<-'Number_of_comedy'
A13<-read.csv("No._of_international_tourists_per_year.csv", TRUE, sep=",",dec=".", na.strings="NA", encoding='UTF-8', stringsAsFactors=FALSE)
colnames(A13)[2]<-'Number_of_tourists'
A14<-read.csv("Number_of_restaurants.csv", TRUE, sep=",",dec=".", na.strings="NA", encoding='UTF-8', stringsAsFactors=FALSE)
colnames(A14)[2]<-"Number_of_restaurants"
A15<-read.csv('Public_Libraries.csv', TRUE, sep=",",dec=".", na.strings="NA", encoding='UTF-8', stringsAsFactors=FALSE)
colnames(A15)[2]<-'Public_libraries'
t<-15 #total spreadsheets
N<-list()
for (i in 1:t) {
	N[[i]]<-get(paste('A',i, sep=''))
}

#crude for loop to remove all the text formatting errors from city names
for (i in 1:t) {
	grep('Bogot', N[[i]]$City)->b #Does this dataframe have Bogota?
	grep('Mont', N[[i]]$City)->c #Does this dataframe have Montreal?
	grep('Paulo', N[[i]]$City)->d #Does this dataframe have Sao Paulo?
	if (length(b) != 0) {
		N[[i]]$City[b]<-'Bogota'
		}
	if (length(c) != 0) {
		N[[i]]$City[c]<-'Montreal'
		}
	if (length(d) != 0) {
		N[[i]]$City[d]<-'Sao Paulo'
		}
}
A<-merge(N[[1]],N[[2]], by.x='City', by.y='City', all=TRUE)
for (i in 3:t) {
	A<-merge(A, N[[i]], by.x='City', by.y='City', all.x=TRUE)
}
A$Educated<-as.numeric(sub('%', '', A$Educated))
A$Educated<-A$Educated/100
B<-A[,c(1,c(0:(t-1)*5+2))]
for (i in 1:ncol(B)) {
	if (any(grepl('%',B[,i]))) {
		B[,i]<-as.numeric(gsub('%','', B[,i]))
		B[,i]<-B[,i]/100
	}
	if (any(grepl(',',B[,i]))) {
		B[,i]<-gsub(',','', B[,i])
	}
	if (any(grepl('[:$:]',B[1,i]))) {
		B[,i]<-gsub('[:$:]','', B[,i])
	}	
}

B[,-1]<-apply(B[,-1],2,as.numeric)
metro<-read.csv('Metros.csv')
B<-merge(B, metro[,1:5], by="City", all.x=T)
B$Usage[is.na(B$Usage)]<-0
B$Annual.ridership[is.na(B$Annual.ridership)]<-0
Starbucks<-read.csv('Starbucks_summary2.csv', encoding='UTF-8')
#Several lines to facilitate mapping of Starbucks dataset
country_codes<-read.csv('country_codes2.csv', encoding='UTF-8')
country_codes<-apply(country_codes, 2, as.character)
Starbucks$Country<-as.character(Starbucks$Country)
Starbucks$Country<-mapvalues(Starbucks$Country, from=country_codes[,2], to=country_codes[,1])
Starbucks$Country<-tocamel(tolower((Starbucks$Country)), upper=TRUE, sep=" ")
Starbucks$Country<-mapvalues(Starbucks$Country, from=c("United States", "Korea  Republic Of", "Russian Federation", "Taiwan  Province Of China"), to = c('USA', 'South Korea', 'Russia', "Taiwan"))
Starbucks$City<-mapvalues(Starbucks$City, from=c('Warszawa', 'Wien', 'Stockholm - Arlanda', 'Taipei City'), to=c('Warsaw', 'Vienna', 'Stockholm', 'Taipei'))
Starbucks[which(Starbucks$City=="Hong Kong"),]$Country<-'Hong Kong S.A.R.'
B$Country<-as.character(B$Country)
B2<-merge(B, Starbucks, by=c("City", "Country"), all.x=T)
Starbucks_sum<-ddply(B2, .(City), summarize, Starbucks=sum(Starbucks))
Starbucks_sum[is.na(Starbucks_sum$Starbucks),]$Starbucks<-0
B2<-B2[!duplicated(B2$City),]
B2$Starbucks<-Starbucks_sum$Starbucks
#Still need to solve cities in Starbucks database duplicated because of differenet country subdivision
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")
runways<-read.csv('cities_runways.csv', encoding="UTF-8")
runways$municipality<-gsub("'|~", "", iconv(runways$municipality, to='ASCII//TRANSLIT'))
runways$iso_country<-mapvalues(runways$iso_country, from=country_codes[,2], to=country_codes[,1])
runways$iso_country<-tocamel(tolower((runways$iso_country)), upper=TRUE, sep=" ")
runways$iso_country<-mapvalues(runways$iso_country, from=c("United States", "Korea  Republic Of", "Russian Federation", "Taiwan  Province Of China", "Hong Kong"), to = c('USA', 'South Korea', 'Russia', "Taiwan", "Hong Kong S.A.R."))
runways$municipality<-mapvalues(runways$municipality, from=c("Rio De Janeiro"), to=c("Rio de Janeiro"))
runways$iso_region<-as.character(runways$iso_region)
#Need to figure out a way to ignore case in merge
B3<-merge(B2, runways, by.x=c("City", "Country"), by.y=c("municipality", "iso_country"), all.x=T)
runways_sum<-ddply(B3, .(City), summarize, runways=sum(total_runways))
B3<-B3[!duplicated(B3$City),]
B3$total_runways<-runways_sum$runways
C40<-read.csv('C40_GHG_Emissions_2014.csv')
C40<-C40[,colnames(C40) %in% c('City.Short.Name','Country', 'Total.City.wide.Emissions..metric.tonnes.CO2e.')]
C40$City.Short.Name<-as.character(C40$City.Short.Name)
C40$Country<-as.character(C40$Country)
C40[C40$City.Short.Name=='Hong Kong',]$Country<-'Hong Kong S.A.R.'
C40$City.Short.Name<-mapvalues(C40$City.Short.Name, from=c('Bogotá', 'São Paulo'), to=c('Bogota', 'Sao Paulo'))
B3<-merge(B3, C40, by.x=c('City', 'Country'), by.y=c('City.Short.Name', 'Country'), all.x=T)
data(world.cities)
world.cities$name<- mapvalues(world.cities$name, from=c("Xianggangdao", "Soul", "Bombay"), to = c("Hong Kong", "Seoul", "Mumbai"))
E<-merge(B3, world.cities, by.x="City", by.y="name")
E<-E[which(E$pop %in% ddply(E, .(City), summarize, pop=max(pop))$pop),]
E[which(E$City=="Hong Kong"),]$pop

B3[,-c(which(apply(B3, 2, max, na.rm=T) < 1), grep('per', colnames(B3)) )] ->Raw
Raw<-sweep(Raw[, which(!colnames(Raw) %in% c('City', 'Country', 'Country.Subdivision', 'iso_region'))], 1, E$pop, "/")
B4<-data.frame(City=E$City, Country=E$Country, B[,c(which(apply(B, 2, max, na.rm=T) < 1), grep('per', colnames(B)) )], Raw)

cbind(B3$City, apply(apply(B3, 2, is.na), 1, sum))