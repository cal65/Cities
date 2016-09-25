read.csv("UN_Population.csv", na.strings="NA", encoding='UTF-8') -> POP
POP<-POP[-which(duplicated(POP$City)),]
POP$City <- gsub("\\s*\\([^\\)]+\\)", "", POP$City) #remove all parenthesis and content within them
POP$City<-iconv(POP$City) #remove diacritics and accents
POP$City<-tocamel(tolower((POP$City)), upper=TRUE, sep=" ")
POP$City <- mapvalues(POP$City, from= c('Mexico  Ciudad De', 'Moskva', 'Hong Kong Sar', 'Bogot   D C', 'Rio De Janeiro', 'Montr Al', 'Roma', "Wien", 'Warszawa', 'Bruxelles', 'Kobenhavn', 'Milano'), to=c('Mexico City', 'Moscow', 'Hong Kong', 'Bogota', 'Rio de Janeiro', 'Montreal', 'Rome', 'Vienna', 'Warsaw', 'Brussels', 'Copenhagen', 'Milan'))
POP_df<-data.frame(City=POP$City, Population=POP$Value)
write.csv(merge(B3, POP_df, by="City", all.x=T)[,c(1, 27)], 'Population.csv', row.names=F)