library(tidyverse)
library(dplyr)
library(ggplot2)
library(dslabs)

#abaixo inserir nos parenteses o caminho para wine.data
crimes = read.csv('D:/Dados/Downloads/crime.csv', header = TRUE)
View(crimes)

data = crimes

#omite valores sem preenchimento
crimes = na.omit(crimes)

#filtrando tipos de crimes
crime_type <- filter(crimes, OFFENSE_CODE_GROUP %in% c("Larceny", "Drug Violation", "Homicide"))
crime_larceny <- filter(crimes, OFFENSE_CODE_GROUP %in% c("Larceny"))
crime_drug_violation <- filter(crimes, OFFENSE_CODE_GROUP %in% c("Drug Violation"))
crime_homicide <- filter(crimes, OFFENSE_CODE_GROUP %in% c("Homicide"))

#retirando os dados considerados outliers
crime_larceny <- crime_larceny[-c(which(crime_larceny$Long>-10)),]
crime_drug_violation <- crime_drug_violation[-c(which(crime_drug_violation$Long>-10)),]

#grafico de acordo com horario
ggplot(data=crime_type, aes(x=HOUR, y=OFFENSE_CODE_GROUP, fill = OFFENSE_CODE_GROUP)) +
  geom_bar(stat="identity", width=0.5)

#grafico de acordo com o dia da semana
ggplot(data=crime_type, aes(x=DAY_OF_WEEK, y=OFFENSE_CODE_GROUP, fill = OFFENSE_CODE_GROUP)) +
  geom_bar(stat="identity", width=0.5)

#grafico de cada tipo de crime: lat-lon
#furto
ggplot(data = crime_larceny) + 
  geom_point(aes(x = Lat, y = Long, col = DISTRICT)) +
  ggtitle("Larceny crime")

#drogas
ggplot(data = crime_drug_violation) + 
  geom_point(aes(x = Lat, y = Long, col = DISTRICT)) +
  ggtitle("Drug Violation crime")

#assassinato
ggplot(data = crime_homicide) + 
  geom_point(aes(x = Lat, y = Long, col = DISTRICT)) +
  ggtitle("Homicide crime")

#series temporais
ggplot(crime_type, aes(YEAR, MONTH, col = OFFENSE_CODE_GROUP)) +
  geom_line()

nrow(subset(crime_larceny, YEAR==2015))
nrow(subset(crime_larceny, YEAR==2016))     
nrow(subset(crime_larceny, YEAR==2017))
nrow(subset(crime_larceny, YEAR==2018))

nrow(subset(crime_drug_violation, YEAR==2015))
nrow(subset(crime_drug_violation, YEAR==2016))     
nrow(subset(crime_drug_violation, YEAR==2017))
nrow(subset(crime_drug_violation, YEAR==2018))

nrow(subset(crime_homicide, YEAR==2015))
nrow(subset(crime_homicide, YEAR==2016))     
nrow(subset(crime_homicide, YEAR==2017))
nrow(subset(crime_homicide, YEAR==2018))