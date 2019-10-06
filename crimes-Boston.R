library(tidyverse)
library(dplyr)
library(ggplot2)
library(dslabs)

#abaixo inserir nos parenteses o caminho para crimes data
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

#series temporais#

#craindo um dataset com os numeros de cada ano para cada tipo de crime
larceny_evolution <-c (nrow(subset(crime_larceny, YEAR==2015)), nrow(subset(crime_larceny, YEAR==2016)), nrow(subset(crime_larceny, YEAR==2017)), nrow(subset(crime_larceny, YEAR==2018)))
drug_evolution <-c (nrow(subset(crime_drug_violation, YEAR==2015)), nrow(subset(crime_drug_violation, YEAR==2016)), nrow(subset(crime_drug_violation, YEAR==2017)), nrow(subset(crime_drug_violation, YEAR==2018)))
homicide_evolution <-c (nrow(subset(crime_homicide, YEAR==2015)), nrow(subset(crime_homicide, YEAR==2016)), nrow(subset(crime_homicide, YEAR==2017)), nrow(subset(crime_homicide, YEAR==2018)))
years <-c (2015, 2016, 2017, 2018)

crime_evolution <- data.frame(larceny_evolution, drug_evolution, homicide_evolution, years)

#serie temporal em linha
ggplot(crime_evolution, aes(years, larceny_evolution)) +
  geom_line()+
  geom_point()+
  ggtitle("Larceny crime evolution through the years")

ggplot(crime_evolution, aes(years, drug_evolution, col = years)) +
  geom_line()+
  geom_point()+
  ggtitle("Drug Violation crime evolution through the years")

ggplot(crime_evolution, aes(years, homicide_evolution, col = years)) +
  geom_line()+
  geom_point()+
  ggtitle("Homicide crime evolution through the years")

#procurando os distritos mais violentos em geral
ggplot(data=crime_type, aes(x=DISTRICT, y=OFFENSE_CODE_GROUP, fill = OFFENSE_CODE_GROUP)) +
  geom_bar(stat="identity", width=0.5)

#filtra-los para os tipos de crimes e distritos mais violentos
D4 <- filter(crime_type, DISTRICT %in% ("D4"))
A1 <- filter(crime_type, DISTRICT %in% ("A1"))
B2 <- filter(crime_type, DISTRICT %in% ("B2"))

d4_evolution <-c (nrow(subset(D4, YEAR == 2015)), nrow(subset(D4, YEAR == 2016)), nrow(subset(D4, YEAR == 2017)), nrow(subset(D4, YEAR == 2018)))
a1_evolution <-c (nrow(subset(A1, YEAR == 2015)), nrow(subset(A1, YEAR == 2016)), nrow(subset(A1, YEAR == 2017)), nrow(subset(A1, YEAR == 2018)))
b2_evolution <-c (nrow(subset(B2, YEAR == 2015)), nrow(subset(B2, YEAR == 2016)), nrow(subset(B2, YEAR == 2017)), nrow(subset(B2, YEAR == 2018)))

districts_evolution <- data.frame(d4_evolution, a1_evolution, b2_evolution, years)
View(districts_evolution)

ggplot(districts_evolution, aes(years, d4_evolution)) +
  geom_line()+
  geom_point()+
  ggtitle("District D4 evolution through the years")

ggplot(districts_evolution, aes(years, a1_evolution)) +
  geom_line()+
  geom_point()+
  ggtitle("District A1 evolution through the years")

ggplot(districts_evolution, aes(years, b2_evolution)) +
  geom_line()+
  geom_point()+
  ggtitle("District B2 evolution through the years")
