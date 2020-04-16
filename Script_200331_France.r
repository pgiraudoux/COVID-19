#### France

# https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/#_

url1<-"https://static.data.gouv.fr/resources/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/20200330-190005/donnees-hospitalieres-covid19-2020-03-30-19h00.csv"

url1<-"https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7" # url stable



cov19brut<-read.table(url1,sep=";",header=TRUE)

head(cov19brut)
tail(cov19brut)
cov19<-cov19brut[cov19brut$sexe==0,]
cov19<-aggregate(cov19[,4:7],by=list(jour=cov19$jour),sum)
cov19$jour<-strptime(cov19$jour,format="%Y-%m-%d")


par(mfrow=c(2,2))

## d�c�s cumulatif
plot(dc~as.numeric(jour),data=cov19,xaxt="n",xlab="",ylab="D�c�s cumul�s",las=1,type="l")
axis(1,at=as.numeric(cov19$jour),labels=as.POSIXlt(cov19$jour)$mday)

## d�c�s journalier
plot(diff(cov19$dc)~as.numeric(jour)[2:length(as.numeric(jour))],data=cov19,xaxt="n",xlab="",ylab="D�c�s/jour",las=1,type="h")
axis(1,at=as.numeric(cov19$jour)[2:length(as.numeric(cov19$jour))],labels=as.POSIXlt(cov19$jour)$mday[2:length(as.numeric(cov19$jour))])

## hospitalis�es
cov19$hosp
plot(hosp~as.numeric(jour),data=cov19,xaxt="n",xlab="",ylab="Hospitalis�s",las=1,type="l")
axis(1,at=as.numeric(cov19$jour),labels=as.POSIXlt(cov19$jour)$mday)

## entrees
plot(diff(cov19$hosp)~as.numeric(jour)[2:length(as.numeric(jour))],data=cov19,xaxt="n",xlab="",ylab="Entr�es/jour",las=1,type="h")
axis(1,at=as.numeric(cov19$jour)[2:length(as.numeric(cov19$jour))],labels=as.POSIXlt(cov19$jour)$mday[2:length(as.numeric(cov19$jour))])

diff(cov19$hosp)
# tendance � partir du 25


mod<-glm(diff(cov19$hosp)[7:length(diff(cov19$hosp))]~as.numeric(cov19$jour)[8:length(as.numeric(cov19$jour))],family=poisson)
summary(mod)
library(pgirmess)
PermTest(mod)
anova(mod,test="Chi")

as.POSIXlt(cov19$jour)$mday[8:length(as.numeric(cov19$jour))]

#### Global

glob<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

globd<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

nrow(glob)
names(glob)
dates<-strptime(substr(names(glob)[5:ncol(glob)],2,20),format="%m.%d.%y")

glob$Country.Region

### France

par(mfrow=c(2,2))

plot(dates,glob[117,5:ncol(glob)],type="l",las=1,ylab="",xlab="",main="COVID-19 cases")
plot(dates[2:length(dates)],diff(unlist(glob[117,5:ncol(glob)])),type="h",ylab="",xlab="",main="COVID-19 cases/day",las=1)

nrow(globd)
names(globd)
datesd<-strptime(substr(names(globd)[5:ncol(globd)],2,20),format="%m.%d.%y")



plot(dates,globd[117,5:ncol(globd)],type="l",las=1,ylab="",xlab="",main="COVID-19 deaths")
plot(datesd[2:length(datesd)],diff(unlist(globd[117,5:ncol(globd)])),type="h",ylab="",xlab="",main="COVID-19 deaths/day",las=1)


globd[117,ncol(globd)]/glob[117,ncol(glob)]

### Kin

par(mfrow=c(1,2))
plot(dates,glob[85,5:ncol(glob)],type="l",las=1,ylab="", xlab="", main="COVID-19 cases")
plot(dates[2:length(dates)],diff(unlist(glob[85,5:ncol(glob)])),type="h",ylab="",xlab="",las=1,  main="COVID-19 deaths")



### France Italie Espagne

plot(dates,glob[138,5:ncol(glob)],type="l",las=1,ylab="",xlab="",main="COVID-19 cases",col="green")
lines(dates,glob[202,5:ncol(glob)],type="l",las=1,ylab="",xlab="",main="COVID-19 cases",col="orange")
lines(dates,glob[117,5:ncol(glob)],type="l",las=1,ylab="",xlab="",main="COVID-19 cases",col="blue")

# from European CDC


ecdc<-read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

head(ecdc[ecdc$countriesAndTerritories=="France",])
tail(ecdc[ecdc$countriesAndTerritories=="France",])

# write.delim(data.frame(country=unique(ecdc$countriesAndTerritories)),"Countries.txt")




