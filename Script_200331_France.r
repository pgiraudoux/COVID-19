#### France

# https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/#_

# url1<-"https://static.data.gouv.fr/resources/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/20200330-190005/donnees-hospitalieres-covid19-2020-03-30-19h00.csv"

url1<-"https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7" # url stable

url2<-"https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c"

# cov19brut<-read.table(url1,sep=";",header=TRUE)
# head(cov19brut)
# tail(cov19brut)
# cov19<-cov19brut[cov19brut$sexe==0,]
# cov19<-aggregate(cov19[,4:7],by=list(jour=cov19$jour),sum)
# cov19$jour<-strptime(cov19$jour,format="%Y-%m-%d")


cov19<-read.table(url2,sep=";",header=TRUE)
cov19<-cov19[nchar(cov19$dep)<3,]
# cov19n<-aggregate(cov19[,3:6],by=list(jour=cov19$jour),sum)
cov19n<-cov19[cov19$dep=="25",]
head(cov19n)
tail(cov19n)
cov19n$jour<-strptime(cov19n$jour,format="%Y-%m-%d")

span<-40/length(cov19n$jour)
temps<-c(cov19n$jour[1],cov19n$jour[nrow(cov19n)])
par(mfrow=c(2,2))


plot(cov19n$jour,cov19n$incid_hosp,xlab="",ylab="Entrées/jour",las=1,type="h",main=paste0("Hôpital: nombre d'entrées/jour\n",format(temps[1],"%d %b")," - ",format(temps[2],"%d %b")))
loe<-loess(cov19n$incid_hosp~as.numeric(cov19n$jour),span=span)
lines(loe$fitted~as.numeric(cov19n$jour),col="red",lwd=2)


plot(cov19n$jour,cov19n$incid_hosp-cov19n$incid_rad,xlab="",ylab="Entrées-sorties",las=1,type="h",main="(Entrées-sorties)/jour")

# plot(cov19n$jour,cumsum(cov19n$incid_hosp)-cumsum(cov19n$incid_rad),xlab="",ylab="Nombre d'hospitalisés",las=1,type="l",main="Nombre d'hospitalisés")


plot(cov19n$jour,cov19n$incid_rea,xlab="",ylab="Entrées/jour",las=1,type="h",main="Nombre d'entrées\nen réanimation/jour")
loe<-loess(cov19n$incid_rea~as.numeric(cov19n$jour),span=span)
lines(loe$fitted~as.numeric(cov19n$jour),col="red",lwd=2)


plot(cov19n$jour,cov19n$incid_dc,xlab="",ylab="Décès/jour",las=1,type="h",main="Décès journaliers hospitaliers")
 
# mtext(paste0("Total: ",sum(cov19n$incid_dc)),3,line=-1.51,adj=0,at=1584572400)

cov19n$incid_hosp
cov19n$incid_dc

cumsum(cov19n$incid_hosp)
cumsum(cov19n$incid_rad)


ecdc<-read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
head(ecdc[ecdc$countriesAndTerritories=="France",])
tail(ecdc[ecdc$countriesAndTerritories=="France",])
names(ecdc)[1]<-"dateRep"
ecdc$dateRep<-strptime(ecdc$dateRep,format="%d/%m/%Y")
unique(ecdc$countriesAndTerritories)

pays<-ecdc[ecdc$countriesAndTerritories=="Switzerland",]
pays<-pays[order(pays$dateRep),]
pays
firstcase<-which(cumsum(pays$cases)>=start)[1]

par(mfrow=c(2,2))

plot(pays$dateRep[firstcase:length(pays$cases)],cumsum(pays$cases)[firstcase:length(pays$cases)]/1000,las=1,type="l",las=1,xlab="",ylab="cases",main="n cas x 1000")
abline(v=1590962400, col="green", lwd=2)

plot(pays$dateRep[firstcase:length(pays$cases)],pays$cases[firstcase:length(pays$cases)],las=1,type="l",las=1,xlab="",ylab="cases",main = "N cas/jour")
loe<-loess(pays$cases[firstcase:length(pays$cases)]~as.numeric(pays$dateRep[firstcase:length(pays$cases)]),span=span)
lines(loe$fitted~as.numeric(pays$dateRep[firstcase:length(pays$cases)]),col="red",lwd=2)
abline(v=1590962400, col="green", lwd=2)

plot(pays$dateRep[firstcase:length(pays$cases)],cumsum(pays$deaths)[firstcase:length(pays$cases)]/1000,las=1,type="l",las=1,xlab="",ylab="cases", main="N morts x 1000")

plot(pays$dateRep[firstcase:length(pays$cases)],pays$deaths[firstcase:length(pays$cases)],las=1,type="l",las=1,xlab="",ylab="cases",main="N morts/jour")
loe<-loess(pays$deaths[firstcase:length(pays$cases)]~as.numeric(pays$dateRep[firstcase:length(pays$cases)]),span=span)
lines(loe$fitted~as.numeric(pays$dateRep[firstcase:length(pays$cases)]),col="red",lwd=2)

pays[,c("dateRep","cases")]
pays[,c("dateRep","deaths")]

sum(pays[(nrow(pays)-7):nrow(pays),"deaths"])


data.frame(date=pays$dateRep[firstcase:length(pays$cases)],num=as.numeric(pays$dateRep[firstcase:length(pays$cases)]))
##### ancienne présentation < 18/04/20



par(mfrow=c(2,2))

## décès cumulatif
plot(dc~as.numeric(jour),data=cov19,xaxt="n",xlab="",ylab="Décès cumulés",las=1,type="l")
axis(1,at=as.numeric(cov19$jour),labels=as.POSIXlt(cov19$jour)$mday)

## décès journalier
plot(diff(cov19$dc)~as.numeric(jour)[2:length(as.numeric(jour))],data=cov19,xaxt="n",xlab="",ylab="Décès/jour",las=1,type="h")
axis(1,at=as.numeric(cov19$jour)[2:length(as.numeric(cov19$jour))],labels=as.POSIXlt(cov19$jour)$mday[2:length(as.numeric(cov19$jour))])

## hospitalisées
cov19$hosp
plot(hosp~as.numeric(jour),data=cov19,xaxt="n",xlab="",ylab="Hospitalisés",las=1,type="l")
axis(1,at=as.numeric(cov19$jour),labels=as.POSIXlt(cov19$jour)$mday)

## entrees
plot(diff(cov19$hosp)~as.numeric(jour)[2:length(as.numeric(jour))],data=cov19,xaxt="n",xlab="",ylab="Entrées/jour",las=1,type="h")
axis(1,at=as.numeric(cov19$jour)[2:length(as.numeric(cov19$jour))],labels=as.POSIXlt(cov19$jour)$mday[2:length(as.numeric(cov19$jour))])

diff(cov19$hosp)
# tendance à partir du 25


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


bilsex<-aggregate(cov19brut[cov19brut$sex!=0,"hosp",],by=list(sex=cov19brut[cov19brut$sex!=0,"sexe",]),sum)

bilsex$x/sum(bilsex$x)

