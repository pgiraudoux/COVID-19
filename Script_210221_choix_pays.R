
# ecdc<-read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv") # jusquen 2020

# Every week between Monday and Wednesday, a team of epidemiologists screen up to 500 relevant sources to collect the latest figures for publication on Thursday. https://www.ecdc.europa.eu/en/covid-19/data-collection


ecdc<-read.csv("https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv",encoding="UTF-8")
names(ecdc)[1]<-"country"




country<-"Switzerland"
country<-"France"
head(ecdc[ecdc$country==country,])
tail(ecdc[ecdc$country==country,])
# if(!class(ecdc$dateRep)[1]=="POSIXlt") ecdc$dateRep


### cases


par(mfrow=c(2,2))

indic<-"cases"
pays<-ecdc[ecdc$country==country & ecdc$indicator==indic,]
pays<-pays[order(pays$year_week),]
# pays
firstcase<-which(cumsum(pays$weekly_count)>=start)[1]

mydate<-strptime(paste0(pays$year_week,"-1"),format="%Y-%W-%u")[firstcase:length(pays$year_week)]

mydaterange<-range(mydate,na.rm=TRUE)+6*24*60*60 # end of week on Sunday


dates<-seq(mydaterange[1],mydaterange[2],l=length(firstcase:length(pays$year_week)))


# temps<-range(pays$dateRep[firstcase:length(pays$dateRep)])
temps<-as.character(mydaterange)

# span<-40/length(pays$dateRep[firstcase:length(pays$dateRep)])


# plot(firstcase:length(pays$dateRep),cumsum(pays$weekly_count)[firstcase:length(pays$dateRep)]/1000,las=1,type="l",las=1,xlab="",ylab="cases",main=paste0("n cas x 1000\n",format(temps[1],"%d %b")," - ",format(temps[2],"%d %b")),xaxt="n")

plot(dates,cumsum(pays$weekly_count)[firstcase:length(pays$year_week)]/1000,las=1,type="l",las=1,xlab="",ylab=indic,main=paste0("n ",indic," x 1000 par semaine\n",temps[1]," à ",temps[2]))

plot(dates,pays$weekly_count[firstcase:length(pays$year_week)]/1000,las=1,type="h",las=1,xlab="",ylab=indic,main = paste0("n ",indic,"/semaine\n(x 1000)"))


# loe<-loess(pays$cases[firstcase:length(pays$cases)]~as.numeric(pays$dateRep[firstcase:length(pays$cases)]),span=span)
# lines(loe$fitted~as.numeric(pays$dateRep[firstcase:length(pays$cases)]),col="red",lwd=2)


popCH<-8603900
popF<-67848156
popF/popCH

popCH

pays$weekly_count[length(pays$weekly_count)]/popCH*1e5

pays$weekly_count[length(pays$weekly_count)]/popF*1e5


