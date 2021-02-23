
ecdc<-read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

country<-"Switzerland"
country<-"France"
head(ecdc[ecdc$countriesAndTerritories==country,])
tail(ecdc[ecdc$countriesAndTerritories==country,])
names(ecdc)[1]<-"dateRep"
if(!class(ecdc$dateRep)[1]=="POSIXlt") ecdc$dateRep<-strptime(ecdc$dateRep,format="%d/%m/%Y")


pays<-ecdc[ecdc$countriesAndTerritories==country,]
pays<-pays[order(pays$dateRep),]
pays
firstcase<-which(cumsum(pays$cases)>=start)[1]


temps<-range(pays$dateRep[firstcase:length(pays$cases)])

span<-40/length(pays$dateRep[firstcase:length(pays$cases)])

par(mfrow=c(2,2))

plot(pays$dateRep[firstcase:length(pays$cases)],cumsum(pays$cases)[firstcase:length(pays$cases)]/1000,las=1,type="l",las=1,xlab="",ylab="cases",main=paste0("n cas x 1000\n",format(temps[1],"%d %b")," - ",format(temps[2],"%d %b")))

plot(pays$dateRep[firstcase:length(pays$cases)],pays$cases[firstcase:length(pays$cases)]/1000,las=1,type="h",las=1,xlab="",ylab="cases",main = "n cas/semaine\n(x 1000)")
# loe<-loess(pays$cases[firstcase:length(pays$cases)]~as.numeric(pays$dateRep[firstcase:length(pays$cases)]),span=span)
# lines(loe$fitted~as.numeric(pays$dateRep[firstcase:length(pays$cases)]),col="red",lwd=2)


plot(pays$dateRep[firstcase:length(pays$cases)],cumsum(pays$deaths)[firstcase:length(pays$cases)]/1000,las=1,type="l",las=1,xlab="",ylab="cases", main="N morts x 1000")

plot(pays$dateRep[firstcase:length(pays$cases)],pays$deaths[firstcase:length(pays$cases)],las=1,type="h",las=1,xlab="",ylab="cases",main="N morts/semaine")
# loe<-loess(pays$deaths[firstcase:length(pays$cases)]~as.numeric(pays$dateRep[firstcase:length(pays$cases)]),span=span)
# lines(loe$fitted~as.numeric(pays$dateRep[firstcase:length(pays$cases)]),col="red",lwd=2)


popCH<-8603900
popF<-67848156
popF/popCH

popCH

pays$cases[length(pays$cases)]/popCH*1e5

pays$cases[length(pays$cases)]/popF*1e5



pays[,c("dateRep","cases")]
pays[,c("dateRep","deaths")]

sum(pays[(nrow(pays)-7):nrow(pays),"deaths"])

