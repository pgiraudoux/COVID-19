
ecdc<-read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

country<-"France"
head(ecdc[ecdc$countriesAndTerritories==country,])
tail(ecdc[ecdc$countriesAndTerritories==country,])
names(ecdc)[1]<-"dateRep"
ecdc$dateRep<-strptime(ecdc$dateRep,format="%d/%m/%Y")


pays<-ecdc[ecdc$countriesAndTerritories==country,]
pays<-pays[order(pays$dateRep),]
pays
firstcase<-which(cumsum(pays$cases)>=start)[1]

par(mfrow=c(2,2))

plot(pays$dateRep[firstcase:length(pays$cases)],cumsum(pays$cases)[firstcase:length(pays$cases)]/1000,las=1,type="l",las=1,xlab="",ylab="cases",main="n cas x 1000")

plot(pays$dateRep[firstcase:length(pays$cases)],pays$cases[firstcase:length(pays$cases)],las=1,type="l",las=1,xlab="",ylab="cases",main = "N cas/jour")

plot(pays$dateRep[firstcase:length(pays$cases)],cumsum(pays$deaths)[firstcase:length(pays$cases)]/1000,las=1,type="l",las=1,xlab="",ylab="cases", main="N morts x 1000")

plot(pays$dateRep[firstcase:length(pays$cases)],pays$deaths[firstcase:length(pays$cases)],las=1,type="l",las=1,xlab="",ylab="cases",main="N morts/jour")


pays[,c("dateRep","cases")]
pays[,c("dateRep","deaths")]

sum(pays[(nrow(pays)-7):nrow(pays),"deaths"])

