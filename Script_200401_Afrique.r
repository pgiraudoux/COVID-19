# from European CDC
ecdc<-read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")



head(ecdc[ecdc$countriesAndTerritories=="France",])
tail(ecdc[ecdc$countriesAndTerritories=="France",])
names(ecdc)[1]<-"dateRep"
ecdc$dateRep<-strptime(ecdc$dateRep,format="%d/%m/%Y")

# write.delim(data.frame(country=unique(ecdc$countriesAndTerritories)),"Countries.txt")

### exemple 
grep("Madagascar",unique(ecdc$countriesAndTerritories))
unique(ecdc$countriesAndTerritories)
pays<-ecdc[ecdc$countriesAndTerritories=="Madagascar",]
pays<-pays[order(pays$dateRep),]
head(pays)
tail(pays)

plot(pays$dateRep,cumsum(pays$cases),las=1,ylab="Cases",xlab="Time",type="l")
plot(pays$dateRep,pays$cases,las=1,ylab="Cases",xlab="Time",type="h")

plot(pays$dateRep,log(cumsum(pays$cases)),las=1,ylab="ln(Cases)",xlab="Time")
mod1<-lm(log(cumsum(pays$cases))~as.numeric(pays$dateRep))
abline(mod1)

AC<-read.delim("AfricanCountries.txt")
# ACOMS<-AC[AC[,2]!="no",]
ACOMS<-AC
nrow(ACOMS)
save(ACOMS,file="ACOMS.Rdata")


## exemple test
# pays<-ecdc[ecdc$countriesAndTerritories=="Algeria",]
# pays<-pays[order(pays$dateRep),]
# firstcase<-which(cumsum(pays$cases)>0)[1]
# plot(pays$dateRep[firstcase:length(pays$dateRep)],cumsum(pays$cases)[firstcase:length(pays$dateRep)],las=1,ylab="Cases",xlab="Time",main="Algeria")


# Cas cum par pays sur les 10 derniers jour



resPred<-data.frame(country=rep(NA,nrow(ACOMS)+2),dateP=rep(NA,nrow(ACOMS)+2),ncasJ=rep(NA,nrow(ACOMS)+2),ncasP=rep(NA,nrow(ACOMS)+2),r2adj=rep(NA,nrow(ACOMS)+2),model=rep(NA,nrow(ACOMS)+2))

prev<-15*24*60*60 # 15 jours en secondes
for(i in 1:nrow(ACOMS)){
  print(as.character(ACOMS[i,1]));flush.console()
  
  if(ACOMS[i,1]!="South_Sudan" & ACOMS[i,1]!="Malawi") {
    
  pays<-ecdc[ecdc$countriesAndTerritories==as.character(ACOMS[i,1]),]
  pays<-pays[order(pays$dateRep),]
  firstcase<-length(cumsum(pays$cases))-9
  if(firstcase<1) firstcase<-1
  datapays<-data.frame(date=pays$dateRep[firstcase:length(pays$dateRep)],dateN=as.numeric(pays$dateRep[firstcase:length(pays$dateRep)]),cas=cumsum(pays$cases)[firstcase:length(pays$dateRep)],logcas=log(cumsum(pays$cases)[firstcase:length(pays$dateRep)]))

  newdata<-data.frame(dateN=as.numeric(datapays[nrow(datapays),1]+prev))
  modL<-lm(cas~dateN,data=datapays)
  modE<-lm(logcas~dateN,data=datapays)
  
  if(summary(modE)$adj.r.squared<summary(modL)$adj.r.squared){
    resPred[i,1]<-unlist(as.character(ACOMS[i,1]))
    resPred[i,2]<-as.character(datapays[nrow(datapays),1]+prev)
    resPred[i,3]<-datapays$cas[length(datapays$cas)]
    resPred[i,4]<-round(exp(predict(modE,newdata=newdata)),0)
    resPred[i,5]<-round(summary(modE)$adj.r.squared,2)
    resPred[i,6]<-"exp"
  } else {
    resPred[i,1]<-unlist(as.character(ACOMS[i,1]))
    resPred[i,2]<-as.character(datapays[nrow(datapays),1]+prev)
    resPred[i,3]<-datapays$cas[length(datapays$cas)]
    resPred[i,4]<-round(predict(modL,newdata=newdata),0)
    resPred[i,5]<-round(summary(modL)$adj.r.squared,2)
    resPred[i,6]<-"lin"
  }
  
  
  jpeg(paste0("./CasCum10/",as.character(ACOMS[i,1]),"_cas.jpg"))
  plot(pays$dateRep[firstcase:length(pays$dateRep)],cumsum(pays$cases)[firstcase:length(pays$dateRep)],las=1,ylab="Cases",xlab="Time",main=as.character(ACOMS[i,1]))
  if(resPred[i,6]=="lin") {
    te<-try(abline(modL))
    if(!inherits(te,"try-error")) abline(te)
  }
  dev.off()
  
  jpeg(paste0("./CasCumLog10/",as.character(ACOMS[i,1]),"_cas.jpg"))
  plot(datapays$date,datapays$logcas,las=1,ylab="ln(cases)",xlab="Time",main=as.character(ACOMS[i,1]))
  if(resPred[i,6]=="exp") {
    te<-try(abline(modE))
    if(!inherits(te,"try-error")) abline(te)
  }
  dev.off() 
  }
}


resPred[is.na(resPred[,1]),1]<-"South_Sudan"
resPred[29,1]<-"Malawi"
resPred[nrow(resPred),1]<-"Lesotho"
resPred[nrow(resPred)-1,1]<-"Western_Sahara"        

resPred
library(pgirmess)
write.delim(resPred,"resPred.txt")

### vérif modèle Ben Philips

fg<-na.omit(read.delim("ComparaisonsFG.txt"))
windows()
par(mfrow=c(1,3))
plot(fg[,1],fg[,2], main="Minimum Ben Philips ",xlab="Estimation PG",ylab="Estimation Ben Philips")
abline(lm(fg[,2]~fg[,1]))
plot(fg[,1],fg[,3], main="Maximum Ben Philips ",xlab="Estimation PG",ylab="Estimation Ben Philips")
abline(lm(fg[,3]~fg[,1]))
plot(fg[,1],rowMeans(fg[,2:3]), main="(Max + Min)/2 Ben Philips", xlab="Estimation PG",ylab="Estimation Ben Philips")
abline(lm(rowMeans(fg[,2:3])~fg[,1]))



predict(mod0,newdata=newdata,se.fit=TRUE,interval="prediction")



# Cas cum par pays à partir du premier cas

for(i in 1:nrow(ACOMS)){
  jpeg(paste0("./CasCum/",as.character(ACOMS[i,1]),"_cas.jpg"))
  pays<-ecdc[ecdc$countriesAndTerritories==as.character(ACOMS[i,1]),]
  pays<-pays[order(pays$dateRep),]
  firstcase<-which(cumsum(pays$cases)>0)[1]
  plot(pays$dateRep[firstcase:length(pays$dateRep)],cumsum(pays$cases)[firstcase:length(pays$dateRep)],las=1,ylab="Cases",xlab="Time",main=as.character(ACOMS[i,1]))
  dev.off()
}

resPred<-data.frame(country=rep(NA,nrow(ACOMS)),date=rep(NA,nrow(ACOMS)),ncas=rep(NA,nrow(ACOMS)))
prev<-15*24*60*60 # 15 jours en secondes
for(i in 1:nrow(ACOMS)){
  print(as.character(ACOMS[i,1]));flush.console()
  jpeg(paste0("./CasCumLog/",as.character(ACOMS[i,1]),"_cas.jpg"))
  pays<-ecdc[ecdc$countriesAndTerritories==as.character(ACOMS[i,1]),]
  pays<-pays[order(pays$dateRep),]
  firstcase<-which(cumsum(pays$cases)>0)[1]
  datapays<-data.frame(date=pays$dateRep[firstcase:length(pays$dateRep)],dateN=as.numeric(pays$dateRep[firstcase:length(pays$dateRep)]),logcas=log(cumsum(pays$cases)[firstcase:length(pays$dateRep)]))
  plot(datapays$date,datapays$logcas,las=1,ylab="ln(cases)",xlab="Time",main=as.character(ACOMS[i,1]))
  mod0<-lm(logcas~dateN,data=datapays)
  te<-try(abline(mod0))
  if(!inherits(te,"try-error")) abline(te)
  dev.off()
  newdata<-data.frame(dateN=as.numeric(datapays[nrow(datapays),1]+prev))
  cat(unlist(as.character(ACOMS[i,1])),as.character(datapays[nrow(datapays),1]+prev),": ",round(exp(predict(mod0,newdata=newdata)),0),"\n")
  resPred[i,1]<-unlist(as.character(ACOMS[i,1]))
  resPred[i,2]<-as.character(datapays[nrow(datapays),1]+prev)
  resPred[i,3]<-round(exp(predict(mod0,newdata=newdata)),0)
  
}


library(pgirmess)
write.delim(resPred,"resPred.txt")

