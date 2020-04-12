
ecdc<-read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
head(ecdc[ecdc$countriesAndTerritories=="France",])
tail(ecdc[ecdc$countriesAndTerritories=="France",])
names(ecdc)[1]<-"dateRep"
ecdc$dateRep<-strptime(ecdc$dateRep,format="%d/%m/%Y")


"Uganda"



pays<-ecdc[ecdc$countriesAndTerritories=="Uganda",]
pays<-pays[order(pays$dateRep),]
plot(pays$dateRep,cumsum(pays$cases),las=1,type="l",las=1,xlab="",ylab="cases")

start<-1
firstcase<-which(cumsum(pays$cases)>=start)[1]

duree<-20
mescas<-cumsum(pays$cases)[(length(pays$cases)-duree+1):length(pays$cases)]
length(mescas)
datapays<-data.frame(dateN=1:duree,cas=mescas,logcas=log(mescas),lldate=log(1:duree))
plot(datapays$dateN,datapays$cas)

modL<-lm(cas~dateN,data=datapays)
modE<-lm(logcas~dateN,data=datapays)
modsL<-lm(cas~lldate,data=datapays)
modLL<-lm(logcas~lldate,data=datapays)
R2s<-c(summary(modL)$adj.r.squared,summary(modE)$adj.r.squared, summary(modsL)$adj.r.squared, summary(modLL)$adj.r.squared)
R2s
modAs<-nls(cas~SSasymp(dateN, Asym, resp0, lrc),data=datapays)


plot(log(1:20),log(datapays$cas))

sum(abs(resid(modL)))
sum(exp(abs(resid(modE))))
sum(exp(abs(resid(modLL))))
sum(abs(resid(modAs)))


par(mfrow=c(2,2))
plot(datapays$dateN,datapays$cas)
abline(modL)

plot(datapays$dateN,datapays$logcas)
abline(modE)

plot(datapays$lldate,datapays$logcas)
abline(modLL)

plot(datapays$dateN,datapays$cas)
lines(datapays$dateN,predict(modAs))

