
#### échelle normale

start<-100
paysRef<-ecdc[ecdc$countriesAndTerritories=="Italy",]
paysRef<-paysRef[order(paysRef$dateRep),]
firstcaseRef<-which(cumsum(paysRef$cases)>=start)[1]
par(mar=c(5.1,5.1,4.1,2.1))
plot(1:length(firstcaseRef:length(paysRef$dateRep)),cumsum(paysRef$cases)[firstcaseRef:length(paysRef$dateRep)],las=1,ylab="",xlab="Time (jours)",type="l",col="green")
mtext("Cases",2,line=3.5)


selec<-c("France","Spain","Germany")
cols<-c("green","blue","orange","brown")

for(i in 1:length(selec)){
  print(as.character(selec[i]))
  pays<-ecdc[ecdc$countriesAndTerritories==selec[i],]
  pays<-pays[order(pays$dateRep),]
  firstcase<-which(cumsum(pays$cases)>start)[1]
  if(!is.na(firstcase)) {
    lines(1:length(firstcase:length(pays$dateRep)),cumsum(pays$cases)[firstcase:length(pays$dateRep)],col=cols[i+1])
  }
}




compteur<-0
for(i in 1:nrow(ACOMS)){
print(as.character(ACOMS$country[i]))
pays<-ecdc[ecdc$countriesAndTerritories==as.character(ACOMS$country[i]),]
pays<-pays[order(pays$dateRep),]
firstcase<-which(cumsum(pays$cases)>start)[1]
if(!is.na(firstcase)) {
  compteur<-compteur+1
  lines(1:length(firstcase:length(pays$dateRep)),cumsum(pays$cases)[firstcase:length(pays$dateRep)])
}
}

legend("topleft",legend=c("African countries","Italy","France","Spain","Germany"), lty=1,col=c("black",cols),bty="n")



#### échelle logarithmique

start<-10
paysRef<-ecdc[ecdc$countriesAndTerritories=="Italy",]
paysRef<-paysRef[order(paysRef$dateRep),]
firstcaseRef<-which(cumsum(paysRef$cases)>=start)[1]
plot(1:length(firstcaseRef:length(paysRef$dateRep)),log10(cumsum(paysRef$cases)[firstcaseRef:length(paysRef$dateRep)]),las=1,ylab="log10(cases)",xlab="Time (jours)",type="l",col="green")


selec<-c("France","Spain","Germany")
cols<-c("green","blue","orange","brown")

for(i in 1:length(selec)){
  print(as.character(selec[i]))
  pays<-ecdc[ecdc$countriesAndTerritories==selec[i],]
  pays<-pays[order(pays$dateRep),]
  firstcase<-which(cumsum(pays$cases)>start)[1]
  if(!is.na(firstcase)) {
    compteur<-compteur+1
    lines(1:length(firstcase:length(pays$dateRep)),log10(cumsum(pays$cases)[firstcase:length(pays$dateRep)]),col=cols[i+1])
  }
}




compteur<-0
for(i in 1:nrow(ACOMS)){
  print(as.character(ACOMS$country[i]))
  pays<-ecdc[ecdc$countriesAndTerritories==as.character(ACOMS$country[i]),]
  pays<-pays[order(pays$dateRep),]
  firstcase<-which(cumsum(pays$cases)>start)[1]
  if(!is.na(firstcase)) {
    compteur<-compteur+1
    lines(1:length(firstcase:length(pays$dateRep)),log10(cumsum(pays$cases)[firstcase:length(pays$dateRep)]))
  }
}

legend("topleft",legend=c("African countries","Italy","France","Spain","Germany"), lty=1,col=c("black",cols),bty="n")



#### classification

library(ade4)

idx15j<-which(sapply(series10,length)>=15)

mat15j<-t(sapply(series10[idx15j],function(x) x[1:15]))

coa15j<-dudi.coa(mat15j,scannf=FALSE,nf=2)


par(mfrow=c(1,2))
scatter(coa15j,clab.col=0)

plot(coa15j$li[,1:2],type="n",asp=1)
abline(v=0,h=0,col="grey",lty=2)
text(coa15j$li[,1:2],labels=rownames(mat15j),cex=0.8)


plot(hclust(dist(coa15j$li)))


lapply(series10, function(x) x[1:15])

