#### France

# https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/#_

read.delim("region2020.csv",sep=",",encoding="UTF-8")
urlage<-"https://www.data.gouv.fr/fr/datasets/r/08c18e08-6780-452d-9b8c-ae244ad529b3"

cov19age<-read.table(urlage,sep=";",header=TRUE)
cov19age<-cov19age[cov19age$cl_age90!=0,]
cov19age<-cov19age[cov19age$reg>=11,]

cov19age<-cov19age[cov19age$reg==27,]

sum(cov19age[cov19age$jour=="2020-03-20",4])



head(cov19age,20)

unique(cov19age$cl_age90)
unique(cov19age$reg)

aglim<-69
suplim<-cov19age$cl_age90>=aglim

legd<-c(paste0(">= ",aglim," ans"),paste0("< ",aglim," ans"))


cov19agr<-aggregate(cov19age[,4:7],by=list(age=suplim,jour=cov19age$jour),sum)
cov19agr$jour<-strptime(cov19agr$jour,format="%Y-%m-%d")
temps<-c(cov19agr[1,2],cov19agr[nrow(cov19agr),2])
greenhorns<-cov19agr[!cov19agr$age,]
oldtimers<-cov19agr[cov19agr$age,]


par(mfrow=c(1,1))

plot(range(cov19agr$jour),c(0,max(oldtimers$hosp+greenhorns$hosp)/1000),xlab="",ylab="Nombre",las=1,type="n",main=paste0("Nombre de patients hospitalisés (x 1000)\n",format(temps[1],"%d %b")," - ",format(temps[2],"%d %b")),xaxt="n")
segments(as.numeric(oldtimers$jour),0,as.numeric(oldtimers$jour),(oldtimers$hosp+greenhorns$hosp)/1000)
segments(as.numeric(greenhorns$jour),0,as.numeric(greenhorns$jour),greenhorns$hosp/1000,col="green")
axis.POSIXct(1,oldtimers$jour)

legend(locator(n=1),fill=c("black","green"),legend=legd,bty="n")

