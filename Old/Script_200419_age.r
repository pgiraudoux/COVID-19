# https://www.covid.is/data

ages<-read.csv("./age structure/Sheet 1.csv")
names(ages)<-c("age","active","recov","death")
ages[is.na(ages)]<-0


ageF<-read.csv("https://www.data.gouv.fr/fr/datasets/r/08c18e08-6780-452d-9b8c-ae244ad529b3",sep=";")
head(ageF)
tail(ageF)
names(ageF)[2]<-"age"
# ageF[ageF$jour=="2020-04-07" & ageF$reg==1,]
ageF<-ageF[ageF$age!=0,]
ageF18<-aggregate(ageF[ageF$jour=="2020-04-18",c(4,6,7)],by=list(ageF$age[ageF$jour=="2020-04-18"]),sum)
colSums(ageF18)


unique(ageF$age)

ageI<-data.frame(rbind(colSums(ages[1:4,2:4]),ages[5:13,2:4]))
row.names(ageI)<-c("1-17",as.character(ages[5:13,1]))
ageI


barplot(t(ageI[,1:2]),las=1)

# France
ageF<-read.csv("https://www.data.gouv.fr/fr/datasets/r/08c18e08-6780-452d-9b8c-ae244ad529b3",sep=";")
head(ageF)
tail(ageF)
names(ageF)[2]<-"age"
# ageF[ageF$jour=="2020-04-07" & ageF$reg==1,]
ageF<-ageF[ageF$age!=0,]
ageF18<-aggregate(ageF[ageF$jour=="2020-04-18",c(4,6,7)],by=list(ageF$age[ageF$jour=="2020-04-18"]),sum)
colSums(ageF18)
catF<-paste0(ageF18[,1]-9,"-",ageF18[,1])
catF[c(1,length(catF))]<-c("<10","\u226590")
rownames(ageF18)<-catF

# Islande
ageI<-read.csv("./age structure/Sheet 1.csv")
names(ageI)<-c("age","active","recov","death")
ageI[is.na(ageI)]<-0

# Corée
ageK<-read.delim("./age structure/SouthKorea.txt")[1:9,1:3]
ageK<-ageK[9:1,]

barplot(t(ageI[,2:3]),names.arg=ageI[,1], las=1,main="Islande",legend=TRUE)
barplot(ageI[,2],names.arg=ageI[,1], las=1,main="Corée du sud (cases)",legend=TRUE)
barplot(t(ageF18[,2:3]),names.arg=catF,main="France",las=1, legend=TRUE)

Ikids<-sum(ageI[1:4,2:3])/sum(ageI[,2:3]) # proportion des < 18
Iseniors<-sum(ageI[9:13,2:3])/sum(ageI[,2:3]) # proportion des >= 60
1-Ikids-Iseniors

Kkids<-sum(ageK[1:2,2])/sum(ageK[,2]) # proportion des < 20
Kseniors<-sum(ageK[7:9,2])/sum(ageK[,2]) # proportion des >= 60
1-Kkids-Kseniors

        