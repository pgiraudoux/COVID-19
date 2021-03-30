library(rgdal)
library(pgirmess)
library(dplyr)
library(RColorBrewer)
# library(classInt)
library(wordcloud)


###récupération des unités IRIS Besançon

# irisFrance<-readOGR("../Map tools/IRIS/CONTOURS-IRIS_2-1__SHP__FRA_2019-01-01/CONTOURS-IRIS_2-1__SHP__FRA_2020-01-01/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2020-01-00139/CONTOURS-IRIS_2-1_SHP_LAMB93_FXX-2019","CONTOURS-IRIS",encoding="UTF-8",use_iconv=TRUE)
# head(irisFrance@data)
# comB<-read.delim("CommunesGrandBesancon.txt")

# idx<-irisFrance@data$INSEE_COM%in%comB$INSEE_COM

# irisBes<-(irisFrance[idx,])
# plot(irisBes)
# rm(irisFrance)

urliris<-"https://www.data.gouv.fr/fr/datasets/r/44d4c265-24c3-4720-9144-f3e4a5213422"



### Nom des zones GB

jpeg("GB.jpg",width=15,height=15, units="cm", res=300)
par(mar=c(0,0,0,0))
plot(irisBes,lwd=0.1)
plot(irisBes[irisBes$NOM_COM=="Besançon",],col="grey",border="grey",add=TRUE)
# text(coordinates(irisBes)[irisBes$NOM_COM!="Besançon",],irisBes$NOM_IRIS[irisBes$NOM_COM!="Besançon"],cex=0.7)
textplot(coordinates(irisBes)[irisBes$NOM_COM!="Besançon",1],coordinates(irisBes)[irisBes$NOM_COM!="Besançon",2],words=irisBes$NOM_IRIS[irisBes$NOM_COM!="Besançon"],cex=0.7,new=FALSE)
dev.off()

### Nom des zones Besançon
jpeg("Bes.jpg",width=15,height=15, units="cm", res=300)
par(mar=c(0,0,0,0))
plot(irisBes[irisBes$NOM_COM=="Besançon",],lwd=0.1)
# text(coordinates(irisBes)[irisBes$NOM_COM=="Besançon",],irisBes$NOM_IRIS[irisBes$NOM_COM=="Besançon"],cex=0.7)
textplot(coordinates(irisBes)[irisBes$NOM_COM=="Besançon",1],coordinates(irisBes)[irisBes$NOM_COM=="Besançon",2],words=irisBes$NOM_IRIS[irisBes$NOM_COM=="Besançon"],cex=0.5,new=FALSE)
dev.off()

# irisdb0<-read.delim("sg-iris-opendata-2021-03-13-17h22.csv",sep=",") # lecture locale (téléchargement ~23 s à https://www.data.gouv.fr/fr/datasets/donnees-de-laboratoires-infra-departementales-durant-lepidemie-covid-19/#_)
# 
# head(irisdb0)



irisdb<-read.delim("sg-iris-opendata-2021-03-26-18h14.csv",sep=",") # lecture locale (téléchargement ~23 s à https://www.data.gouv.fr/fr/datasets/donnees-de-laboratoires-infra-departementales-durant-lepidemie-covid-19/#_)

# to<-Sys.time()
# irisdb<-read.table(urliris, sep=",",header=TRUE) #URL stable
# t1<-Sys.time()
# t1-to
# Time difference of 6.653344 mins


dim(irisdb)
unique(irisdb$clage_65)
sort(unique(irisdb$ti_classe))

head(irisdb)
tail(irisdb)

# extraction Besançon
idx2<-irisdb$iris2019%in%irisBes@data$CODE_IRIS
irisdb<-irisdb[idx2,]
irisdb$date<-strptime(substr(irisdb$semaine_glissante,12,21),format="%Y-%m-%d")


############### Générique

datesuivies<-sort(unique(irisdb$date))
range(irisdb$date)
datesuivies
length(datesuivies)/7

agesel<-65 # choix de la classe d'âge (0 = tous âges ou 65 = plus de 65)
ndate<-6 # nombre de dates
irisdbsel<-irisdb[irisdb$clage_65==agesel,]
irisdbsel$date<-strptime(substr(irisdbsel$semaine_glissante,12,21),format="%Y-%m-%d")
head(irisdbsel)

select<-seq(length(datesuivies),1,l=ndate) # sélection de dates

# Tableau d'évolution

ti.db<-matrix(nrow=length(unique(irisdb[,"iris2019"])),ncol=ndate)
ii<-1
for(i in select) {
  mydate<-unique(irisdb$date)[i]
  print(mydate)
  tmp<-irisdbsel[irisdb$date==mydate,]
  idxti<-match(irisBes$CODE_IRIS,irisdbsel[irisdbsel$date==mydate,"iris2019"])
  valti<-irisdbsel[irisdbsel$date==mydate,"ti_classe"][idxti]
  ti.db[,ii]<-valti
  ii<-ii+1
}
rownames(ti.db)<-irisBes$NOM_IRIS
colnames(ti.db)<-as.character(unique(irisdb$date)[select])
ti.db


### tableaux des fréquences par catégories
titab<-matrix(0,ncol=8,nrow=ndate)
colnames(titab)<-c("[0;10[", "[10;20[", "[20;50[", "[50;150[", "[150;250[", "[250;500[", "[500;1000[","[1000;Max]")
rownames(titab)<-colnames(ti.db)

restitab<-apply(ti.db,2,table)

for(i in 1:length(restitab)){
  idx<-match(c("[0;10[", "[10;20[", "[20;50[", '[50;150[', "[150;250[", "[250;500[", "[500;1000[","[1000;Max]"),names(restitab[[i]]))
  titab[i,]<-restitab[[i]][idx]
}
titab[is.na(titab)]<-0
titab

rowSums(titab[,6:8])/rowSums(titab)

ttdatestab65<-table(irisdb$date[irisdb$clage_65==65],irisdb$ti_classe[irisdb$clage_65==65])[,2:9][,c(1,2,5,7,4,6,8,3)]
ttdatestab<-table(irisdb$date[irisdb$clage_65==0],irisdb$ti_classe[irisdb$clage_65==0])[,2:9][,c(1,2,5,7,4,6,8,3)]


tsages<-rowSums(ttdatestab[,6:8])/rowSums(ttdatestab)
agesup65<-rowSums(ttdatestab65[,6:8])/rowSums(ttdatestab65)

plot(datesuivies,tsages,type="l",ylim=range(tsages,agesup65),las=1,ylab="ratio d'unités IRIS à incidence \u2265 250",xlab="",col="green3")
lines(datesuivies,agessup65,col="black")
legend(list(x=as.numeric(min(datesuivies)),y=0.6),legend=c("tous âges","plus de 65 ans"),lty=1,col=c("green3","black"),bty="n",title="")

pal<-brewer.pal(8,"Reds")

## carte toutes catégories
par(mfrow=c(3,2))
par(mar=c(0,0,0,0))
for(i in select) {
  mydate<-unique(irisdbsel$date)[i]
  print(mydate)
  idxti<-match(irisBes$CODE_IRIS,irisdbsel[irisdbsel$date==mydate,"iris2019"])
  valti<-irisdbsel[irisdbsel$date==mydate,"ti_classe"][idxti]
  colBes<-recode(valti, "[0;10[" = pal[1], "[10;20["=pal[2], "[20;50["=pal[3], '[50;150[' = pal[4], "[150;250["=pal[5], "[250;500["=pal[6], "[500;1000["=pal[7],"[1000;Max]"=pal[8])
  colBes[colBes==""]<-NA
  plot(irisBes,col=colBes)
  plot(irisBes[64,], col="grey",add=TRUE)
  text(coordinates(irisBes[64,]),"Chailluz",cex=0.7)
  mtext(mydate,line=-2,font=2)
  legend(list(x=909000,y=6705000),fill=c(pal,"grey"),legend=c(ti_class[c(9,5,4,1,2,3,8,6)],"no data"),bty="n",title="")
}


## carte seuillage réduit
par(mfrow=c(2,3))
par(mar=c(0,0,0,0))
for(i in select) {
  mydate<-unique(irisdbsel$date)[i]
  print(mydate)
  idxti<-match(irisBes$CODE_IRIS,irisdbsel[irisdbsel$date==mydate,"iris2019"])
  valti<-irisdbsel[irisdbsel$date==mydate,"ti_classe"][idxti]
  colBes<-recode(valti, "[0;10[" = "green", "[10;20["="green", "[20;50["="green", '[50;150[' = "orange", "[150;250["="orange", "[250;500["="red", "[500;1000["="red","[1000;Max]"="violet")
  colBes[colBes==""]<-NA
  plot(irisBes,col=colBes)
  plot(irisBes[64,], col="grey",add=TRUE)
  text(coordinates(irisBes[64,]),"Chailluz",cex=0.7)
  mtext(mydate,line=-2,font=2)
  legend(list(x=909000,y=6705000),fill=c("green","orange","red","violet","grey"),legend=c("<50","[50, 250[","[250, 1000[",">=1000","no data"),bty="n",title="")
}




# Video
# magick convert -delay 70 *.jpg video.mp4

agesel<-65 # choix de la classe d'âge (0 = tous âges ou 65 = plus de 65)
irisdbsel<-irisdb[irisdb$clage_65==agesel,]
irisdbsel$date<-strptime(substr(irisdbsel$semaine_glissante,12,21),format="%Y-%m-%d")
head(irisdbsel)
select<-sort(unique(irisdbsel$date))

for(i in 1:length(select)) {
  jpeg(paste0("./video/",select[i],"_",agesel,".jpg"))
  par(mar=c(0,0,0,0))
  mydate<-select[i]
  print(mydate)
  idxti<-match(irisBes$CODE_IRIS,irisdbsel[irisdbsel$date==mydate,"iris2019"])
  valti<-irisdbsel[irisdbsel$date==mydate,"ti_classe"][idxti]
  colBes<-recode(valti, "[0;10[" = "green", "[10;20["="green", "[20;50["="green", '[50;150[' = "orange", "[150;250["="orange", "[250;500["="red", "[500;1000["="red","[1000;Max]"="violet")
  colBes[colBes==""]<-NA
  plot(irisBes,col=colBes)
  plot(irisBes[64,], col="grey",add=TRUE)
  text(coordinates(irisBes[64,]),"Chailluz",cex=0.7)
  mtext(mydate,line=-2,font=2)
  legend(list(x=909000,y=6701000),fill=c("green","orange","red","violet","grey"),legend=c("<50","[50, 250[","[250, 1000[",">=1000","no data"),bty="n",title="")
  dev.off()
}




#        
# irisdb0<-irisdb[irisdb$clage_65==0,]
# irisdb65-irisdb[irisdb$clage_65==65,]
# 
# irisdb0$date<-strptime(substr(irisdb0$semaine_glissante,12,21),format="%Y-%m-%d")
# irisdb65$date<-strptime(substr(irisdb65$semaine_glissante,12,21),format="%Y-%m-%d")
# 
# ## recodage taux d'incidence
# 
# ti_class<-unique(irisdb0$ti_classe)
# 
# 
# 
# irisdb0$ti_recode<-recode(irisdb0$ti_classe, "[0;10[" = 1, "[10;20["=2, "[20;50["=5, '[50;150[' = 15, "[150;250["=25, "[250;500["=50, "[500;1000["=100,"[1000;Max]"=1000)
# 
# 
# mydate<-"2021-03-10"
# idxti<-match(irisBes$CODE_IRIS,irisdb0[irisdb0$date==mydate,"iris2019"])
# # valti<-irisdb0$ti_recode[idxti]
# # pal<-brewer.pal(5,"Reds")
# # # q5<-classIntervals(resPred[idx,"pred7"],n=5,style="fixed",fixedBreaks=c(0,100,500,1000,2500,100000))
# # q5<-classIntervals(valti,n=3,style="jenks")
# # # plot(q5,pal)
# # q5Colours<-findColours(q5,pal)
# # plot(irisBes,col=q5Colours)
# 
# legend("topleft",fill=c(attr(q5Colours,"palette"),"grey"),legend=c(names(attr(q5Colours,"table")),"no data"),bty="n",title="")
# legend("bottomright",paste0(ti_class[c(9,5,4,1,2,3,8,6,7)]," = ",c(1,2,5,15,25,50,100,1000,"no data")),bty="n",title="Encodage")
# 
# 
# valti<-irisdb0$ti_classe[idxti]
# 
# pal<-brewer.pal(8,"Reds")
# 
# colBes<-recode(valti, "[0;10[" = pal[1], "[10;20["=pal[2], "[20;50["=pal[3], '[50;150[' = pal[4], "[150;250["=pal[5], "[250;500["=pal[6], "[500;1000["=pal[7],"[1000;Max]"=pal[8],)
# 
# 
# plot(irisBes,col=colBes,main=mydate)
# legend(list(x=910089.9,y=6702608),fill=c(pal,"grey"),legend=c(ti_class[c(9,5,4,1,2,3,8,6)],"no data"),bty="n",title="taux d'incidence tous âges")
# 
# 
# 

# 
# 
# range(irisdb0$date)
# datesuivies<-unique(irisdb0$date)
# select<-seq(length(datesuivies),1,l=4) # sélection de dates
# 
# 
# # Tableau d'évolution
# 
# ti.db<-matrix(nrow=length(irisdb0[irisdb0$date==mydate,"iris2019"]),ncol=4)
# ii<-1
# for(i in select) {
#   mydate<-unique(irisdb0$date)[i]
#   print(mydate)
#   tmp<-irisdb0[irisdb0$date==mydate,]
#   idxti<-match(irisBes$CODE_IRIS,irisdb0[irisdb0$date==mydate,"iris2019"])
#   valti<-irisdb0[irisdb0$date==mydate,"ti_classe"][idxti]
#   ti.db[,ii]<-valti
#   ii<-ii+1
# }
# 
# ti.db
# rownames(ti.db)<-irisBes$NOM_IRIS
# colnames(ti.db)<-as.character(unique(irisdb0$date)[seq(31,1,l=4)])
# 
# 
# 
# ## toutes catégories
# par(mfrow=c(2,2))
# par(mar=c(0,0,0,0))
# for(i in select) {
#   mydate<-unique(irisdb0$date)[i]
#   print(mydate)
#   idxti<-match(irisBes$CODE_IRIS,irisdb0[irisdb0$date==mydate,"iris2019"])
#   valti<-irisdb0[irisdb0$date==mydate,"ti_classe"][idxti]
#   colBes<-recode(valti, "[0;10[" = pal[1], "[10;20["=pal[2], "[20;50["=pal[3], '[50;150[' = pal[4], "[150;250["=pal[5], "[250;500["=pal[6], "[500;1000["=pal[7],"[1000;Max]"=pal[8])
#   colBes[colBes==""]<-NA
#   plot(irisBes,col=colBes)
#   plot(irisBes[64,], col="grey",add=TRUE)
#   text(coordinates(irisBes[64,]),"Chailluz",cex=0.7)
#   mtext(mydate,line=-2,font=2)
#   legend(list(x=909000,y=6705000),fill=c(pal,"grey"),legend=c(ti_class[c(9,5,4,1,2,3,8,6)],"no data"),bty="n",title="")
# }
# 
# 
# ## seuillage réduit
# par(mfrow=c(2,2))
# par(mar=c(0,0,0,0))
# for(i in select) {
#   mydate<-unique(irisdb0$date)[i]
#   print(mydate)
#   idxti<-match(irisBes$CODE_IRIS,irisdb0[irisdb0$date==mydate,"iris2019"])
#   valti<-irisdb0[irisdb0$date==mydate,"ti_classe"][idxti]
#   colBes<-recode(valti, "[0;10[" = "green", "[10;20["="green", "[20;50["="green", '[50;150[' = "orange", "[150;250["="orange", "[250;500["="red", "[500;1000["="red","[1000;Max]"="violet")
#   colBes[colBes==""]<-NA
#   plot(irisBes,col=colBes)
#   plot(irisBes[64,], col="grey",add=TRUE)
#   text(coordinates(irisBes[64,]),"Chailluz",cex=0.7)
#   mtext(mydate,line=-2,font=2)
#   legend(list(x=909000,y=6705000),fill=c("green","orange","red","violet","grey"),legend=c("<50","[50, 250[","[250, 1000[",">=1000","no data"),bty="n",title="")
# }
# 
# 
# 
# 
#        
#        
# #
# to<-Sys.time()
# iristmp<-read.table(urliris, sep=",",header=TRUE,colClasses=c("character","character","integer","character","character","character"),comment.char="") #URL stable
# t1<-Sys.time()
# t1-to
# # Time difference of 6.482594 mins
# 
# to<-Sys.time()
# iristmp <- scan(url(urliris),sep=',',what=list("character","character","integer","character","character","character"),comment.char="") #URL stable
# t1<-Sys.time()
# t1-to
# # Time difference of 6.517613 mins        