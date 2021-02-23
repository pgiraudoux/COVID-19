library(rgdal)
Afmap<-readOGR("./Africa_SHP","Africa")
plot(Afmap)
Afmap@data


# write.delim(data.frame(Afmap@data$COUNTRY,AC$country[match(Afmap@data$COUNTRY,AC$country)]),"./Africa_SHP/Countryconv.txt")

conver<-read.delim("./Africa_SHP/Countryconv.txt")
head(conver)
tail(conver)

head(Afmap@data)
Afmap@data<-data.frame(Afmap@data,ECDC=conver[,2])


writeOGR(Afmap,"./Africa_SHP","Africa_ECDC","ESRI Shapefile")


ECDCmap<-readOGR("./Africa_SHP","Africa_cont_ECDC")


#### carte avec Sud Soudan, etc.

Afmap<-readOGR("./africa_national_boundary","africa_national_boundary_cont_simple")


Afmap@data<-read.delim("./africa_national_boundary/Countryconv.txt")

plot(Afmap)
text(coordinates(Afmap),labels=Afmap@data$ECDC)

writeOGR(Afmap,"./africa_national_boundary","africa_national_boundary_cont_simple","ESRI Shapefile")


ECDCmap<-Afmap
save(ECDCmap,file="ECDCmap.Rdata")
#### Cartes


noOMS<-is.na(ECDCmap@data$ECDC)| ECDCmap@data$ECDC=="Djibouti" | ECDCmap@data$ECDC=="Egypt" | ECDCmap@data$ECDC=="Libya" | ECDCmap@data$ECDC=="Morocco" | ECDCmap@data$ECDC=="Somalia" | ECDCmap@data$ECDC=="Sudan" | ECDCmap@data$ECDC=="Tunisia"
nodata<-ECDCmap@data$ECDC=="Lesotho" | ECDCmap@data$ECDC=="Western_Sahara" | ECDCmap@data$ECDC=="South_Sudan" | ECDCmap@data$ECDC=="Malawi"

resPred[!resPred[,1]%in%ECDCmap@data$ECDC,1]



idx<-match(ECDCmap@data$ECDC,resPred[,1])

# commencer ici pour update
# 
# values<-resPred[idx,"ncasP"]
# valuesJ<-resPred[idx,"ncasJ"]
# ValuesR<-resPred[idx,"r2adj"]
# model<-toupper(substr(resPred[idx,"model"],1,1))

library(RColorBrewer)
library(classInt)

pal<-brewer.pal(5,"Reds")
q5<-classIntervals(resPred[idx,"ncasP"],n=5,style="fixed",fixedBreaks=c(0,100,1000,5000,10000,25000))
plot(q5,pal)
q5Colours<-findColours(q5,pal)

par(mar=c(0,0,0,0))
plot(ECDCmap,col=q5Colours)
# text(coordinates(ECDCmap),labels=ECDCmap$ECDC)
# plot(ECDCmap[noOMS,],col="grey",add=TRUE)
plot(ECDCmap[nodata,],col="grey",add=TRUE)
text(coordinates(ECDCmap),labels=ifelse(!is.na(resPred[idx,"ncasP"]),paste0(toupper(substr(resPred[idx,"model"],1,1)),resPred[idx,"ncasJ"],"->",resPred[idx,"ncasP"],"\n",resPred[idx,"r2adj"]),""),cex=0.7)
# legend("bottomleft",fill=c(attr(q5Colours,"palette"),"grey"),legend=c(names(attr(q5Colours,"table")),"non WHO Africa or NA"),bty="n",title="Cases 16/4/20 (predicted)")
legend("bottomleft",fill=c(attr(q5Colours,"palette"),"grey"),legend=c(names(attr(q5Colours,"table")),"no data"),bty="n",title=paste0("Cases ",resPred$dateP[1]," (predicted)"))

text(coordinates(ECDCmap),labels=round(coordinates(ECDCmap)[,2],1),cex=0.7)
# legend("bottomleft",fill=c(attr(q5Colours,"palette"),"grey"),legend=c(names(attr(q5Colours,"table")),"non WHO Africa or NA"),bty="n",title="Cases 16/4/20 (predicted)")

write.delim(ECDCmap@data,"clipboard")

write.delim(data.frame(ecdc=unique(ecdc$countriesAndTerritories)),"clipboard")

##### avec bandeau


windows(10,8)
layout(matrix(c(1,2,2,1,2,2), ncol=3,nrow=2, byrow = TRUE))

plot(rnorm(30),rnorm(30),type="n")

par(mar=c(0,0,0,0))
plot(ECDCmap,col=q5Colours)
# plot(ECDCmap[noOMS,],col="grey",add=TRUE)
plot(ECDCmap[nodata,],col="grey",add=TRUE)
text(coordinates(ECDCmap),labels=ifelse(!is.na(values),paste0(toupper(substr(resPred[idx,"model"],1,1)),resPred[idx,"ncasJ"],"->",resPred[idx,"ncasP"],"\n",resPred[idx,"r2adj"]),""),cex=0.7)
# legend("bottomleft",fill=c(attr(q5Colours,"palette"),"grey"),legend=c(names(attr(q5Colours,"table")),"non WHO Africa or NA"),bty="n",title="Cases 16/4/20 (predicted)")
legend("bottomleft",fill=c(attr(q5Colours,"palette"),"grey"),legend=c(names(attr(q5Colours,"table")),"no data"),bty="n",title=paste0("Cases ",resPred$dateP[1]," (predicted)"))




