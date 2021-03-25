# https://www.data.gouv.fr/fr/datasets/chiffres-cles-concernant-lepidemie-de-covid19-en-france/



# chifcles<-read.csv("https://www.data.gouv.fr/fr/datasets/r/0b66ca39-1623-4d9c-83ad-5434b7f9e2a4",header=TRUE, sep=",") # trop lent

chifcles<-read.csv("chiffres-cles.csv",header=TRUE, sep=",", encoding="UTF-8")
names(chifcles)
unique(chifcles[,"source_nom"])

head(chifcles)
unique(chifcles$maille_nom)

chif<-chifcles[chifcles$maille_nom=="Haute-Saône" | chifcles$maille_nom=="Jura"  | chifcles$maille_nom=="Doubs",]
names(chif)

chif$date2<-strptime(chif$date,format="%Y-%m-%d")

range(chif$date2)
plot(chif$date2[chif$maille_nom=="Doubs"],chif$cas_confirmes[chif$maille_nom=="Doubs"],type="h")
plot(chif$date2[chif$maille_nom=="Haute-Saône"],chif$cas_confirmes[chif$maille_nom=="Haute-Saône"],type="h")
plot(chif$date2[chif$maille_nom=="Doubs"],chif$cas_confirmes[chif$maille_nom=="Doubs"],type="h")