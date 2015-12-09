




# rappresentazione copertura 
library(sp)
library(rgdal)

it_adm1<-readRDS("C:/Users/paolo/Documents/AgID/R/ITA_adm1.rds") # regione
#it_adm2<-readRDS("C:/Users/paolo/Documents/AgID/R/ITA_adm2.rds") # provincia
#it_adm3<-readRDS("C:/Users/paolo/Documents/AgID/R/ITA_adm3.rds") # comune
#plot(it_adm3)

#it_province <- spTransform(it_adm2, CRS("+proj=longlat +datum=WGS84"))
#it_province.df <- fortify(it_province)

it_regione <- spTransform(it_adm1, CRS("+proj=longlat +datum=WGS84"))
it_regione.df <- fortify(it_regione)

# corregge i nomi differenti
it_regione$NAME_1[2]<-"Puglia"
it_regione$NAME_1[6]<-"Emilia Romagna"
it_regione$NAME_1[7]<-"Friuli Venezia Giulia"
it_regione$NAME_1[15]<-"Sicilia"
it_regione$NAME_1[17]<-"Trentino Alto Adige"
it_regione$NAME_1[19]<-"Valle D'Aosta"

#estrae la mappa id-regione 
mappa_id_regione<-it_regione@data[,c("NAME_1","ID_1")]


base_plot<-ggplot()
base_poly<-geom_polygon(data=it_regione.df, aes(x=long, y=lat, group=group), alpha=0.2, color='white', fill="white")
# test
# poly<-geom_polygon(data=it_regione.df[it_regione.df$id==mappa_id_regione[mappa_id_regione$NAME_1=="Molise","ID_1"],], aes(x=long, y=lat, group=group), alpha=0.2, color='white', fill="red")
# + base_poly +poly


# vettore dei colori
color_vec<-rainbow(length(names(tab_top_prod)), alpha=0.2)

# vettore dei poligoni
poly_list = list()
for (i in 1:length(names(tab_top_prod))){
  poly_list[[i]] = list()
  #cat("i:",i)
  regioni_prodotto<-top_prodotti_ita[top_prodotti_ita$Var1==names(tab_top_prod[i]),"Var2"]
  for (j in 1:length(regioni_prodotto)){
    #cat("j:",j)
    poly_list[[i]][[j]]<-geom_polygon(data=it_regione.df[it_regione.df$id==mappa_id_regione[mappa_id_regione$NAME_1==regioni_prodotto[j],"ID_1"],], aes(x=long, y=lat, group=group), alpha=0.2, color='white', fill=color_vec[i])
  }
}
# -- ahimé la sovrapposizione crea degli effetti troppo illeggibili
base_plot + base_poly + poly_list[[1]][[1]] + poly_list[[1]][[2]] + poly_list[[2]][[1]] + poly_list[[3]][[1]] + poly_list[[4]][[1]]


# TODO
# provare a rappresentare i 20 prodotti più venduti in assoluto rispetto alla loro distribuzione sulle regioni
# (ovvero come il spin plot già fatto, ma stavolta con meno valori sulle ordinate)

# altra cosa è provare a riusare la presentazione su mappa ma con meno prodotti, 20 sono troppi.

# idea della rappresentazione con shape file potrebbe essere riusata per produttori se sono frammentati