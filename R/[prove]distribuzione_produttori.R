require("RPostgreSQL")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "paolo"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "dbpa2",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw) # removes the password

query<-paste0("
SELECT 
              tipi_amministrazioni.codice_tipologia, 
              tipi_amministrazioni.nome, 
              tipi_amministrazioni.macro_categoria, 
              soggetti.denominazione, 
              soggetti.new, 
              formati.new, 
              formati.formato, 
              prodotti.titolo, 
              prodotti.new, 
              produttori.nome, 
              produttori.new, 
              applicativi.codice_licenza_app, 
              tipi_licenze_app.codice_licenza_app, 
              tipi_licenze_app.denominazione, 
              tipi_licenze_app.new, 
              applicativi.rilevanza, 
              applicativi.descrizione, 
              amministrazioni.codice_localizzazione, 
              localizzazioni.comune, 
              localizzazioni.provincia, 
              localizzazioni.regione, 
              localizzazioni.lat, 
              localizzazioni.lon
              FROM 
              pa.amministrazioni, 
              pa.tipi_amministrazioni, 
              pa.applicativi, 
              pa.basi_dati, 
              pa.soggetti, 
              pa.tipi_licenze_app, 
              pa.produttori, 
              pa.prodotti, 
              pa.localizzazioni, 
              pa.formati
              WHERE 
              amministrazioni.codice_ipa = basi_dati.codice_ipa AND
              amministrazioni.codice_localizzazione = localizzazioni.codice_localizzazione AND
              tipi_amministrazioni.codice_tipologia = amministrazioni.codice_tipologia AND
              applicativi.codice_bd = basi_dati.codice_bd AND
              applicativi.codice_prodotto = prodotti.codice_prodotto AND
              applicativi.codice_licenza_app = tipi_licenze_app.codice_licenza_app AND
              basi_dati.codice_formato = formati.codice_formato AND
              basi_dati.codice_soggetto = soggetti.codice_soggetto AND
              produttori.codice_produttore = prodotti.codice_produttore
              AND 
              tipi_amministrazioni.codice_tipologia in ('C7','L34','L22','L7','L8')
              ")

# execute query
data<-dbGetQuery(con, query)

# trasformazione in factor per aggregazioni
data$regione <- as.factor(data$regione)
data$provincia <- as.factor(data$provincia)

# numero di dataset per regione e per provincia
cnt_regione <- data.frame(table(data$regione), stringsAsFactors = FALSE)
colnames(cnt_regione) <- c("regione_fact","cntregione")
cnt_regione$regione<-as.character(cnt_regione$regione_fact)
cnt_provincia <- data.frame(table(data$provincia), stringsAsFactors = FALSE)
colnames(cnt_provincia) <- c("provincia","cntprovincia")

data<-merge(data, cnt_regione)
data<-merge(data, cnt_provincia)

# carica i dati di geonames
geonames_data = read.table("C:/Users/paolo/Documents/AgID/R/IT.txt",  header = FALSE, stringsAsFactors = FALSE ,sep="\t", encoding="UTF-8", quote="", comment.char="")
geonames_regioni <- geonames_data[geonames_data$V8=="ADM1",]
geonames_province <- geonames_data[geonames_data$V8=="ADM2",]

# merge: totali per provincia con dati geonames di provincia
cnt_provincia<-merge(geonames_province,cnt_provincia,by.x="V12",by.y="provincia")

# preprocessing dati regioni (alcuni nomi sono diversi)
#individua i corrispondenti in geonames in base alla distanza minima
perm_index<-sapply(cnt_regione$regione,FUN=agrep,geonames_regioni$V2)
for(i in 1:length(perm_index)){
  cnt_regione[i,"regione"]<-geonames_regioni[perm_index[i],"V2"]
}
# merge: totali per regione   
cnt_regione<-merge(geonames_regioni,cnt_regione,by.x="V2",by.y="regione")


library(ggmap)
map <- get_map(location = 'Italy', maptype="terrain", zoom=6, color = "bw")
mapPointsReg <- ggmap(map)  +   geom_point(aes(x = V6, y = V5), size =sqrt(cnt_regione$cntregione), col="red",data = cnt_regione, alpha = 0.4)
mapPointsProv <- mapPointsReg  +   geom_point(aes(x = V6, y = V5), size =sqrt(cnt_provincia$cntprovincia), col="blue",data = cnt_provincia, alpha = 0.4)

mapPointsProv # invocare per disegnare
  
  

# bounding box italia
# N 47.091945, 
# E 18.521666
# W 6.625556
# s 35.490000

# test
# 45.479453, 12.256045
x<-45.479453
y<-12.256045

library(sp)
it_adm0<-readRDS("C:/Users/paolo/Documents/AgID/R/ITA_adm0.rds") 
it_adm1<-readRDS("C:/Users/paolo/Documents/AgID/R/ITA_adm1.rds") 
spplot(it_adm1)

library(ggmap)
map <- get_map(location = 'Italy', maptype="roadmap", zoom=6)
mapPoints <- ggmap(map)  +   geom_point(aes(x = lon, y = lat), size =3, col="red",data = data, alpha = 0.4)
  #  +   geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = data, alpha = .5)
mapPoints # invocare per disegnare


# cercare qui il sistema per aggiungere l'informazione del numero di entry per comune

# t <- table(dfClust$cluster)
# t <- cbind(t,t/length(dfClust$cluster))
# t <- t[order(t[,2], decreasing=TRUE),]
# p <- data.frame(factorName=rownames(t), binCount=t[,1], percentFound=t[,2])
# dfClust <- merge(x=dfClust, y=p, by.x = 'cluster', by.y='factorName', all.x=T)
# dfClust <- dfClust[rev(order(dfClust$binCount)),]
# names(dfClust) <-  c('cluster','product')
# head (dfClust[c('cluster','product')],50)



# library(maps)
# library(mapdata)
# 
# data(italyMapEnv)
# map('italy', fill=TRUE, col="grey96")

