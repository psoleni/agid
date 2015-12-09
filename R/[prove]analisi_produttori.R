# carica le librerie necessarie
# stringdist: utilzzata per la metrica sulle stringhe
library(stringdist)

# lettura, codifica in utf 8, considera header e sostiuisce celle vuote con NA
data <- read.csv("~/AgID/R/prodotti-salute-aggiornato[provePerR].csv", header=TRUE, sep=",",encoding="UTF-8", na.strings = "")

#considera solo i casi completi (salta le righe con NA)
data_complete <- data[complete.cases(data[,c("X.titolo.prodotto.","X.nome.produttore.")]),c("X.titolo.prodotto.","X.nome.produttore.")]
# quanti na ci sono?
# table(is.na(data))

#rinomina le colonne per praticità
colnames(data_complete)[1] <- "prodotti"
colnames(data_complete)[2] <- "produttori"


# definizione funzione fingerprint
keyFingerPrint<-function(s){
  s<-gsub("^\\s+|\\s+$", "", s); # trim string string 
  s<-tolower(s);# lowercase
  s<-gsub(pattern = "[[:punct:]]", replacement = "", s) # then remove all punctuation and control chars
  s<-gsub("\\s\\s+", " ",s) # remove empty spaces inside string
  tokens <- strsplit(s, " ")[[1]] # split by whitespaces
  # ordina e mette in lista senza duplicati
  tokens<-sort(unique(tokens))
  # collassa i risultati in una unica stringa
  s<-paste(tokens,collapse=" ") 
  # converte ad ascii
  s<-(iconv(s, to="ASCII//TRANSLIT"))
  # ripulisce dopo la conversione (workadround)
  # si può omettere se si fa prima la conversione?
  s<-gsub(pattern = "[[:punct:]]", replacement = "", s)
  s<-tolower(s);# lowercase
  return(s)
}


# definizione funzione ngram-fingerprint
ngramFingerPrint<-function(s,n=2){
  s<-gsub("^\\s+|\\s+$", "", s); # trim string string
  s<-(iconv(s, to="ASCII//TRANSLIT"))
  s<-tolower(s);# lowercase
  s<-gsub("\\s\\s+", " ",s) # remove empty spaces inside string
  s<-gsub(pattern = "[[:punct:]]", replacement = "", s) # then remove all punctuation and control chars
  
  tokens<-vector()
  size<-nchar(s)
  for (i in seq(1, size)){
    if(i+n-1>size){break}
    else{
      tokens<-c(tokens, (substring(s,i,i+n-1))) 
    }
  }
  tokens<-sort(unique(tokens))
  s<-paste(tokens,collapse="")
  return(s)
}



# aggiorna il dataset con il fingerprint della colonna produttori
data_complete$produttori_fingerprint<-sapply(data_complete$produttori,keyFingerPrint)


# ispeziona gli elementi con frequenza maggiore
t<-table(data_complete$produttori_fingerprint)
head(t[order(t,decreasing=TRUE)])

# calcolo della matrice delle distanze
uniqueProducers <- unique(as.character(data_complete$produttori_fingerprint))
distanceMatrixProducers <- stringdistmatrix(uniqueProducers,uniqueProducers,method = "jw")
rownames(distanceMatrixProducers) <- uniqueProducers
colnames(distanceMatrixProducers) <- uniqueProducers
# clustering e cutree
hc <- hclust(as.dist(distanceMatrixProducers), method="single")
cutree_height<-0.06
plot(hc)
# plot(as.dendrogram(hc), horiz = TRUE)
rect.hclust(hc,h=cutree_height)

dfClust <- data.frame(uniqueProducers, cutree(hc, h=cutree_height))
names(dfClust) <- c('produttori_fingerprint','cluster')
plot(table(dfClust$cluster))

data_ <- merge(data_complete,dfClust, by="produttori_fingerprint")


# range<-100:1000
# data_complete_small <- data_complete[range,]
# uniqueproducers_small <- unique(as.character(data_complete_small$produttori_fingerprint))
# distanceproducers_small <- stringdistmatrix(uniqueproducers_small,uniqueproducers_small,method = "jw")
# rownames(distanceproducers_small) <- uniqueproducers_small
# colnames(distanceproducers_small) <- uniqueproducers_small
# hc <- hclust(as.dist(distanceproducers_small), method="single")
# plot(hc)
# plot(as.dendrogram(hc), horiz = TRUE)
# rect.hclust(hc,h=0.06)


