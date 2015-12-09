# carica le librerie necessarie
# stringdist: utilzzata per la metrica sulle stringhe
library(stringdist)

# lettura, codifica in utf 8, considera header e sostiuisce celle vuote con NA
data <- read.csv("~/AgID/R/prodotti-salute-aggiornato[provePerR].csv", header=TRUE, sep=",",encoding="UTF-8", na.strings = "")

#considera solo i casi completi (salta le righe con NA)
data_complete <- data[complete.cases(data[,c("X.titolo.prodotto.","X.nome.produttore.")]),c("X.titolo.prodotto.","X.nome.produttore.")]
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

# aggiorna il dataset con il fingerprint della colonna prodotti
data_complete$prodotti_fingerprint<-sapply(data_complete$prodotti,keyFingerPrint)


# ispeziona gli elementi con frequenza maggiore
t<-table(data_complete$prodotti_fingerprint)
head(t[order(t,decreasing=TRUE)])



# calcolo della matrice delle distanze
uniqueProducts <- unique(as.character(data_complete$prodotti_fingerprint))
distanceMatrixProducts <- stringdistmatrix(uniqueProducts,uniqueProducts,method = "jw")
rownames(distanceMatrixProducts) <- uniqueProducts
colnames(distanceMatrixProducts) <- uniqueProducts
# clustering e cutree
hc <- hclust(as.dist(distanceMatrixProducts), method="single")
cutree_height<-0.1
plot(hc)
rect.hclust(hc,h=cutree_height)

dfClust <- data.frame(uniqueProducts, cutree(hc, h=cutree_height))
names(dfClust) <- c('prodotti_fingerprint','cluster')
plot(table(dfClust$cluster))

data_ <- merge(data_complete,dfClust, by="prodotti_fingerprint")







# mostra il contenuto dei cluster più corposi
t <- table(dfClust$cluster)
t <- cbind(t,t/length(dfClust$cluster))
t <- t[order(t[,2], decreasing=TRUE),]

# il problema è nelle stringhe corte, che vengono subito aggregate in cluster


# attenzione a considerare solo i prodotti perché bisogna abbinare anche il produttore accanto per vedere se si tratta proprio dello stesso prodotto

# rappresentare l'aggregazione dei produttori rispetto all'area geografica di riferimento della amministrazione

# vale la pena fare una analisi delle corrispondenze / multiple o no che siano?
# library(FactoMineR)?

