
# lettura, codifica in utf 8, considera header e sostiuisce celle vuote con NA
data <- read.csv("~/AgID/R/prodotti-salute-aggiornato[provePerR].csv", header=TRUE, sep=",",encoding="UTF-8", na.strings = "")

# dummy dataset for test
produttori<-c("pippo","poppo","lallo","lullo")
prodotti<-c("prog1","prog2","web1","web 2.0")
data_complete<-data.frame(prodotti, produttori)
colnames(data_complete)<-c("X.titolo.prodotto.","X.nome.produttore.")

#considera solo i casi completi (salta le righe con NA)
data_complete <- data[complete.cases(data[,c("X.titolo.prodotto.","X.nome.produttore.")]),c("X.titolo.prodotto.","X.nome.produttore.")]
#rinomina le colonne per praticità
colnames(data_complete)[1] <- "prodotti"
colnames(data_complete)[2] <- "produttore"

unique(data_complete[,1])

# TODO pulire vettore di stringhe
# pulizie preliminari
library(stringr)
data_complete$prodotti<-iconv(data_complete$prodotti, to="ASCII//TRANSLIT")
data_complete$prodotti<-tolower(data_complete$prodotti)
data_complete$prodotti<-str_trim(data_complete$prodotti)
data_complete$prodotti<-gsub(pattern = "[[:punct:]]", replacement = " ", data_complete$prodotti)
data_complete$prodotti<-gsub("\\s\\s+", " ", data_complete$prodotti)


# fingerprint
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
  s<-gsub(pattern = "[[:punct:]]", replacement = "", s)
  s<-tolower(s);# lowercase
  return(s)
}


data_complete$prodotti_fingerprint<-sapply(data_complete$prodotti,keyFingerPrint)


# utilizza stringdist per la metrica sulle distanze ed i cluster
# 
library(stringdist)

range<-1:200
data_complete_small <- data_complete[range,]
uniqueproducts_small <- unique(as.character(data_complete_small$prodotti_fingerprint))
#distanceproducts_small <- stringdistmatrix(uniqueproducts_small,uniqueproducts_small,method = "jaccard", q=3)
distanceproducts_small <- stringdistmatrix(uniqueproducts_small,uniqueproducts_small,method = "jw")
rownames(distanceproducts_small) <- uniqueproducts_small
colnames(distanceproducts_small) <- uniqueproducts_small
hc <- hclust(as.dist(distanceproducts_small), method="single")
plot(hc)
rect.hclust(hc,h=0.1)


dfClsust <- data.frame(uniqueproducts_small, cutree(hc, k=1))
names(dfClust) <- c('product','cluster')
plot(table(dfClust$cluster))



# esempio più corposo
# 
# data_complete_medium <- data_complete[sample(1:nrow(data_complete), 1500,replace=FALSE),]
data_complete_medium <- data_complete
uniqueproducts_medium <- unique(as.character(data_complete_medium$prodotti))
distanceproducts_medium <- stringdistmatrix(uniqueproducts_medium,uniqueproducts_medium,method = "dl")
rownames(distanceproducts_medium) <- uniqueproducts_medium
colnames(distanceproducts_medium) <- uniqueproducts_medium
hc <- hclust(as.dist(distanceproducts_medium))
rect.hclust(hc,k=200)
dfClust <- data.frame(uniqueproducts_medium, cutree(hc, h=3))
names(dfClust) <- c('product','cluster')
plot(table(dfClust$cluster))

# mostra dendogram e step di clustering
par(mfrow=c(1,2))
plot(hc,xlab="", sub="")
plot(hc$height)
for(i in 1:length(hc$height)) lines(c(i,i),c(0,hc$height[i]),col=4)



# plot dendrogram with some cuts
hcd = as.dendrogram(hc)
op = par(mfrow = c(2, 1))
plot(cut(hcd, h = 0.05)$upper, main = "Upper tree of cut at h=75")
plot(cut(hcd, h = 0.05)$lower[[2]], main = "Second branch of lower tree with cut at h=75")



# mostra il contenuto dei cluster più corposi
t <- table(dfClust$cluster)
t <- cbind(t,t/length(dfClust$cluster))
t <- t[order(t[,2], decreasing=TRUE),]
p <- data.frame(factorName=rownames(t), binCount=t[,1], percentFound=t[,2])
dfClust <- merge(x=dfClust, y=p, by.x = 'cluster', by.y='factorName', all.x=T)
dfClust <- dfClust[rev(order(dfClust$binCount)),]
names(dfClust) <-  c('cluster','product')
head (dfClust[c('cluster','product')],50)








# controllo valori di soggetto ordinati in senso inverso
sort(table(data_complete$X.soggetto.), decreasing = TRUE)


d <- dist(as.matrix(data_complete))


# hc <- hclust(d,method="ward.D")
d <- dist(as.matrix(complete.cases(data[,c("X.titolo.prodotto.","X.nome.produttore.")])))
hc <- hclust(d,method="ward.D")
plot(hc)



#function for calculating Cramer's v
cramer <- function(y,x){
  K <- nlevels(y)
  L <- nlevels(x)
  n <- length(y)
  chi2 <- chisq.test(y,x,correct=F)
  print(chi2$statistic)
  v <- sqrt(chi2$statistic/(n*min(K-1,L-1)))
  return(v)
}



#similarity matrix
sim <- matrix(1,nrow=ncol(data_complete),ncol=ncol(data_complete))
rownames(sim) <- colnames(data_complete)
colnames(sim) <- colnames(data_complete)
for (i in 1:(nrow(sim)-1)){
  for (j in (i+1):ncol(sim)){
    y <- data_complete[,i]
    x <- data_complete[,j]
    sim[i,j] <- cramer(y,x)
    sim[j,i] <- sim[i,j]
  }
}
#distance matrix
dissim <- as.dist(1-sim)
#clustering
tree <- hclust(dissim,method="ward.D")
plot(tree)
