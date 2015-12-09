# install.packages("RPostgreSQL")
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
              soggetti.denominazione
              FROM 
              pa.soggetti
              ")
data<-dbGetQuery(con, query)


library(tm)
library(Matrix)

strings.to.cluster <- as.character(data$denominazione)
corpus <- Corpus( VectorSource( strings.to.cluster ) )
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('italian'))

dtm <- DocumentTermMatrix(corpus)
dtmatrix <- as.matrix(dtm)
frequency <- colSums(dtmatrix)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency,100)
findAssocs(dtm, names(head(frequency,30)),0.1)


plot(frequency)

dtm_clean <- removeSparseTerms(dtm, sparse=0.93)
dtm_clean_matrix <- as.matrix(dtm_clean)

distMatrix <-dist(scale(dtm_clean_matrix))
hc<-hclust(distMatrix,method="ward.D")
plot(hc)
rect.hclust(hc,k=20)
cutree(hc,k=20)



y <- sparseMatrix( i=x$i, j=x$j, x=x$v, dimnames = dimnames(x) )  
inspect(x[1:10,1:10])
findFreqTerms(x, 2000)
# plot( hclust(dist(t(y))) )