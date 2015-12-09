# AgID Catalogue dataset analysis
# focusing on sanity-related administrations

# TODO aggiornare lista
# consistenza del dataset rispetto al bacino di dati
# distribuzione geografica dei dati
# varietà dei soggetti
# varietà dei formati
# prodotti: i 10 prodotti più usati per regione
# produttori: i 10 produttori più presenti per regione
# gli n prodotti+produttori più usati in generale
# gli n produttori più diffusi per numero di dataset
# gli n produttori più diffusi per copertura territoriale
# coperture territoriali

# author Paolo Soleni
# Univerista Ca Foscari
# 2015 11

# import moduli
library(ggplot2)
library(ggmap)
library(stringr)
library(sp)
library(grid)
library(xtable)

# path
inputPath<- "AgID/analisi dati socio-sanitari/input"
outputPath<- "AgID/analisi dati socio-sanitari/export"

# elementi comuni ai grafici
chart_title<-element_text(face="bold", size=20)
legend_title<-element_text(size = 14)
axis_label<-element_text(size = 12)
axis_tick_label<-element_text(size = 12, colour="black")
legend_item<-element_text(size = 12)


data <- read.csv(file=file.path(inputPath, "agid_dati_amministrazioni_sanita[refined].csv"), header=TRUE, sep=",",encoding="UTF-8", na.strings = "")
#
# consistenza del dataset
#
# creo variabile categoriale fattore codice_tipologia
amministrazioni_df <- read.csv(file=file.path(inputPath, "amministrazioni.csv"), header=TRUE, sep=",",encoding="UTF-8", na.strings = "")
amministrazioni_df$codice_tipologia_fact <- factor(amministrazioni_df$codice_tipologia)

# le tipologie di interesse sono quelle del codice_tipologia
amministrazioni_df$healthsector <- amministrazioni_df$codice_tipologia_fact %in% levels(data$codice_tipologia)

# creazione pie diagram
tipologie_table_df<-as.data.frame(table(amministrazioni_df$healthsector))
labels_tipologie_percent<-paste0(100*round(tipologie_table_df$Freq/sum(tipologie_table_df$Freq),3),"%")
labels_tipologie = paste(c("Altre tipologie","Amministrazioni di Area Sanitara"), labels_tipologie_percent)
y.breaks <- cumsum(tipologie_table_df$Freq) - tipologie_table_df$Freq/2
png(filename=file.path(outputPath, "1_amministrazioni.png"),width = 1200, height = 900)

ggplot(tipologie_table_df, aes(x=factor(1),y=Freq, fill=Var1))+
  geom_bar(width = 1,stat="identity")+
  coord_polar(theta="y") +
  xlab('') +
  ylab('') +
  labs(fill='Tipologia amministrazione')+
  scale_y_continuous(
    breaks=y.breaks,   # where to place the labels
    labels=labels_tipologie # the labels
  )+
  ggtitle('Cardinalità rispetto al totale delle Amministrazioni') +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        legend.position="none",
        plot.title=chart_title,
        axis.title=axis_label,
        axis.text=axis_tick_label
        )
dev.off()

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
geonames_data = read.table(file=file.path(inputPath, "IT.txt"),  header = FALSE, stringsAsFactors = FALSE ,sep="\t", encoding="UTF-8", quote="", comment.char="")
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

cnt_totali<-rbind(setNames(cnt_regione[,c("V5","V6","V8","cntregione")],c("lat", "lng","ADMtype","cnt")), setNames(cnt_provincia[,c("V5","V6","V8","cntprovincia")],c("lat", "lng","ADMtype","cnt")))
cnt_totali$ADMtype <- factor(cnt_totali$ADMtype)
png(filename=file.path(outputPath, "2_distribuzione_dati_dataset.png"),width = 1200, height = 900)
ggmap(get_map(location = 'Italy',source = "google", maptype="terrain", zoom=6), extent='panel') + 
  geom_point(data = cnt_totali, aes(x = lng, y = lat, colour=ADMtype, size=sqrt(cnt_totali$cnt)), alpha = 0.6) +
  scale_size_continuous("Numero di dataset",range = c(min(sqrt(cnt_totali$cnt)),max(sqrt(cnt_totali$cnt)))) +
  scale_colour_discrete('Raggruppamento', labels = c('Regioni','Province')) +
  labs(x = '', y = '') + 
  ggtitle('Distribuzione dataset per Regione e Provincia') +
  theme( axis.ticks = element_blank(),
         axis.text=element_blank(),
         plot.title=chart_title, 
         legend.text=legend_item, 
         legend.title=legend_title
         )
dev.off()


# ripartizione dei soggetti
# definisce la soglia di taglio per aggregare i soggetti sotto i 50 dataset
cut_threshold <- 30

soggetti<-table(data$soggetto)
top_soggetti<-soggetti[order(soggetti,decreasing=TRUE)]
top_soggetti_df <- as.data.frame(top_soggetti)
top_soggetti_df$soggetti<-factor(rownames(top_soggetti))
top_soggetti_df$top_soggetti_head <- ifelse(top_soggetti_df$top_soggetti > cut_threshold, levels(top_soggetti_df$soggetti)[top_soggetti_df$soggetti], "Altro")

threshold_cumsum<-sum(top_soggetti_df[top_soggetti_df$top_soggetti<=cut_threshold, "top_soggetti"])
top_soggetti_df$top_soggetti_head_cumsum <- as.numeric(ifelse(top_soggetti_df$top_soggetti > cut_threshold, top_soggetti_df$top_soggetti, threshold_cumsum))

# costruisce factor che raccoglie i casi marginali in un altra categoria
top_soggetti_df$top_soggetti_head <- ifelse(top_soggetti_df$top_soggetti > cut_threshold, levels(top_soggetti_df$soggetti)[top_soggetti_df$soggetti], "Altro")
# ridefinisce l'ordine del factor per priorità (serve per avere la legenda ordinata)
top_soggetti_df$top_soggetti_head <- factor(top_soggetti_df$top_soggetti_head,levels=unique(top_soggetti_df$top_soggetti_head))

top_soggetti_levels <- top_soggetti_df[1:length(levels(top_soggetti_df$top_soggetti_head)), "top_soggetti_head_cumsum"]

top_soggetti_chart_values <- top_soggetti_df[1:length(levels(top_soggetti_df$top_soggetti_head)),"top_soggetti_head_cumsum"]
top_soggetti_chart_total <- sum(top_soggetti_chart_values) 
labels_top_soggetti_percent<-paste0(100*round(top_soggetti_chart_values/top_soggetti_chart_total,3),"%")
labels_top_soggetti = paste(levels(top_soggetti_df$top_soggetti_head), labels_top_soggetti_percent, sep = "\n")

y.breaks <- cumsum(top_soggetti_levels) - top_soggetti_levels/2
png(filename=file.path(outputPath, "3_soggetti.png"),width = 1200, height = 900)
ggplot(top_soggetti_df, aes(x=factor(1), y=top_soggetti,fill=top_soggetti_head))+
  geom_bar(width = 1,stat="identity")+
  coord_polar(theta="y") +
  xlab('') +
  ylab('') +
  labs(fill='Soggetto') +
  scale_y_continuous(
    breaks=y.breaks,   # where to place the labels
    labels=labels_top_soggetti # the labels
  )+
  ggtitle('Tipologie di soggetti') +
  scale_fill_brewer(palette = "Oranges") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=chart_title,
        axis.title=axis_label,
        axis.text=axis_tick_label,
        legend.text=legend_item, 
        legend.title=legend_title
  )
dev.off()


# ripartizione dei formati
# definisce la soglia di taglio per aggregare i formati sotto tale soglia di numero di dataset
cut_threshold <- 30

formati<-table(data$formato)
top_formati<-formati[order(formati,decreasing=TRUE)]
top_formati_df <- as.data.frame(top_formati)
top_formati_df$formati<-factor(rownames(top_formati))
top_formati_df$top_formati_head <- ifelse(top_formati_df$top_formati > cut_threshold, levels(top_formati_df$formati)[top_formati_df$formati], "Altro")

threshold_cumsum<-sum(top_formati_df[top_formati_df$top_formati<=cut_threshold, "top_formati"])
top_formati_df$top_formati_head_cumsum <- as.numeric(ifelse(top_formati_df$top_formati > cut_threshold, top_formati_df$top_formati, threshold_cumsum))

# costruisce factor che raccoglie i casi marginali in un altra categoria
top_formati_df$top_formati_head <- ifelse(top_formati_df$top_formati > cut_threshold, levels(top_formati_df$formati)[top_formati_df$formati], "Altro")
# ridefinisce l'ordine del factor per priorità (serve per avere la legenda ordinata)
top_formati_df$top_formati_head<- factor(top_formati_df$top_formati_head,levels=unique(top_formati_df$top_formati_head))
# droppa dal factor i livelli non usati

top_formati_levels <- top_formati_df[1:length(levels(top_formati_df$top_formati_head)), "top_formati_head_cumsum"]

top_formati_chart_values <- top_formati_df[1:length(levels(top_formati_df$top_formati_head)),"top_formati_head_cumsum"]
top_formati_chart_total <- sum(top_formati_chart_values) 
labels_top_formati_percent<-paste0(100*round(top_formati_chart_values/top_formati_chart_total,3),"%")
labels_top_formati = paste(str_replace(levels(top_formati_df$top_formati_head)," ","\n"), labels_top_formati_percent)

y.breaks <- cumsum(top_formati_levels) - top_formati_levels/2
png(filename=file.path(outputPath, "4_formati.png"),width = 1200, height = 1200)
ggplot(top_formati_df, aes(x=factor(1), y=top_formati,fill=top_formati_head))+
  geom_bar(width = 1,stat="identity")+
  coord_polar(theta="y") +
  xlab('') +
  ylab('') +
  labs(fill='Formato') +
  scale_y_continuous(
    breaks=y.breaks,   # where to place the labels
    labels=labels_top_formati # the labels
   ) +
  ggtitle('Tipologie di formati') +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=chart_title,
        axis.title=axis_label,
        # axis.text=axis_tick_label,
        axis.text=element_text(size = 12, colour="black", hjust=0.5, vjust=0.5),
        legend.text=legend_item, 
        legend.title=legend_title,
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  )
dev.off()

# dopo le statistiche aggregate, effetua ulteriore filtro di pulizia
# elimina le righe che non presentano dati completi per prodotto, produttore, regione
data<-data[complete.cases(data[,c("prodotto", "produttore","regione")]),]

# suddivisione dei formati in base ai produttori
formati_produttori_list <- list() #create an empty list
for(fp in 1:length(levels(top_formati_df$top_formati_head))){
  fp_vec <-levels(top_formati_df$top_formati_head)
  #print(fp_vec[fp])
  fp_table<-table(data[data$formato==fp_vec[fp],"produttore"])
  fp_table_top<-head(fp_table[order(fp_table,decreasing=TRUE)],10)
  for(f in 1:length(fp_table_top)){
    vec=character()
    vec[1] = fp_vec[fp]
    vec[2] = unname(fp_table_top[f])
    vec[3] = names(fp_table_top[f])
    formati_produttori_list[[(fp-1)*length(fp_table_top)+f]] = vec
  }
}
formati_produttori_df<-do.call("rbind",formati_produttori_list)
formati_produttori_df<-as.data.frame(formati_produttori_df)
colnames(formati_produttori_df) <- c("formato_prodotto", "occorrenze","produttore")
formati_produttori_df$occorrenze<-as.numeric(as.character(formati_produttori_df$occorrenze))
formati_produttori_df<-formati_produttori_df[formati_produttori_df$occorrenze>0,]
print(xtable(formati_produttori_df, 
             caption = 'Formati Produttori',
             digits = 0
             ), 
      type = "html", 
      caption.placement = 'top',
      file=file.path(outputPath, "produttori_formati.html")
)

# prodotti: i 10 prodotti più usati per regione

prodotti<-table(data$prodotto, data$regione)
prodotti<-as.data.frame(prodotti)
prodotti<-prodotti[order(prodotti$Var2, -prodotti$Freq),]
top_prodotti_list<-by(prodotti, prodotti$Var2, head, n=10)
top_prodotti_table<-do.call(rbind,top_prodotti_list)
top_prodotti_table$Var1<-droplevels(top_prodotti_table$Var1) # droppa dal factor i livelli non usati
top_prodotti_table_nz<-top_prodotti_table[top_prodotti_table$Freq>0,]
top_prodotti_table_nz$Var1<-droplevels(top_prodotti_table_nz$Var1) # droppa dal factor i livelli non usati

top_prodotti_ordered<-tapply(top_prodotti_table_nz$Freq, top_prodotti_table_nz$Var1, FUN=sum)
top_prodotti_ordered<-top_prodotti_ordered[order(top_prodotti_ordered, decreasing=TRUE)]
top_prodotti_ordered_df <-as.data.frame(top_prodotti_ordered)
top_prodotti_ordered_df$names <- rownames(top_prodotti_ordered_df)
top_prodotti_table_nz<-merge(top_prodotti_table_nz, top_prodotti_ordered_df, by.x="Var1", by.y="names")
top_prodotti_table_nz$Var1 = factor(top_prodotti_table_nz$Var1, levels=unique(top_prodotti_table_nz[order(top_prodotti_table_nz$top_prodotti_ordered, decreasing=TRUE), "Var1"]))

png(filename=file.path(outputPath, "5_10_top_prodotti_regione.png"),width = 1200, height = 1200)
ggplot(top_prodotti_table_nz, aes(Var2, Var1, size=Freq)) + 
  geom_point() +
  xlab("Regioni") + ylab("Prodotti") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  ggtitle('I 10 prodotti più usati in ogni Regione\n (ordinati per numero di dataset complessivi)') +
  scale_size_continuous("Numero di dataset") +
  theme(axis.text.y=element_text(size=10, colour="black"),
        axis.text.x=element_text(size=12, colour="black", hjust=1, angle=30),
        panel.grid=element_line(colour="gray", size=1),
        panel.background = element_rect(fill="light gray"),
        plot.title=chart_title,
        legend.text=legend_item, 
        legend.title=legend_title
        )
dev.off()


# gli n prodotti+produttori più usati in generale
nrprod_prod<-50
data$prodotto_produttore <- paste(data$prodotto, data$produttore, sep="XXbyXX")
tab_prod_prod<-table(data$prodotto_produttore)
tab_top_prod_prod<-head(tab_prod_prod[order(tab_prod_prod,decreasing=TRUE)],nrprod_prod)
top_prod_prod_ita<-data[data$prodotto_produttore %in% names(tab_top_prod_prod),]
prodotti_produttori<-table(top_prod_prod_ita$prodotto_produttore, top_prod_prod_ita$regione)
prodotti_produttori<-as.data.frame(prodotti_produttori)
prodotti_produttori<-prodotti_produttori[prodotti_produttori$Freq>0,]

# ridefinisce l'ordine del factor per priorità (serve per avere la legenda ordinata) 
prodotti_produttori$Var1 = factor(prodotti_produttori$Var1, levels=unique(prodotti_produttori[order(prodotti_produttori$Freq, decreasing=TRUE), "Var1"]))
prodotti_produttori$Var1<-droplevels(prodotti_produttori$Var1)

png(filename=file.path(outputPath, "6_50_top_prodottiEproduttori_nazionali.png"),width = 1200, height = 1200)
ggplot(prodotti_produttori, aes(Var2, Var1, size=Freq)) + 
  geom_point() +
  xlab("Regioni") + ylab("Prodotti") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) str_replace(x,"XXbyXX","\n"))+
  scale_size_continuous("Numero di dataset") +
  ggtitle(paste("Prodotti\n(", nrprod_prod," maggiori a livello nazionale per numero di dataset)",sep=""))+
  theme(axis.text.y=element_text(size=10, colour="black"),
        axis.text.x=element_text(size=12, colour="black", hjust=1, angle=30),
        panel.grid=element_line(colour="gray", size=1),
        panel.background = element_rect(fill="light gray"),
        axis.title=axis_label,
        plot.title=chart_title,
        legend.text=legend_item, 
        legend.title=legend_title
  )
dev.off()


# prodotti: i 50 prodotti più diffusi
nrprod_aggregati<-50
data$prodotto_produttore <- paste(data$prodotto, data$produttore, sep="XXbyXX")
prodotti_aggregati_regioni<-aggregate(data["prodotto_produttore"], by=data[c("prodotto_produttore","regione")], FUN=length)
colnames(prodotti_aggregati_regioni) <- c("prodotto_produttore","regione","cnt_dataset_prodotto_in_regione")
prodotti_aggregati_regioni$prodotto_produttore<-as.factor(prodotti_aggregati_regioni$prodotto_produttore)
prodotti_aggregati_regioni$cnt_regione<-ave(prodotti_aggregati_regioni$cnt_dataset_prodotto_in_regione, prodotti_aggregati_regioni$prodotto_produttore,FUN=length)
colnames(prodotti_aggregati_regioni)[4]<-"cnt_regioni_presenza_prodotto"

# aggrego per nome prodotto_produttore per estrarre solo i primi n
prodotti_aggregati<-aggregate(prodotti_aggregati_regioni[c("prodotto_produttore")], by=prodotti_aggregati_regioni["prodotto_produttore"], FUN=length)
colnames(prodotti_aggregati) <- c("prodotto_produttore","cnt_regioni_presenza_prodotto")
top_prodotti_aggregati<-head(prodotti_aggregati[order(prodotti_aggregati$cnt_regioni_presenza_prodotto, decreasing=TRUE),],nrprod_aggregati)
# estrae le righe dal dataframe relative agli n prodotti più diffusi 
prodotti_aggregati_regioni_filtered<-prodotti_aggregati_regioni[prodotti_aggregati_regioni$prodotto_produttore %in% top_prodotti_aggregati$prodotto_produttore,]
# riordina secondo il factor per avere il grafico desiderato
prodotti_aggregati_regioni_filtered$prodotto_produttore = factor(prodotti_aggregati_regioni_filtered$prodotto_produttore, levels=unique(prodotti_aggregati_regioni_filtered[order(prodotti_aggregati_regioni_filtered$cnt_regioni_presenza_prodotto, decreasing=TRUE), "prodotto_produttore"]))
png(filename=file.path(outputPath, "7_50_top_prodotti_diffusi.png"),width = 1200, height = 1200)
ggplot(prodotti_aggregati_regioni_filtered, aes(regione, prodotto_produttore, size=cnt_dataset_prodotto_in_regione)) + 
  geom_point() +
  xlab("Regioni") + ylab("Prodotti") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_discrete(labels = function(x) str_replace(x,"XXbyXX"," - "))+
   ggtitle('I 50 prodotti più diffusi\n (ordinati per numero totale di regioni di copertura)') +
  scale_size_continuous("Numero di dataset") +
  theme(axis.text.y=element_text(size=10, colour="black"),
        axis.text.x=element_text(size=12, colour="black", hjust=1, angle=30),
        panel.grid=element_line(colour="gray", size=1),
        panel.background = element_rect(fill="light gray"),
        plot.title=chart_title,
        legend.text=legend_item, 
        legend.title=legend_title
  )
dev.off()

# produttori: i 10 produttori più presenti per regione

produttori<-table(data$produttore, data$regione)
produttori<-as.data.frame(produttori)
produttori<-produttori[order(produttori$Var2, -produttori$Freq),]
top_produttori_list<-by(produttori, produttori$Var2, head, n=10)
top_produttori_table<-do.call(rbind,top_produttori_list)
top_produttori_table$Var1<-droplevels(top_produttori_table$Var1) # droppa dal factor i livelli non usati
top_produttori_table_nz<-top_produttori_table[top_produttori_table$Freq>0,]

top_produttori_ordered<-tapply(top_produttori_table_nz$Freq, top_produttori_table_nz$Var1, FUN=sum)
top_produttori_ordered<-top_produttori_ordered[order(top_produttori_ordered, decreasing=TRUE)]
top_produttori_ordered_df <-as.data.frame(top_produttori_ordered)
top_produttori_ordered_df$names <- rownames(top_produttori_ordered_df)
top_produttori_table_nz<-merge(top_produttori_table_nz, top_produttori_ordered_df, by.x="Var1", by.y="names")
top_produttori_table_nz$Var1 = factor(top_produttori_table_nz$Var1, levels=unique(top_produttori_table_nz[order(top_produttori_table_nz$top_produttori_ordered, decreasing=TRUE), "Var1"]))
png(filename=file.path(outputPath, "8_10_top_produttori_regione.png"),width = 1200, height = 1200)
ggplot(top_produttori_table_nz, aes(Var2, Var1, size=Freq)) + 
  geom_point() +
  xlab("Regioni") + ylab("Produttori [10 maggiori per regione]") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))  +
  scale_y_discrete(labels = function(x) strtrim(x, width = 30)) +
  scale_size_continuous("Numero di dataset") +
  ggtitle('I 10 produttori più diffusi in ogni Regione\n (ordinati per numero di dataset complessivi)') +
  theme(axis.text.y=element_text(size=10, colour="black"),
        axis.text.x=element_text(size=12, colour="black", hjust=1, angle=30),
        panel.grid=element_line(colour="gray", size=1),
        panel.background = element_rect(fill="light gray"),
        axis.title=axis_label,
        plot.title=chart_title,
        legend.text=legend_item, 
        legend.title=legend_title
  )
dev.off()

# gli n prodotti più usati in generale
# nrprod<-20
# tab_prod<-table(data$prodotto)
# tab_top_prod<-head(tab_prod[order(tab_prod,decreasing=TRUE)],nrprod)
# top_prodotti_ita<-prodotti[prodotti$Var1 %in% names(tab_top_prod),]
# top_prodotti_ita<-top_prodotti_ita[top_prodotti_ita$Freq>0,]
# # ridefinisce l'ordine del factor per priorità (serve per avere la legenda ordinata) 
# top_prodotti_ita$Var1 = factor(top_prodotti_ita$Var1, levels=unique(top_prodotti_ita[order(top_prodotti_ita$Freq, decreasing=TRUE), "Var1"]))
# top_prodotti_ita$Var1<-droplevels(top_prodotti_ita$Var1)
# png(filename=file.path(outputPath, "7_20_top_prodotti_nazionali.png"),width = 1200, height = 900)
# ggplot(top_prodotti_ita, aes(Var2, Var1, size=Freq)) + 
#   geom_point() +
#   xlab("Regioni") + ylab("Prodotti") +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
#   scale_size_continuous("Numero di dataset") +
#   ggtitle(paste("Prodotti \n(", nrprod," maggiori a livello nazionale)",sep=""))+
#   theme(axis.text.y=element_text(size=10, colour="black"),
#         axis.text.x=element_text(size=12, colour="black", hjust=1, angle=30),
#         panel.grid=element_line(colour="gray", size=1),
#         panel.background = element_rect(fill="light gray"),
#         axis.title=axis_label,
#         plot.title=chart_title,
#         legend.text=legend_item, 
#         legend.title=legend_title
#   )
# dev.off()


# gli n produttori più diffusi per numero di dataset

nrproduttori<-20
tab_produttori<-table(data$produttore)
tab_tab_produttori<-head(tab_produttori[order(tab_produttori,decreasing=TRUE)],nrproduttori)

top_produttori_ita<-produttori[produttori$Var1 %in% names(tab_tab_produttori),]
top_produttori_ita<-top_produttori_ita[top_produttori_ita$Freq>0,]
top_produttori_ita$cnt_dataset<-ave(top_produttori_ita$Freq, top_produttori_ita$Var1,FUN=sum)

# ridefinisce l'ordine del factor per priorità (serve per avere la legenda ordinata) 
top_produttori_ita$Var1 = factor(top_produttori_ita$Var1, levels=unique(top_produttori_ita[order(top_produttori_ita$cnt_dataset, decreasing=TRUE), "Var1"]))
top_produttori_ita$Var1<-droplevels(top_produttori_ita$Var1)
png(filename=file.path(outputPath, "9_20_top_produttori_per_nr_dataset.png"),width = 1200, height = 900)
ggplot(top_produttori_ita, aes(Var2, Var1, size=Freq)) + 
  geom_point() +
  xlab("Regioni") + ylab("Produttori") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_discrete(labels = function(x) strtrim(x, width = 30))+
  scale_size_continuous("Numero di dataset") +
  ggtitle(paste("Produttori \n(", nrproduttori," maggiori a livello nazionale per numero di dataset)",sep=""))+
  theme(axis.text.y=element_text(size=10, colour="black"),
        axis.text.x=element_text(size=12, colour="black", hjust=1, angle=30),
        panel.grid=element_line(colour="gray", size=1),
        panel.background = element_rect(fill="light gray"),
        axis.title=axis_label,
        plot.title=chart_title,
        legend.text=legend_item, 
        legend.title=legend_title
  )
dev.off()

# gli n produttori più diffusi per copertura territoriale
nrproduttori_terr<-20
produttori_regioni<-table(data$produttore, data$regione)
produttori_regioni<-ifelse(produttori_regioni>0, 1,0)

produttori_territori<-margin.table(produttori_regioni,1)
top_produttori_territori<- head(produttori_territori[order(produttori_territori, decreasing=TRUE)],nrproduttori_terr)

top_produttori_regioni<-produttori[produttori$Var1 %in% names(top_produttori_territori),]
top_produttori_regioni<-top_produttori_regioni[top_produttori_regioni$Freq>0,]
top_produttori_regioni$Var1 = factor(top_produttori_regioni$Var1, levels=names(top_produttori_territori))

png(filename=file.path(outputPath, "10_20_top_produttori_per_copertura.png"),width = 1200, height = 900)
ggplot(top_produttori_regioni, aes(Var2, Var1, size=Freq)) + 
  geom_point() +
  xlab("Regioni") + ylab("Produttori") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  scale_y_discrete(labels = function(x) strtrim(x, width = 30))+
  scale_size_continuous("Numero di dataset") +
  ggtitle(paste("Produttori \n(", nrproduttori_terr," maggiori a livello nazionale per copertura)",sep=""))+
  theme(axis.text.y=element_text(size=10, colour="black"),
        axis.text.x=element_text(size=12, colour="black", hjust=1, angle=30),
        panel.grid=element_line(colour="gray", size=1),
        panel.background = element_rect(fill="light gray"),
        axis.title=axis_label,
        plot.title=chart_title,
        legend.text=legend_item, 
        legend.title=legend_title
  )
dev.off()

  
# import dati shapefile regione
it_adm1<-readRDS(file=file.path(inputPath, "ITA_adm1.rds"))
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
# definisce le mappe di base
base_plot<-ggplot()
base_poly<-geom_polygon(data=it_regione.df, aes(x=long, y=lat, group=group), alpha=0.2, color='white', fill="white")
# gradient per i colori in base alla scala
colourgradient<-scale_fill_gradient(name="Numero di dataset",limits=c(min(top_produttori_regioni$Freq), max(top_produttori_regioni$Freq)), low="blue", high="red")

for (p in 1:length(top_produttori_territori)) {
  regioni_names_vector<-as.character(top_produttori_regioni[top_produttori_regioni$Var1==names(top_produttori_territori[p]),"Var2"])
  regioni_values_vector<-top_produttori_regioni[top_produttori_regioni$Var1==names(top_produttori_territori[p]),"Freq"]

  for (r in 1:length(regioni_names_vector)) {
    IDvalue<-mappa_id_regione[mappa_id_regione$NAME_1==regioni_names_vector[r],"ID_1"]
    it_regione.df$value[it_regione.df$id==IDvalue]<-regioni_values_vector[r]
  }
  png(filename=file.path(outputPath, paste("copertura_",names(top_produttori_territori[p]),".png", sep="")),width = 1200, height = 900)
  copertura_map<-geom_polygon(data=it_regione.df[it_regione.df$id %in% mappa_id_regione[mappa_id_regione$NAME_1 %in% regioni_names_vector,"ID_1"],], aes(x=long, y=lat, group=group, fill=value), alpha=0.2, color='white')
  map<-base_plot + base_poly + copertura_map +  coord_map() + colourgradient +
    labs(x = '', y = '') + 
    ggtitle(paste('Distribuzione dataset per produttore',names(top_produttori_territori[p]))) +
    theme( axis.ticks = element_blank(),
           axis.text=element_blank(),
           plot.title=chart_title, 
           legend.text=legend_item, 
           legend.title=legend_title
    )
  print(map)
  dev.off()
  }

