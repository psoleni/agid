
library(ggplot2)
library(stringr)
library(gridExtra)

### esempi sottostanti per gestione legenda
# sembra che con due layer di geom_point non si riesca ad ottenere la legenda.



# Get a map - A map of Canberra will do
ACTmap = get_map(c(149.1, -35.325), zoom = 12, source = "google", maptype = "roadmap")

# A data frame of lon and lat coordinates and the variable Size
df = data.frame(lon = c(149.0307, 149.1326, 149.089, 149.048, 149.0965),
                lat = c(-35.3892, -35.28225, -35.34005, -35.34857, -35.34833),
                Size = c(1,2,3,4,5))



library(gridExtra)
ggmap(get_map(c(149.1, -35.325), zoom = 12, source = "google", maptype = "roadmap")) +
  geom_point(data = df, aes(x = lon, y = lat, colour = Size), alpha = 0.6,  size = 10)

basemap <- ggmap(get_map(location = 'Italy',source = "google", maptype="terrain", color="bw" ,zoom=6), extent='panel')
regionelayer<- geom_point(data = cnt_regione, aes(x = V6, y = V5, colour=sqrt(cnt_regione$cntregione)), size =sqrt(cnt_regione$cntregione), alpha = 0.6)
basemap+regionelayer + labs(x = '', y = '') + ggtitle('Distribuzione dataset per regione')+ scale_color_gradient(low='yellow',high='red', name = "Numero di dataset")


provincialayer<- geom_point(data = cnt_provincia, aes(x = V6, y = V5, colour=sqrt(cnt_provincia$cnt_provincia)), size =sqrt(cnt_provincia$cnt_provincia))
basemap+provincialayer + labs(x = '', y = '') + ggtitle('Distribuzione dataset per provincia')+ scale_color_gradient(low='green',high='blue', name = "Numero di dataset")



completemap<-basemap + regionelayer + provincialayer
# grouping the plots together in one plot    
grid.arrange(provinciamap,regionemap,ncol=1)
