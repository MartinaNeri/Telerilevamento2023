
# codice per generare mappe sulla copertura del suolo (land cover, naturale, antropizzata, ecc) da immagini satellitari

library(raster)
install.packages("ggplot2") # pacchetto per la creazione di grafici più eleganti
library(ggplot2)
install.packages("patchwork")
library(patchwork) # To combine 2 ggplot graphs into the same graph

setwd("C:/lab/")

# importazione immagine Landsat del Rio Peixoto in Brasile nel 1992 (già usata in una lezione precedente)

I92 <- brick("defor1_.png") # l92 = immagine Landsat del 1992 
I92

# NIR   = 1
# Red   = 2
# Green = 3

plotRGB(I92, 1, 2, 3, stretch="lin") # abbiamo scritto 1, 2, 3 senza definire che siano r, g e b perchè di default la funzione li legge in modo corretto
# la vegetazione riflette molto nel vicino infrarosso quindi sappiamo che nell'immagine il rosso è il NIR (Near InfraRed) e corrisponde alla copertura vegetale

# importazione immagine Landsat del Rio Peixoto in Brasile nel 2006
I06 <- brick("defor2_.png")

# creazione del multiframe con le due immagini (con 2 righe e 1 colonna)
par(mfrow=c(2,1))
plotRGB(I92, 1, 2, 3, stretch="lin")
plotRGB(I06, 1, 2, 3, stretch="lin")
dev.off()

# i pixel dell'immagine vengono classificati campionandone una parte in modo casuale nel seguente modo:
# immaginando un grafico con la banda del rosso su x e il NIR su y
# i pixel rossi corrisponderanno ad alti valori di NIR e bassi di rosso (in alto a sx del grafico) -> vegetazione
# i pixel blu/grigi a bassi valori di NIR e alti valori nel rosso (in basso a dx) -> aree agricole/antropizzate
# tutti gli altri pixel che non rientrano nelle due categorie precedenti vengono associati in base alla distanza minima alla nuvola di pixel più vicina
# (principio della maximum likelihood) -> per calibrare il modello iniziale

# CLASSIFICATION OF THE 1992 IMAGE

# 1. Get values
singlenr1 <- getValues(I92)
singlenr1

# 2. Classify
kcluster1 <- kmeans(singlenr1, centers=2)
kcluster1

# 3. Recreating an image
I92_class <- setValues(I92[[1]], kcluster1$cluster)

# Plot the classified image
plot(I92_class)


# CLASSIFICATION OF THE 2006 IMAGE

# 1. Get values
singlenr2 <- getValues(I06)
singlenr2

# 2. Classify
kcluster2 <- kmeans(singlenr2, centers=2)
kcluster2

# 3. Recreating an image
I06_class <- setValues(I06[[1]], kcluster2$cluster)

# Plot the classified image
plot(I06_class)

# Create a multiframe
par(mfrow=c(2, 1))
plot(I92_class)
plot(I06_class)
dev.off()


# per vedere la differenza di copertura vegetale tra il 92 e il 2006 potremmo calcolare la proporzione di foresta presente 
# nelle due immagini utilizzando il numero di pixel delle due classi (che distinguono la foresta dalle zone antropizzate)
# e il numero totale di pixel presenti nelle due immagini satellitari


# Class percentage 1992
frequencies1 <- freq(I92_class)
frequencies1
#1 306297
#2  34995


tot1 <- ncell(I92_class)
tot1 # 341292

percentage1 <- (frequencies1*100)/tot1
percentage1

# Forest    = 89.74632%
# Bare soil = 10.25368%




# Class percentage 2006
frequencies2 <- freq(I06_class)
frequencies2
#1 178456
#2 164270

tot2 <- ncell(I06_class)
tot2 # 342726

percentage2 <- (frequencies2*100)/tot2
percentage2

# Forest    = 52.06958%
# Bare soil = 47.93042%

# Let's make a dataframe
cover <- c("Forest","Bare Soil")
perc_1992 <- c(89.75, 10.25)
perc_2006 <- c(52.07, 47.93)
percentages <- data.frame(cover, perc_1992, perc_2006)
percentages


# Plot using ggplot2

ggplot(percentages,
       aes(x=cover, y=perc_1992, color=cover)) +
  geom_bar(stat="identity",
           fill="white")
# we wanna know the IDENTITY for this kind of statistics, not the count

ggplot(percentages,
       aes(x=cover, y=perc_2006, color=cover)) +
  geom_bar(stat="identity",
           fill="white")

# Patchwork
p1 <- ggplot(percentages,
             aes(x=cover, y=perc_1992, color=cover)) +
  geom_bar(stat="identity",
           fill="white") +
  ggtitle("1992")

p2 <- ggplot(percentages,
             aes(x=cover, y=perc_2006, color=cover)) +
  geom_bar(stat="identity",
           fill="white") +
  ggtitle("2006")

# Let's put together p1 and p2
p1 + p2

# To standardize the y axes we use ylim() function


p1 <- ggplot(percentages, aes(x=cover, y=perc_1992, color=cover)) +
  geom_bar(stat="identity", fill="white") +
  ggtitle("Year 1992") +
  ylim(c(0,100))

p2 <- ggplot(percentages, aes(x=cover, y=perc_2006, color=cover)) +
  geom_bar(stat="identity", fill="white") + 
  ggtitle("Year 2006") +
  ylim(c(0,100))

p1 + p2
