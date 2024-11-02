
#https://www.globalforestwatch.org/dashboards/country/ROU/27/15/?map=eyJjYW5Cb3VuZCI6dHJ1ZX0%3D

# Installazione dei packages necessari
install.packages("raster")  # To work with raster images - functions like 'raster', 'stack', 'crop', 'plotRGB', 'plot', 'values', 'focal', 'aggregate', 'rasterPCA'
install.packages("ggplot2") # To make plots - functions like 'ggplot2', 'geom_bar', 'labs', 'scale_fill_manual', 'geom_text', 'theme_minimal' 
install.packages("patchwork") # To easily pair plots
install.packages("viridis") # To plot images and graphs with color blind friendly palettes
install.packages("RStoolbox") # for rasterPCA function

# Caricamento dei packages installati
library(raster)
library(ggplot2)
library(patchwork)
library(viridis)
library(RStoolbox)

# Impostare la working directory
setwd("C:/lab/Esame_telerilevamento")

#Bălan, Romania
#B2 - Blue  [1]
#B3 - Green [2]
#B4 - Red   [3]
#B8 - NIR   [4]
#B8A -      [5]
#B11 - SWIR [6]
#B12 - SWIR [7]

### IMMAGINI 2017
rlist_2017 <- list.files(pattern = "2017-09-01")
rlist_2017

# Applico la funzione raster() all'intera lista
import_2017 <- lapply(rlist_2017, raster)
import_2017

# Unione di tutte le bande presenti nella lista in un solo oggetto
img_2017 <- stack(import_2017)

# Visualizzo le informazioni
img_2017

plot(img_2017)

#2024

rlist_2024 <- list.files(pattern = "2024-08-25")
rlist_2024

import_2024 <- lapply(rlist_2024, raster)
import_2024

img_2024 <- stack(import_2024)

img_2024

plot(img_2024)

# Plot e salvataggio in PDF
pdf("img_2017.pdf")
par(mfrow = c(1, 2))
plotRGB(img_2017, 3, 2, 1, stretch = "lin")
plotRGB(img_2017, 4, 3, 2, stretch = "lin")
dev.off()

pdf("img_2024.pdf")
par(mfrow = c(1, 2))
plotRGB(img_2024, 3, 2, 1, stretch = "lin")
plotRGB(img_2024, 4, 3, 2, stretch = "lin")
dev.off()

#Pot per tipo
par(mfrow = c(1, 2))
plotRGB(img_2017, 3, 2, 1, stretch = "lin")
plotRGB(img_2024, 3, 2, 1, stretch = "lin")
dev.off()

par(mfrow = c(1, 2))
plotRGB(img_2017, 4, 3, 2, stretch = "lin")
plotRGB(img_2024, 4, 3, 2, stretch = "lin")
dev.off()

#### 2. Calcolo degli indici spettrali ####

# 2.1 Calcolo del DVI (Differenza Vegetazione) per 2017 e 2024
DVI_2017 <- img_2017[[4]] - img_2017[[3]]  # NIR - Rosso
DVI_2024 <- img_2024[[4]] - img_2024[[3]]
DVI_fin <- DVI_2024 - DVI_2017

# Visualizzazione del DVI
viridis_palette <- colorRampPalette(viridis(7))(255)  # Colore per DVI
par(mfrow = c(1, 2))
plot(DVI_2017, col = viridis_palette, main = "DVI 2017")
plot(DVI_2024, col = viridis_palette, main = "DVI 2024")
dev.off()

# 2.2 Calcolo del NDVI per il 2017 e il 2024
NDVI_2017 <- DVI_2017 / (img_2017[[4]] + img_2017[[3]])
NDVI_2024 <- DVI_2024 / (img_2024[[4]] + img_2024[[3]])

# Visualizzazione del NDVI
par(mfrow = c(1, 2))
plot(NDVI_2017, col = viridis_palette, main = "NDVI 2017")
plot(NDVI_2024, col = viridis_palette, main = "NDVI 2024")
dev.off()

#### Calcolo della perdita di copertura vegetale ####

# Parametri di input
soglia <- -0.2
ndvi_iniziale <- NDVI_2017
ndvi_finale <- NDVI_2024

# Calcolo della differenza NDVI tra l'anno iniziale e l'anno finale
differenza_ndvi <- ndvi_finale - ndvi_iniziale

# Identificazione dei pixel con perdita significativa
perdita <- differenza_ndvi < soglia

# Calcolo del numero di pixel con perdita di copertura vegetale
numero_pixel_perduti <- cellStats(perdita, sum)

# Calcolo della percentuale di area con perdita di copertura
percentuale_area_perduta <- (numero_pixel_perduti / ncell(perdita)) * 100

# Creazione di una lista con i risultati
perdita_2017_2024 <- list(numero_pixel_perduti = numero_pixel_perduti,
                          percentuale_area_perduta = percentuale_area_perduta)

# Stampa dei risultati
print(perdita_2017_2024)

#output
#$numero_pixel_perduti
#[1] 87475
#Questo valore rappresenta il numero totale di pixel in cui la perdita di NDVI
#supera la soglia impostata (soglia = -0.2). Quindi 87475 pixel hanno 
#subito una perdita significativa di NDVI (ossia, un degrado della vegetazione).

#$percentuale_area_perduta
#[1] 2.082738
#Questa è la percentuale dell'area totale coperta da perdita di vegetazione 
#rispetto al totale dei pixel. In questo caso, il risultato è 2.082738%, 
#quindi più del 2% dell'area totale monitorata ha subito una perdita 
#significativa di vegetazione tra il 2017 e il 2018.

#### 3. Calcolo e visualizzazione EVI e differenza ####

# Definisco i coefficienti per l'EVI
C1 <- 6
C2 <- 7.5
L <- 1
G <- 2.5  # Fattore di guadagno

# Calcolo dell'EVI per il 2017
EVI_2017 <- G * ((img_2017[[4]] - img_2017[[3]]) / (img_2017[[4]] + C1 * img_2017[[3]] - C2 * img_2017[[1]] + L))

# Calcolo dell'EVI per il 2024
EVI_2024 <- G * ((img_2024[[4]] - img_2024[[3]]) / (img_2024[[4]] + C1 * img_2024[[3]] - C2 * img_2024[[1]] + L))

# Calcolo della differenza EVI tra 2017 e 2024
EVI_diff <- EVI_2024 - EVI_2017

# Visualizzazione EVI
par(mfrow = c(1, 3))
plot(EVI_2017, col = viridis_palette, main = "EVI 2017")
plot(EVI_2024, col = viridis_palette, main = "EVI 2024")
plot(EVI_diff, col = viridis_palette, main = "Differenza EVI 2017-2024")
dev.off()

#### 4. Analisi PCA delle differenze NDVI e DVI ####

# Imposta il seme per la riproducibilità
set.seed(1)

#  Crea un oggetto "box" con le differenze tra gli indici di due anni
NDVI_diff <- NDVI_2024 - NDVI_2017
DVI_diff <- DVI_2024 - DVI_2017

# Unisci le differenze degli indici in un unico stack raster
box <- stack(NDVI_diff, DVI_diff)

# Traccia il grafico delle differenze per capire la variazione tra i due anni
plot(box, col = viridis_palette, main = "Differenze degli indici NDVI e DVI tra 2017 e 2024")
dev.off()

#  Campionamento casuale di punti dal raster "box" per accelerare l'analisi PCA
sr <- sampleRandom(box, 10000)

# Esegui l'Analisi delle Componenti Principali (PCA) sui campioni
pca <- prcomp(sr)

# Mostra un riepilogo della varianza spiegata dalla PCA
summary(pca)

#Traccia un grafico delle componenti principali
plot(pca)

# Applica il modello PCA all'intero raster "box" per ottenere le prime due componenti principali
pci <- predict(box, pca, index = 1:2)

#Visualizza la prima componente principale per vedere dove la variazione è maggiore
plot(pci[[1]], col = viridis, main = "Prima Componente Principale (PC1) - 2017 vs 2024")

#Converti la prima componente principale in data frame con coordinate x, y per ggplot
pc_final <- as.data.frame(pci[[1]], xy = TRUE)

#Visualizza la mappa della prima componente principale utilizzando ggplot

ggplot() + 
  geom_raster(data = pc_final, mapping = aes(x = x, y = y, fill = PC1)) + 
  scale_fill_viridis(option = "inferno") +
  labs(title = "Prima Componente Principale (PC1)", x = "Longitude", y = "Latitude")

# Salva la visualizzazione come file immagine
png("PCA_Deforestation.png", width = 800, height = 400)
ggplot() + 
  geom_raster(data = pc_final, mapping = aes(x = x, y = y, fill = PC1)) + 
  scale_fill_viridis(option = "inferno") +
  labs(title = "Prima Componente Principale (PC1)", x = "Longitude", y = "Latitude")
dev.off()

#### 5. T-test sui valori NDVI ####

# Estrazione dei valori NDVI come vettori
ndvi_2017_values <- values(NDVI_2017)
ndvi_2024_values <- values(NDVI_2024)

# Esecuzione del T-test sui valori NDVI tra 2017 e 2024
t_test_result <- t.test(ndvi_2017_values, ndvi_2024_values, paired = TRUE)
print(t_test_result)

##### 6.  LAND COVER ANALYSIS 

# Crea una palette colori per le classi
cl_freq <- colorRampPalette(c("blue", "yellow"))(2)

# Per ogni anno, estrae i valori delle bande, classifica con k-means, calcola le frequenze e percentuali
# 2017
single_nr_2017 <- getValues(img_2017)
k_cluster_2017 <- kmeans(single_nr_2017, centers = 2)
img_2017_class <- setValues(img_2017[[1]], k_cluster_2017$cluster)
plot(img_2017_class, col = cl_freq)
freq_2017 <- freq(img_2017_class)
perc_2017 <- round((freq_2017 * 100) / ncell(img_2017_class), digits = 5)

# 2024
single_nr_2024 <- getValues(img_2024)
k_cluster_2024 <- kmeans(single_nr_2024, centers = 2)
img_2024_class <- setValues(img_2024[[1]], k_cluster_2024$cluster)
plot(img_2024_class, col = cl_freq)
freq_2024 <- freq(img_2024_class)
perc_2024 <- round((freq_2024 * 100) / ncell(img_2024_class), digits = 5)

# Consolidare i dati in un DataFrame
copertura_vegetale <- c("Buona", "Ridotta/Assente")
Land_cover_perc <- data.frame(
  copertura_vegetale, 
  P_2017 = perc_2014[,2],
  P_2024 = perc_2024[,2]
)
print(Land_cover_perc)

# Plot delle percentuali di copertura per ogni anno e salvataggio in PDF
cl_barplot <- c("Buona" = "blue", "Ridotta/Assente" = "yellow")
pdf("Land_Cover_Percentages.pdf")

for (year in names(Land_cover_perc)[2:7]) {
  ggplot(Land_cover_perc, aes(x = copertura_vegetale, y = Land_cover_perc[[year]], fill = copertura_vegetale)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = cl_barplot) +
    labs(x = "Land Cover", y = "%", title = paste("Land Cover", year)) +
    theme(legend.position = "none") +
    ylim(0, 100) +
    geom_text(aes(label = sprintf("%.2f%%", Land_cover_perc[[year]]), y = Land_cover_perc[[year]]),
              position = position_stack(vjust = 0.5), size = 4)
}
dev.off()


################################ 6. LAND COVER #################################

### CLASSIFICAZIONE IMMAGINI 2017

# Estrazione dei valori dalle immagini del 2017
single_nr_2017 <- getValues(img_2017)

# Classificazione
k_cluster_2017 <- kmeans(single_nr_2017, centers = 2)

# Set dei valori
img_2017_class <- setValues(img_2017[[1]], k_cluster_2017$cluster)

# Plot
cl_freq <- colorRampPalette(c("blue", "yellow"))(2)
plot(img_2017_class, col = cl_freq, main = "Classificazione Land Cover 2017")

# Calcolo delle frequenze
freq_2017 <- freq(img_2017_class)

# Calcolo delle percentuali
perc_2017 <- round((freq_2017 * 100) / ncell(img_2017_class), digits = 5)

### CLASSIFICAZIONE IMMAGINI 2024

# Estrazione dei valori dalle immagini del 2024
single_nr_2024 <- getValues(img_2024)

# Classificazione
k_cluster_2024 <- kmeans(single_nr_2024, centers = 2)

# Set dei valori
img_2024_class <- setValues(img_2024[[1]], k_cluster_2024$cluster)

# Plot
plot(img_2024_class, col = cl_freq, main = "Classificazione Land Cover 2024")

# Calcolo delle frequenze
freq_2024 <- freq(img_2024_class)

# Calcolo delle percentuali
perc_2024 <- round((freq_2024 * 100) / ncell(img_2024_class), digits = 5)

### VISUALIZZAZIONE DEI RISULTATI

perc_2017  # Visualizzazione delle percentuali di copertura per il 2017
perc_2024  # Visualizzazione delle percentuali di copertura per il 2024

### CREAZIONE DI UN DATAFRAME CON I RISULTATI DELLA CLASSIFICAZIONE

# Creazione dei vettori
copertura_vegetale <- c("buona", "ridotta/assente")
P_2017 <- perc_2017[,2]
P_2024 <- perc_2024[,2]

# Creazione del dataframe
Land_cover_perc <- data.frame(
  copertura_vegetale, 
  P_2017 = P_2017,
  P_2024 = P_2024
)

# Visualizzazione del dataframe
print(Land_cover_perc)

### PLOTE DELLE PERCENTUALI DI LAND COVER + ESPORTAZIONE IN .pdf

cl_barplot <- c("buona" = "blue", "ridotta/assente" = "yellow")

# Creazione di un PDF per le percentuali di copertura del 2017 e 2024
pdf("Land_Cover_Percentages.pdf")

# Plot per 2017
ggplot(Land_cover_perc, aes(x = copertura_vegetale, y = P_2017, fill = copertura_vegetale)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cl_barplot) +
  labs(x = "Land Cover", y = "%", title = "Land Cover 2017") +
  theme(legend.position = "none") +
  ylim(0, 100) +
  geom_text(aes(label = sprintf("%.2f%%", P_2017), y = P_2017),
            position = position_stack(vjust = 0.5), size = 4)

# Plot per 2024
ggplot(Land_cover_perc, aes(x = copertura_vegetale, y = P_2024, fill = copertura_vegetale)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cl_barplot) +
  labs(x = "Land Cover", y = "%", title = "Land Cover 2024") +
  theme(legend.position = "none") +
  ylim(0, 100) +
  geom_text(aes(label = sprintf("%.2f%%", P_2024), y = P_2024),
            position = position_stack(vjust = 0.5), size = 4)

dev.off()
