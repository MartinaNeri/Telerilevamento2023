
#https://www.globalforestwatch.org/dashboards/country/ROU/27/15/?map=eyJjYW5Cb3VuZCI6dHJ1ZX0%3D

# Installazione dei packages necessari
install.packages("raster")  # To work with raster images - functions like 'raster', 'stack', 'crop', 'plotRGB', 'plot', 'values'
install.packages("ggplot2") # To make plots - functions like 'ggplot2', 'geom_bar', 'labs'
install.packages("patchwork") # To easily pair plots
install.packages("viridis") # To plot images and graphs with color blind friendly palettes
install.packages("RStoolbox") # for rasterPCA function

# Caricamento dei packages installati
library(raster)
library(ggplot2)
library(patchwork)
library(viridis)
library(RStoolbox)

# Imposto la working directory
setwd("C:/lab/Esame_telerilevamento")

#Bălan, Romania

#le bande delle immagini satellitari (Sentinel-2) scaricate da Copernicus browser sono:
#B2 - Blue  [1]
#B3 - Green [2]
#B4 - Red   [3]
#B8 - NIR   [4]
#B8A -      [5]
#B11 - SWIR [6]
#B12 - SWIR [7]

#viridis: sfumature dal blu al verde-giallo.
#plasma: sfumature dal blu scuro al rosso-giallo.
#magma: sfumature dal viola al rosso-arancio.
#cividis: ottimizzata per persone con deuteranopia e protanopia.


##### 1. CARICO LE IMMAGINI #####

#2017
#Carico le immagini con rlist selezionando tutti i file che contengono "2017-09-01" nel nome

rlist_2017 <- list.files(pattern = "2017-09-01")
rlist_2017

# Applico la funzione raster() all'intera lista
import_2017 <- lapply(rlist_2017, raster)
import_2017

# Unisco tutte le bande presenti nella lista in un solo oggetto
img_2017 <- stack(import_2017)

# Visualizzo le informazioni
img_2017

#plotto le bande caricate
plot(img_2017)
dev.off()

#2024
#Eseguo lo stesso procedimento per le bande dell'immagine del 2024

rlist_2024 <- list.files(pattern = "2024-08-25")
rlist_2024

import_2024 <- lapply(rlist_2024, raster)
import_2024

img_2024 <- stack(import_2024)

img_2024

plot(img_2024)
dev.off()

# Plot e salvataggio in PDF delle immagini sia con sia in colori veri (RGB naturale)
#sia a falsi colori, dove l’infrarosso vicino (NIR) sostituisce il canale rosso,
#mettondo in risalto la vegetazione.

#pdf("img_2017.pdf")
par(mfrow = c(1, 2))
plotRGB(img_2017, 3, 2, 1, stretch = "lin")
plotRGB(img_2017, 4, 3, 2, stretch = "lin")
dev.off()

#pdf("img_2024.pdf")
par(mfrow = c(1, 2)) ###################non so se ha senso ggrgb
ggRGB(img_2024, 3, 2, 1, stretch = "lin")
plotRGB(img_2024, 4, 3, 2, stretch = "lin")
#dev.off()

#Pot per tipo di immagini
par(mfrow = c(1, 2))
plotRGB(img_2017, 3, 2, 1, stretch = "lin")
plotRGB(img_2024, 3, 2, 1, stretch = "lin")
dev.off()

par(mfrow = c(1, 2))
plotRGB(img_2017, 4, 3, 2, stretch = "lin")
plotRGB(img_2024, 4, 3, 2, stretch = "lin")
dev.off()

##### 2. CALCOLO INDICI SPETTRALI #####

# 2.1 Calcolo del DVI (Difference Vegetation Index)

#Calcola la differenza tra la banda dell’infrarosso vicino (NIR) e la banda 
#rossa per identificare la densità di vegetazione.

DVI_2017 <- img_2017[[4]] - img_2017[[3]]  # NIR - Rosso
DVI_2024 <- img_2024[[4]] - img_2024[[3]]
DVI_fin <- DVI_2024 - DVI_2017 # Differenza nella densità di vegetazione tra 2024 e 2017

# Visualizzazione del DVI
viridis_palette <- colorRampPalette(viridis(7))(255)  # Colore per DVI
color <- pal_viridis(option = "plasma")(7)


par(mfrow = c(1, 3))
plot(DVI_2017, col = viridis_palette, main = "DVI 2017")
plot(DVI_2024, col = viridis_palette, main = "DVI 2024")
plot(DVI_fin, col = viridis_palette, main = "differenza DVI")
dev.off()

#DVI_2017: Ogni area con colori verso il viola rappresenta un valore di DVI più 
#basso per il 2017, mentre le aree tendenti al giallo indicano valori DVI più 
#alti per lo stesso anno.
#DVI_2024: Stessa logica per il 2024. Si possono notare le differenze nei livelli 
#rispetto al 2017, dato che le aree gialle potrebbero essersi espanse o ridotte.
#DVI_fin: Questo grafico rappresenta la "differenza DVI". Qui, il giallo indica 
#un aumento dei valori di DVI nel periodo (2017-2024), mentre le aree viola scure 
#rappresentano una diminuzione. I toni intermedi mostrano cambiamenti minori.


# 2.2 Calcolo del NDVI (Normalized Difference Vegetation Index)
# L'NDVI è calcolato come il rapporto normalizzato tra (NIR - RED) e (NIR + RED), 
# risultando in una scala che va da -1 a 1.
# Permette di confrontare la vegetazione su aree e periodi differenti, correggendo le variazioni dovute alle condizioni atmosferiche e alle caratteristiche del suolo.

NDVI_2017 <- DVI_2017 / (img_2017[[4]] + img_2017[[3]])
NDVI_2024 <- DVI_2024 / (img_2024[[4]] + img_2024[[3]])
NDVI_dif <- NDVI_2024 - NDVI_2017

# Visualizzazione del NDVI
par(mfrow = c(1, 3))
plot(NDVI_2017, col = viridis_palette, main = "NDVI 2017")
plot(NDVI_2024, col = viridis_palette, main = "NDVI 2024")
plot(NDVI_dif, col = viridis_palette, main = "Differenza tra NDVI 2024 e 2017")
dev.off()

#stessa logica della lettura dei grafici del DVI

#### Calcolo della perdita di copertura vegetale tramite NDVI####

# Parametri di input
soglia <- -0.2
#Soglia per la perdita di NDVI: Un valore di -0,2 identifica una perdita significativa.
ndvi_iniziale <- NDVI_2017
ndvi_finale <- NDVI_2024

# Calcolo della differenza NDVI tra l'anno iniziale e l'anno finale
# Il codice identifica i pixel con valori di NDVI che scendono sotto 
#al valore soglia e li conta.

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

#### 3. PCA (Analisi delle Componenti Principali) ####
# Evidenzia variazioni strutturali nei dati, permettendo di ridurre la complessità del dataset.

# Imposta il seme per la riproducibilità
set.seed(1)

#  Crea un oggetto "box" con le differenze tra gli indici di due anni
NDVI_diff <- NDVI_2024 - NDVI_2017
DVI_diff <- DVI_2024 - DVI_2017

# Unisci le differenze degli indici in un unico stack raster
box <- stack(NDVI_diff, DVI_diff)

# Traccia il grafico delle differenze per capire la variazione tra i due anni
plot(box, col = color, main = "Differenze degli indici NDVI e DVI tra 2017 e 2024")
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
plot(pci[[1]], col = color, main = "Prima Componente Principale (PC1) - 2017 vs 2024")

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

#### 4. T-TEST SUI VALORI DI NDVI ####
#Permette di verificare se c’è una differenza statisticamente significativa nei valori di NDVI tra 2017 e 2024.

# Estrazione dei valori NDVI come vettori
ndvi_2017_values <- values(NDVI_2017)
ndvi_2024_values <- values(NDVI_2024)

#Un cambiamento statisticamente significativo nell'NDVI potrebbe suggerire un 
#impatto ecologico rilevante, come una riduzione significativa della copertura vegetale. 
# Esecuzione del T-test sui valori NDVI tra 2017 e 2024
t_test_result <- t.test(ndvi_2017_values, ndvi_2024_values, paired = TRUE)
print(t_test_result)


##### 5.  LAND COVER ANALYSIS 

# Crea una palette colori per le classi
cl_freq <- colorRampPalette(c("black", "grey", "white"))(3)

# Per ogni anno, estrae i valori delle bande, classifica con k-means, calcola le frequenze e percentuali
# 2017
single_nr_2017 <- getValues(img_2017)
  
single_nr_2024 <- getValues(img_2024)
k_cluster_2024 <- kmeans(single_nr_2024, centers = 3)
img_2024_class <- setValues(img_2024[[1]], k_cluster_2024$cluster)
plot(img_2024_class, col = cl_freq)
freq_2024 <- freq(img_2024_class)
perc_2024 <- round((freq_2024 * 100) / ncell(img_2024_class), digits = 5)

# Consolidare i dati in un DataFrame
copertura_vegetale <- c("Buona", "Ridotta", "Assente")
Land_cover_perc <- data.frame(
  copertura_vegetale, 
  P_2017 = perc_2017[,2],
  P_2024 = perc_2024[,2]
)
print(Land_cover_perc)


### VISUALIZZAZIONE DEI RISULTATI

perc_2017  # Visualizzazione delle percentuali di copertura per il 2017
perc_2024  # Visualizzazione delle percentuali di copertura per il 2024

### CREAZIONE DI UN DATAFRAME CON I RISULTATI DELLA CLASSIFICAZIONE

# Creazione dei vettori
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

cl_barplot <- c("buona" = "blue", "ridotta" = "darkgreen", "assente" = "yellow")

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





# Analisi dell'eterogeneità per visualizzare i pattern spaziali
# A maggior variabilità corrisponde maggior biodiversità.

# Si estrae da ogni immagine la banda del NIR (banda 4)
nir_2017 <- img_2017[[4]]
nir_2024 <- img_2024[[4]]

# Finestra mobile: matrice 3x3 che si sposta sull'immagine, calcola la deviazione standard
# Tre nuovi file raster che esprimono la variabilità per ciascun anno.
sd_2017 <- focal(nir_2017, matrix(1/9, 3, 3), fun = sd)
sd_2024 <- focal(nir_2024, matrix(1/9, 3, 3), fun = sd)

 # Plot Eterogeneità con ggplot
et_2017 <- ggplot() +
  geom_raster(data = as.data.frame(sd_2017, xy = TRUE), aes(x = x, y = y, fill = layer), show.legend = FALSE) +
  scale_fill_viridis(option = "plasma") +
  ggtitle("2017")

et_2024 <- ggplot() +
  geom_raster(data = as.data.frame(sd_2024, xy = TRUE), aes(x = x, y = y, fill = layer), show.legend = TRUE) +
  scale_fill_viridis(option = "plasma", name = "Variabilità") +
  ggtitle("2024")

# Patchwork per visualizzare i due anni
patchwork_heterogeneity <- et_2017 + et_2024
patchwork_heterogeneity + plot_annotation(
  title = 'Land heterogeneity',
  subtitle = 'Bălan, Romania'
)

# Osservazione qualitativa: la variabilità potrebbe ridursi con l'urbanizzazione.

# Salvataggio in PDF delle mappe di eterogeneità
pdf("variability_balan.pdf")
print(et_2017 + plot_annotation(
  title = 'Land heterogeneity (2017)',
  subtitle = 'Bălan, Romania'
))
print(et_2024 + plot_annotation(
  title = 'Land heterogeneity (2024)',
  subtitle = 'Bălan, Romania'
))
dev.off()



# Classificazione delle immagini in 3 classi
landcover_2017 <- unsuperClass(img_2017, nClasses = 3)
landcover_2024 <- unsuperClass(img_2024, nClasses = 3)

# Calcolo delle percentuali per ciascuna classe
freq_2017 <- freq(landcover_2017$map)
freq_2024 <- freq(landcover_2024$map)
perc_2017 <- round((freq_2017 * 100) / ncell(landcover_2017$map), 2)
perc_2024 <- round((freq_2024 * 100) / ncell(landcover_2024$map), 2)

# Creazione di un DataFrame per confrontare le percentuali
classi <- c("Terreno", "Bosco", "Città")
percentuali <- data.frame(
  Classe = classi,
  Perc_2017 = perc_2017[,2],
  Perc_2024 = perc_2024[,2]
)

# Visualizzazione
print(percentuali)
ggplot(percentuali, aes(x = Classe)) +
  geom_bar(aes(y = Perc_2017, fill = "2017"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Perc_2024, fill = "2024"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("2017" = "blue", "2024" = "green")) +
  labs(title = "Confronto Land Cover 2017 vs 2024", y = "Percentuale", x = "Classe")
