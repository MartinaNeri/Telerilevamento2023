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
setwd("C:/lab/Esame_telerilevamento/prova/Australia")

#South Victoria
#South Victoria è una regione dell'Australia meridionale che comprende la città di Melbourne e le aree circostanti.

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

rlist_2017 <- list.files(pattern = "2018")
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

rlist_2024 <- list.files(pattern = "2021")
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
par(mfrow = c(1, 2)) 
plotRGB(img_2024, 3, 2, 1, stretch = "lin")
plotRGB(img_2024, 4, 3, 2, stretch = "lin")
dev.off()

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
# Imposto una palette di colori
color <- cividis(10)

#faccio il grafico e lo salvo in un pdf

pdf("DVI.pdf")
par(mfrow = c(1, 3))
plot(DVI_2017, col = color, main = "DVI 2017")
plot(DVI_2024, col = color, main = "DVI 2024")
plot(DVI_fin, col = color, main = "differenza DVI")
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
plot(NDVI_2017, col = color, main = "NDVI 2017")
plot(NDVI_2024, col = color, main = "NDVI 2024")
plot(NDVI_dif, col = color, main = "Differenza tra NDVI 2024 e 2017")
dev.off()

#stessa logica della lettura dei grafici del DVI

#### Calcolo della perdita di copertura vegetale tramite NDVI####

# Parametri di input
soglia <- -0.2
#Soglia per la perdita di NDVI: Un valore di -0,2 identifica una perdita significativa.

#La soglia di -0,2 è una scelta tipica per identificare la perdita significativa 
#di copertura vegetale in base all'NDVI. Tuttavia, il valore può essere adattato al 
#contesto geografico, temporale, o all'obiettivo dell'analisi. 
#Se vuoi convalidare ulteriormente questa soglia, potresti confrontare i 
#risultati con dati sul campo o altre metriche ambientali.

#La scelta di -0,2 come soglia può variare a seconda del contesto ecologico e della scala temporale. 
#Ecco alcune considerazioni:
#Contesto geografico: In aree dove la vegetazione è densa, una soglia più severa
#come -0,3 potrebbe essere più appropriata. In aree semiaride, una soglia meno 
#severa come -0,1 potrebbe bastare.
#Durata del periodo di analisi: Per un'analisi su un lungo periodo (come 7 anni 
#nel tuo caso), una differenza di -0,2 è ragionevole per distinguere cambiamenti 
#significativi.

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



#PAIRED T.TEST SUI DATI DI NDVI TRA 2017 2014
#Un t-test appaiato confronta i valori medi di due campioni correlati in 
#questo caso i valori di NDVI per il 2017 e il 2024.
# 1. Estrazione dei valori NDVI dai raster
ndvi_values_2017 <- getValues(NDVI_2017)
ndvi_values_2024 <- getValues(NDVI_2024)

# 3. Esecuzione del t-test tra i due gruppi di NDVI
t_test_result <- t.test(ndvi_values_2017, ndvi_values_2024, paired = TRUE)

# 4. Stampa dei risultati del t-test
print(t_test_result)

#	Paired t-test

#data:  ndvi_values_2017 and ndvi_values_2024
#t = -5.2201, df = 3857066, p-value = 1.789e-07
#alternative hypothesis: true mean difference is not equal to 0
#95 percent confidence interval:
#  -2.070769e-04 -9.402365e-05
#sample estimates:
#  mean difference 
#-0.0001505503 

#Il risultato del t-test appaiato mostra che il valore t è -5.2201, con un
#p-value di 1.789e-07. Questo indica che c'è una differenza significativa
#tra i valori medi di NDVI per il 2017 e il 2024. Il valore medio della
#differenza è -0.0001505503 e indica una diminuzione dell'NDVI, suggerendo 
#una possibile riduzione della densità di vegetazione o della copertura 
#vegetaletra i due anni.






################################ DA QUA A LINEA PARTE INCERTA 270
#### 3. PCA (Analisi delle Componenti Principali) ####
# Evidenzia variazioni strutturali nei dati, permettendo di ridurre la complessità del dataset.



#################### 3. PCA (PRINCIPAL COMPONENT ANALYSIS) #####################

# Imposta il seme per la riproducibilità
set.seed(1)

# Visualizza NDVI_dif come input per la PCA
plot(NDVI_dif, main = "Differenza NDVI (2017-2024)", xaxt = "n", yaxt = "n")

# Campionamento casuale di 10,000 pixel dall'immagine NDVI_dif
sr <- sampleRandom(NDVI_dif, 10000)

# Effettuo la PCA (Principal Component Analysis)
PCA <- prcomp(sr, scale. = TRUE)  # Normalizzazione inclusa

# Visualizzazione delle informazioni relative alla PCA
summary(PCA)

# Plot della varianza spiegata dalle componenti principali

#NN FUNZIONA:( 
plot(PCA, main = "Varianza spiegata da ciascuna componente")

# Proiezione di NDVI_dif nello spazio PCA
PCI <- predict(NDVI_dif, PCA, index = 1:2)

# Plot della prima componente principale (PC1)
plot(PCI[[1]], main = "PC1 - Prima Componente Principale", col = magma(100))

# Conversione di PC1 in un dataframe per ggplot2
PC_fin <- as.data.frame(PCI[[1]], xy = TRUE)

#str(PC_fin)  #perchè non riuscivo a plottare e volevo capire quale fosse il problema

# Creazione di un plot avanzato con ggplot2
plot_PCA <- ggplot() + 
  geom_raster(data = PC_fin, mapping = aes(x = x, y = y, fill = layer.1)) + 
  scale_fill_viridis(option = "magma") +
  labs(title = "PC1 - NDVI Differenza", fill = "PC1") +
  theme_minimal()

# Visualizzazione del plot avanzato
print(plot_PCA)

# Esportazione del plot in formato PDF
pdf("PC1_NDVI.pdf")
print(plot_PCA)
dev.off()



################################ 4. LAND COVER #################################

### CLASSIFICAZIONE IMMAGINI 2017

# Estrazione dei valori dalle immagini del 2017
single_nr_2017 <- getValues(img_2017)

# Classificazione in due cluster
k_cluster_2017 <- kmeans(single_nr_2017, centers = 2)
k_cluster_2017

# Set dei valori classificati
img_2017_class <- setValues(img_2017[[1]], k_cluster_2017$cluster)

# Creazione di una palette di colori
cl_freq <- colorRampPalette(c("blue", "yellow"))(2)

# Plot dell'immagine classificata 2017
plot(img_2017_class, col = cl_freq, main = "Classificazione Land Cover 2017")

# Calcolo delle frequenze
freq_2017 <- freq(img_2017_class)
freq_2017

# Calcolo dei pixel totali
tot_pixel_2017 <- ncell(img_2017_class)

# Calcolo delle percentuali
perc_2017 <- round((freq_2017[, 2] * 100) / tot_pixel_2017, digit = 5)
perc_2017



#par(mfrow = c(1, 2))
#plotRGB(img_2017, 3, 2, 1, stretch = "lin")
#plot(img_2017_class, col = cl_freq, main = "Classificazione Land Cover 2017")



### CLASSIFICAZIONE IMMAGINI 2024

# Estrazione dei valori dalle immagini del 2024
single_nr_2024 <- getValues(img_2024)
single_nr_2024

# Classificazione in due cluster
k_cluster_2024 <- kmeans(single_nr_2024, centers = 2)
k_cluster_2024

# Set dei valori classificati
img_2024_class <- setValues(img_2024[[1]], k_cluster_2024$cluster)

# Plot dell'immagine classificata 2024
plot(img_2024_class, col = cl_freq, main = "Classificazione Land Cover 2024")

# Calcolo delle frequenze
freq_2024 <- freq(img_2024_class)
freq_2024

# Calcolo dei pixel totali
tot_pixel_2024 <- ncell(img_2024_class)

# Calcolo delle percentuali
perc_2024 <- round((freq_2024[, 2] * 100) / tot_pixel_2024, digit = 5)
perc_2024



### CREAZIONE DI UN DATAFRAME CON I RISULTATI DELLA CLASSIFICAZIONE

# Creazione dei vettori
copertura_vegetale <- c("ridotta/assente", "buona")
P_2017 <- perc_2017
P_2024 <- perc_2024

# Creazione del dataframe
Land_cover_perc <- data.frame(copertura_vegetale, P_2017, P_2024)

# Visualizzazione del dataframe
print(Land_cover_perc)



### PLOTTING E ESPORTAZIONE IN PDF

cl_barplot <- c("buona" = "blue", "ridotta/assente" = "yellow")

# 2017
pdf("Percentuali Land Cover 2017.pdf")
plot_2017 <- ggplot(Land_cover_perc, aes(x = copertura_vegetale, y = P_2017, fill = copertura_vegetale)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cl_barplot) +
  labs(x = "Land Cover", y = "%", title = "Land Cover 2017") +
  theme(legend.position = "none") +
  ylim(0, 100) +
  geom_text(aes(label = sprintf("%.2f%%", P_2017), y = P_2017), 
            position = position_stack(vjust = 0.5), size = 4)
print(plot_2017)
dev.off()

# 2024
pdf("Percentuali Land Cover 2024.pdf")
plot_2024 <- ggplot(Land_cover_perc, aes(x = copertura_vegetale, y = P_2024, fill = copertura_vegetale)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cl_barplot) +
  labs(x = "Land Cover", y = "%", title = "Land Cover 2024") +
  theme(legend.position = "none") +
  ylim(0, 100) +
  geom_text(aes(label = sprintf("%.2f%%", P_2024), y = P_2024), 
            position = position_stack(vjust = 0.5), size = 4)
print(plot_2024)
dev.off()



### COMBINAZIONE DEI PLOT E ESPORTAZIONE

# Unione dei due plot in un unico grafico
final_plot <- plot_2017 + plot_2024
final_plot

# Esportazione di final_plot in .pdf
pdf("Final_Land_Cover_2017_2024.pdf")
print(final_plot)
dev.off()



