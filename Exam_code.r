
####### Analisi Incendi Australia 2019-2020-2025

####### 1. Installazione e caricamento dei pacchetti necessari ####### 
install.packages("raster")
install.packages("ggplot2")
install.packages("viridis")

library(raster)
library(ggplot2)
library(viridis)

# Imposto la working directory
setwd("C:/lab/Esame_telerilevamento/prova")

####### 2. Carico le immagini ########

# 2019
# Carico le immagini con rlist selezionando tutti i file che contengono "2019-01-16" nel nome
rlist_2019 <- list.files(pattern = "2019-01-26")
rlist_2019

# Applico la funzione raster() all'intera lista
import_2019 <- lapply(rlist_2019, raster)
import_2019

# Unisco tutte le bande presenti nella lista in un solo oggetto
img_2019 <- stack(import_2019)

# Visualizzo le informazioni
img_2019

# Plotto le bande caricate
plot(img_2019)
dev.off()


# 2020
# Carico le immagini con rlist selezionando tutti i file che contengono "2020-01-21" nel nome
rlist_2020 <- list.files(pattern = "2020-01-21")
rlist_2020

# Applico la funzione raster() all'intera lista
import_2020 <- lapply(rlist_2020, raster)
import_2020

# Unisco tutte le bande presenti nella lista in un solo oggetto
img_2020 <- stack(import_2020)

# Visualizzo le informazioni
img_2020

# Plotto le bande caricate
plot(img_2020)
dev.off()

# 2025
# Carico le immagini con rlist selezionando tutti i file che contengono "2025-01-15" nel nome
rlist_2025 <- list.files(pattern = "2025-01-14")
rlist_2025

# Applico la funzione raster() all'intera lista
import_2025 <- lapply(rlist_2025, raster)
import_2025

# Unisco tutte le bande presenti nella lista in un solo oggetto
img_2025 <- stack(import_2025)

# Visualizzo le informazioni
img_2025

# Plotto le bande caricate
plot(img_2025)
dev.off()

# Salvo in PDF le immagini RGB per il 2019, 2020 e 2025
# pdf("img_2019.pdf")
par(mfrow = c(1, 3))
plotRGB(img_2019, 3, 2, 1, stretch = "lin")
plotRGB(img_2019, 4, 3, 2, stretch = "lin")
dev.off()

# pdf("img_2020.pdf")
par(mfrow = c(1, 3)) 
plotRGB(img_2020, 3, 2, 1, stretch = "lin")
plotRGB(img_2020, 4, 3, 2, stretch = "lin")
dev.off()

# pdf("img_2025.pdf")
par(mfrow = c(1, 3)) 
plotRGB(img_2025, 3, 2, 1, stretch = "lin")
plotRGB(img_2025, 4, 3, 2, stretch = "lin")
dev.off()

# Plotto le immagini per tipo di immagini
par(mfrow = c(1, 3))
plotRGB(img_2019, 3, 2, 1, stretch = "lin")
plotRGB(img_2020, 3, 2, 1, stretch = "lin")
plotRGB(img_2025, 3, 2, 1, stretch = "lin")
dev.off()

par(mfrow = c(1, 3))
plotRGB(img_2019, 4, 3, 2, stretch = "lin")
plotRGB(img_2020, 4, 3, 2, stretch = "lin")
plotRGB(img_2025, 4, 3, 2, stretch = "lin")
dev.off()

####### 3. Calcolo indici spettrali####### 

# 3.1 Calcolo del DVI (Difference Vegetation Index)
#Utilizzo il DVI per valutare la densità della vegetazione e le differenze tra le immagini.

# Calcola la differenza tra la banda dell’infrarosso vicino (NIR) e la banda rossa per identificare la densità di vegetazione.
DVI_2019 <- img_2019[[4]] - img_2019[[3]]  # DVI = NIR - Rosso
DVI_2020 <- img_2020[[4]] - img_2020[[3]]
DVI_2025 <- img_2025[[4]] - img_2025[[3]]
DVI_diff_2020 <- DVI_2020 - DVI_2019 # Differenza nella densità di vegetazione tra 2020 e 2019
DVI_diff_2025 <- DVI_2025 - DVI_2020 # Differenza nella densità di vegetazione tra 2025 e 2019

# Visualizzazione del DVI
# Imposto una palette di colori
color <- cividis(10)

# Salvo in PDF i grafici
# pdf("DVI.pdf")
par(mfrow = c(1, 3))
plot(DVI_2019, col = color, main = "DVI 2019")
plot(DVI_2020, col = color, main = "DVI 2020")
plot(DVI_2025, col = color, main = "DVI 2025")
dev.off()

# pdf("DVI_diff.pdf")
par(mfrow = c(1, 2))
plot(DVI_diff_2020, col = color, main = "Differenza DVI 2020-2019")
plot(DVI_diff_2025, col = color, main = "Differenza DVI 2025-2019")
dev.off()

#Valori positivi: colori più vicini al giallo indicano un aumento della densità di vegetazione.
#Valori negativi: colori più vicini al viola indicano una diminuzione della densità di vegetazione.

# 3.2 Calcolo del NDVI (Normalized Difference Vegetation Index)
#Utilizzo l'NDVI per valutare la salute della vegetazione e le differenze tra le immagini.

# L'NDVI è calcolato come il rapporto tra (NIR - RED) e (NIR + RED), risultando in una scala da -1 a 1.
NDVI_2019 <- DVI_2019 / (img_2019[[4]] + img_2019[[3]])
NDVI_2020 <- DVI_2020 / (img_2020[[4]] + img_2020[[3]])
NDVI_2025 <- DVI_2025 / (img_2025[[4]] + img_2025[[3]])
NDVI_diff_2020 <- NDVI_2020 - NDVI_2019
NDVI_diff_2025 <- NDVI_2025 - NDVI_2020

# Visualizzazione dell'NDVI
par(mfrow = c(1, 3))
plot(NDVI_2019, col = color, main = "NDVI 2019")
plot(NDVI_2020, col = color, main = "NDVI 2020")
plot(NDVI_2025, col = color, main = "NDVI 2025")
dev.off()

# pdf("NDVI_diff.pdf")
par(mfrow = c(1, 2))
plot(NDVI_diff_2020, col = color, main = "Differenza NDVI 2020-2019")
plot(NDVI_diff_2025, col = color, main = "Differenza NDVI 2025-2019")
dev.off()
# stessa lettura del DVI

####### 5. PCA (Principal Component Analysis) #######

#Utilizzo la PCA per ridurre la dimensionalità dei dati e identificare i pattern
#principali nei valori NDVI.

# Combina NDVI_2019, NDVI_2020 e NDVI_2025
NDVI_stack <- stack(NDVI_2019, NDVI_2020, NDVI_2025)

# Campionamento casuale
set.seed(1)
sample_pixels <- sampleRandom(NDVI_stack, size = 10000)

# PCA sui pixel campionati
PCA <- prcomp(sample_pixels, scale. = TRUE)
summary(PCA)

#Importance of components:
#PC1    PC2     PC3
#Standard deviation     1.4712 0.7757 0.48358
#Proportion of Variance 0.7215 0.2006 0.07795
#Cumulative Proportion  0.7215 0.9221 1.00000

#Standard deviation: La deviazione standard per ciascuna componente principale. 
#-Valori più alti indicano una maggiore varianza catturata dalla componente.
#Proportion of Variance: La proporzione di varianza spiegata da ciascuna componente principale.
#Cumulative Proportion: La proporzione cumulativa di varianza spiegata fino 
#-a ciascuna componente principale.

#PC1 (Prima Componente Principale): Spiega il 72.15% della varianza totale nei dati.
#È la componente più importante e probabilmente rappresenta la variazione principale 
#tra i pixel (ad esempio, la densità media della vegetazione).

# Proiezione PCA
PCA_projection <- predict(NDVI_stack, PCA, index = 1:2)

# Plot della prima componente principale (PC1)
plot(PCA_projection[[1]], main = "Prima Componente Principale (PC1)", col = viridis(100))

####### 6. Classificazione Land Cover ####### 

#Faccio una classificazione delle immagini in base alla copertura vegetale.
#Utilizzo il K-Means clustering per classificare i pixel in due cluster
#(ad esempio, "ridotta/assente" e "buona" copertura vegetale).

### CLASSIFICAZIONE IMMAGINI 2019
set.seed(1)

# Estrazione dei valori per il 2019
single_nr_2019 <- getValues(img_2019)
# Classificazione in due cluster
k_cluster_2019 <- kmeans(single_nr_2019, centers = 2)
# Creazione immagine classificata
img_2019_class <- setValues(img_2019[[1]], k_cluster_2019$cluster)

# Calcolo delle frequenze
freq_2019 <- freq(img_2019_class)
tot_pixel_2019 <- ncell(img_2019_class)
perc_2019 <- round((freq_2019[, 2] * 100) / tot_pixel_2019, digit = 5)

# Plot dell'immagine classificata 2019
cl_freq <- colorRampPalette(c("blue", "yellow"))(2)
plot(img_2019_class, col = cl_freq, main = "Classificazione Land Cover 2019")

### CLASSIFICAZIONE IMMAGINI 2020
set.seed(1)

# Estrazione dei valori per il 2020
single_nr_2020 <- getValues(img_2020)
# Classificazione in due cluster
k_cluster_2020 <- kmeans(single_nr_2020, centers = 2)
# Creazione immagine classificata
img_2020_class <- setValues(img_2020[[1]], k_cluster_2020$cluster)

# Calcolo delle frequenze
freq_2020 <- freq(img_2020_class)
tot_pixel_2020 <- ncell(img_2020_class)
perc_2020 <- round((freq_2020[, 2] * 100) / tot_pixel_2020, digit = 5)

# Plot dell'immagine classificata 2020
plot(img_2020_class, col = cl_freq, main = "Classificazione Land Cover 2020")

### CLASSIFICAZIONE IMMAGINI 2025
set.seed(1)

# Estrazione dei valori per il 2025
single_nr_2025 <- getValues(img_2025)
# Classificazione in due cluster
k_cluster_2025 <- kmeans(single_nr_2025, centers = 2)
# Creazione immagine classificata
img_2025_class <- setValues(img_2025[[1]], k_cluster_2025$cluster)

# Calcolo delle frequenze
freq_2025 <- freq(img_2025_class)
tot_pixel_2025 <- ncell(img_2025_class)
perc_2025 <- round((freq_2025[, 2] * 100) / tot_pixel_2025, digit = 5)

# Plot dell'immagine classificata 2025
plot(img_2025_class, col = cl_freq, main = "Classificazione Land Cover 2025")

# Creazione DataFrame
# Creazione dei vettori
copertura_vegetale <- c("ridotta/assente", "buona")
P_2019 <- perc_2019
P_2020 <- perc_2020
P_2025 <- perc_2025

# Creazione del DataFrame
Land_cover_perc <- data.frame(copertura_vegetale, P_2019, P_2020, P_2025)
print(Land_cover_perc)

#  copertura_vegetale   P_2019   P_2020   P_2025
#1    ridotta/assente 62.29453 69.38395 67.59975
#2              buona 37.70547 30.61605 32.40025

#L'incendio del 2019 ha causato una significativa riduzione della copertura 
#vegetale, con un incremento delle aree a bassa o assente vegetazione nel 2020.
#Nel 2025, si nota una lieve rigenerazione, ma la copertura vegetale non è 
#ancora tornata ai livelli del 2019

#Creazione grafici

# Colori per i grafici
cl_barplot <- c("buona" = "blue", "ridotta/assente" = "yellow")

# Percentuali Land Cover 2019
pdf("Percentuali_Land_Cover_2019.pdf")
plot_2019 <- ggplot(Land_cover_perc, aes(x = copertura_vegetale, y = P_2019, fill = copertura_vegetale)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cl_barplot) +
  labs(x = "Land Cover", y = "%", title = "Land Cover 2019") +
  theme(legend.position = "none") +
  ylim(0, 100) +
  geom_text(aes(label = sprintf("%.2f%%", P_2019), y = P_2019), 
            position = position_stack(vjust = 0.5), size = 4)
print(plot_2019)
dev.off()

# Percentuali Land Cover 2020
pdf("Percentuali_Land_Cover_2020.pdf")
plot_2020 <- ggplot(Land_cover_perc, aes(x = copertura_vegetale, y = P_2020, fill = copertura_vegetale)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cl_barplot) +
  labs(x = "Land Cover", y = "%", title = "Land Cover 2020") +
  theme(legend.position = "none") +
  ylim(0, 100) +
  geom_text(aes(label = sprintf("%.2f%%", P_2020), y = P_2020), 
            position = position_stack(vjust = 0.5), size = 4)
print(plot_2020)
dev.off()

# Percentuali Land Cover 2025
pdf("Percentuali_Land_Cover_2025.pdf")
plot_2025 <- ggplot(Land_cover_perc, aes(x = copertura_vegetale, y = P_2025, fill = copertura_vegetale)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cl_barplot) +
  labs(x = "Land Cover", y = "%", title = "Land Cover 2025") +
  theme(legend.position = "none") +
  ylim(0, 100) +
  geom_text(aes(label = sprintf("%.2f%%", P_2025), y = P_2025), 
            position = position_stack(vjust = 0.5), size = 4)
print(plot_2025)
dev.off()

# Combinazione dei grafici
final_plot <- plot_2019 + plot_2020 + plot_2025

# Salvataggio del grafico combinato
pdf("Final_Land_Cover_2019_2020_2025.pdf")
print(final_plot)
dev.off()

#######  7. Esportazione dei risultati####### 

# Salva i risultati in PDF
pdf("Australia_Forest_Fire_Analysis.pdf")
par(mfrow = c(3, 3))
plot(NDVI_2019, main = "NDVI 2019", col = viridis(100))
plot(NDVI_2020, main = "NDVI 2020", col = viridis(100))
plot(NDVI_2025, main = "NDVI 2025", col = viridis(100))
plot(NDVI_diff_2020, main = "Differenza NDVI 2020-2019", col = viridis(100))
plot(NDVI_diff_2025, main = "Differenza NDVI 2025-2019", col = viridis(100))
plot(PCA_projection[[1]], main = "PC1", col = viridis(100))
plot(img_2019_class, main = "Land Cover 2019", col = c("blue", "yellow"))
plot(img_2020_class, main = "Land Cover 2020", col = c("blue", "yellow"))
plot(img_2025_class, main = "Land Cover 2025", col = c("blue", "yellow"))
dev.off()
