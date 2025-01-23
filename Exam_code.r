####### Analisi Incendi Australia (New South Wales) 2019-2020-2024

#{"type":"Polygon","coordinates":[[
#[148.656006,-36.140526],
#[147.884216,-36.140526],
#[147.884216,-35.509558],
#[148.656006,-35.509558],
#[148.656006,-36.140526]]]}
#L'area ha un'estensione di 4892.88 km2

####### 1. Installazione e caricamento dei pacchetti necessari ####### 
install.packages("raster") # per la manipolazione di dati raster
install.packages("ggplot2") # per la creazione di grafici
install.packages("viridis") # per la selezione di palette di colori 

library(raster)
library(ggplot2)
library(viridis)

# Imposto la working directory
setwd("C:/lab/Esame_telerilevamento/prova")

####### 2. Carico le immagini ########

# 2019
# Carico le immagini con rlist selezionando tutti i file che contengono "2018-12-26" nel nome
rlist_2019 <- list.files(pattern = "2019-02-22")
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
rlist_2020 <- list.files(pattern = "2020-02-27")
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

# 2024
# Carico le immagini con rlist selezionando tutti i file che contengono "2024-01-15" nel nome
rlist_2024 <- list.files(pattern = "2024-02-11")
rlist_2024

# Applico la funzione raster() all'intera lista
import_2024 <- lapply(rlist_2024, raster)
import_2024

# Unisco tutte le bande presenti nella lista in un solo oggetto
img_2024 <- stack(import_2024)

# Visualizzo le informazioni
img_2024

# Plotto le bande caricate
plot(img_2024)
dev.off()

# Salvo in PNG le immagini RGB per il 2019, 2020 e 2024
png("img_2019.png", width = 800, height = 600)
par(mfrow = c(1, 2))
plotRGB(img_2019, 3, 2, 1, stretch = "lin")
plotRGB(img_2019, 4, 3, 2, stretch = "lin")
dev.off()

png("img_2020.png", width = 800, height = 600)
par(mfrow = c(1, 2)) 
plotRGB(img_2020, 3, 2, 1, stretch = "lin")
plotRGB(img_2020, 4, 3, 2, stretch = "lin")
dev.off()

png("img_2024.png", width = 800, height = 600)
par(mfrow = c(1, 2)) 
plotRGB(img_2024, 3, 2, 1, stretch = "lin")
plotRGB(img_2024, 4, 3, 2, stretch = "lin")
dev.off()

# Plotto le immagini per tipo di immagini
png("RGB_comparison.png", width = 1200, height = 600)
par(mfrow = c(1, 3))
plotRGB(img_2019, 3, 2, 1, stretch = "lin")
plotRGB(img_2020, 3, 2, 1, stretch = "lin")
plotRGB(img_2024, 3, 2, 1, stretch = "lin")
dev.off()

png("RGB_comparison_2.png", width = 1200, height = 600)
par(mfrow = c(1, 3))
plotRGB(img_2019, 4, 3, 2, stretch = "lin")
plotRGB(img_2020, 4, 3, 2, stretch = "lin")
plotRGB(img_2024, 4, 3, 2, stretch = "lin")
dev.off()

####### 3. Calcolo indici spettrali####### 

# 3.1 Calcolo del DVI (Difference Vegetation Index)
#Utilizzo il DVI per valutare la densità della vegetazione e le differenze tra le immagini.

# Calcola la differenza tra la banda dell’infrarosso vicino (NIR) e la banda rossa per identificare la densità di vegetazione.
DVI_2019 <- img_2019[[4]] - img_2019[[3]]  # DVI = NIR - Rosso
DVI_2020 <- img_2020[[4]] - img_2020[[3]]
DVI_2024 <- img_2024[[4]] - img_2024[[3]]

DVI_diff_2020 <- DVI_2020 - DVI_2019 # Differenza nella densità di vegetazione tra 2020 e 2019
DVI_diff_2024 <- DVI_2024 - DVI_2020 # Differenza nella densità di vegetazione tra 2024 e 2019

# Visualizzazione del DVI
# Imposto una palette di colori
color <- cividis(10)

# Salvo in PNG i grafici
png("DVI.png", width = 1200, height = 600)
par(mfrow = c(1, 3))
plot(DVI_2019, col = color, main = "DVI 2019")
plot(DVI_2020, col = color, main = "DVI 2020")
plot(DVI_2024, col = color, main = "DVI 2024")
dev.off()

png("DVI_diff.png", width = 800, height = 600)
par(mfrow = c(1, 2))
plot(DVI_diff_2020, col = color, main = "Differenza DVI 2020-2019")
plot(DVI_diff_2024, col = color, main = "Differenza DVI 2024-2020")
dev.off()

#Valori positivi: colori più vicini al giallo indicano un aumento della densità di vegetazione.
#Valori negativi: colori più vicini al viola indicano una diminuzione della densità di vegetazione.

# 3.2 Calcolo del NDVI (Normalized Difference Vegetation Index)
#Utilizzo l'NDVI per valutare la salute della vegetazione e le differenze tra le immagini.

# L'NDVI è calcolato come il rapporto tra (NIR - RED) e (NIR + RED), risultando in una scala da -1 a 1.
NDVI_2019 <- DVI_2019 / (img_2019[[4]] + img_2019[[3]])
NDVI_2020 <- DVI_2020 / (img_2020[[4]] + img_2020[[3]])
NDVI_2024 <- DVI_2024 / (img_2024[[4]] + img_2024[[3]])

NDVI_diff_2020 <- NDVI_2020 - NDVI_2019
NDVI_diff_2024 <- NDVI_2024 - NDVI_2020

# Visualizzazione dell'NDVI
png("NDVI.png", width = 1200, height = 600)
par(mfrow = c(1, 3))
plot(NDVI_2019, col = color, main = "NDVI 2019")
plot(NDVI_2020, col = color, main = "NDVI 2020")
plot(NDVI_2024, col = color, main = "NDVI 2024")
dev.off()

png("NDVI_diff.png", width = 800, height = 600)
par(mfrow = c(1, 2))
plot(NDVI_diff_2020, col = color, main = "Differenza NDVI 2020-2019")
plot(NDVI_diff_2024, col = color, main = "Differenza NDVI 2024-2020")
dev.off()
# stessa lettura del DVI

####### 4. Differenza di NDVI e Paired t-Test ####### 

#utilizzo il paired t.test per valutare se le differenze nei valori NDVI sono statisticamente significative.

# 1. Estrazione dei valori NDVI dai raster
ndvi_2019_values <- getValues(NDVI_2019)
ndvi_2020_values <- getValues(NDVI_2020)
ndvi_2024_values <- getValues(NDVI_2024)

# 2. T-test tra 2019 e 2020
t_test_2019_2020 <- t.test(ndvi_2019_values, ndvi_2020_values)
print(t_test_2019_2020)

#Welch Two Sample t-test
#data:  ndvi_2019_values and ndvi_2020_values
#t = 1477.2, df = 12276639, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.1710305 0.1714850
#sample estimates:
#  mean of x mean of y 
#0.6200557 0.4487979   

#La media di NDVI è diminuita significativamente (p-value < 2.2e-16) dal 2019 al 2020, 
#con una riduzione media di circa 0.171 che indica una perdita di copertura vegetale.


# 4. T-test tra 2020 e 2024
t_test_2020_2024 <- t.test(ndvi_2020_values, ndvi_2024_values)
print(t_test_2020_2024)

#Welch Two Sample t-test
#data:  ndvi_2020_values and ndvi_2024_values
#t = -1939.3, df = 12356320, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.2080880 -0.2076678
#sample estimates:
#  mean of x mean of y 
#0.4487979 0.6566758  


#NDVI è aumentato significativamente tra il 2020 e il 2024.
#La differenza media è di circa 0.20, con un'elevata significatività statistica (p-value < 2.2e-16)

####### 5 Classificazione Land Cover ####### 

# Faccio una classificazione delle immagini in base alla copertura vegetale.
# Utilizzo il K-Means clustering per classificare i pixel in tre cluster
# "buona", "ridotta" e "assente" copertura vegetale.

### CLASSIFICAZIONE IMMAGINI 2019
set.seed(1)

# Estrazione dei valori per il 2019
single_nr_2019 <- getValues(img_2019)
# Classificazione in tre cluster
k_cluster_2019 <- kmeans(single_nr_2019, centers = 3)
# Creazione immagine classificata
img_2019_class <- setValues(img_2019[[1]], k_cluster_2019$cluster)

# Calcolo delle frequenze
freq_2019 <- freq(img_2019_class)
tot_pixel_2019 <- ncell(img_2019_class)
perc_2019 <- round((freq_2019[, 2] * 100) / tot_pixel_2019, digit = 5)

# Plot dell'immagine classificata 2019
cl_land_cover <- plasma(3)
png("Land_Cover_2019.png", width = 800, height = 600)
plot(img_2019_class, col = cl_land_cover, main = "Classificazione Land Cover 2019")
dev.off()

### CLASSIFICAZIONE IMMAGINI 2020
set.seed(1)

# Estrazione dei valori per il 2020
single_nr_2020 <- getValues(img_2020)
# Classificazione in tre cluster
k_cluster_2020 <- kmeans(single_nr_2020, centers = 3)
# Creazione immagine classificata
img_2020_class <- setValues(img_2020[[1]], k_cluster_2020$cluster)

# Calcolo delle frequenze
freq_2020 <- freq(img_2020_class)
tot_pixel_2020 <- ncell(img_2020_class)
perc_2020 <- round((freq_2020[, 2] * 100) / tot_pixel_2020, digit = 5)

# Plot dell'immagine classificata 2020
png("Land_Cover_2020.png", width = 800, height = 600)
plot(img_2020_class, col = cl_land_cover, main = "Classificazione Land Cover 2020")
dev.off()

### CLASSIFICAZIONE IMMAGINI 2024
set.seed(1)

# Estrazione dei valori per il 2024
single_nr_2024 <- getValues(img_2024)
# Classificazione in tre cluster
k_cluster_2024 <- kmeans(single_nr_2024, centers = 3)
# Creazione immagine classificata
img_2024_class <- setValues(img_2024[[1]], k_cluster_2024$cluster)

# Calcolo delle frequenze
freq_2024 <- freq(img_2024_class)
tot_pixel_2024 <- ncell(img_2024_class)
perc_2024 <- round((freq_2024[, 2] * 100) / tot_pixel_2024, digit = 5)

# Plot dell'immagine classificata 2024
png("Land_Cover_2024.png", width = 800, height = 600)
plot(img_2024_class, col = cl_land_cover, main = "Classificazione Land Cover 2024")
dev.off()

# Creazione DataFrame
# Creazione dei vettori
copertura_vegetale <- c("buona", "ridotta", "assente")
P_2019 <- perc_2019
P_2020 <- perc_2020
P_2024 <- perc_2024

# Creazione del DataFrame
Land_cover_perc <- data.frame(copertura_vegetale, P_2019, P_2020, P_2024)
print(Land_cover_perc)

#copertura_vegetale   P_2019   P_2020   P_2024
#1              buona 51.83355 38.02002 42.65294
#2            ridotta 17.01086 18.44889 19.82951
#3            assente 31.15559 43.53109 37.51755

# L'incendio del 2019 ha causato una significativa riduzione della copertura 
# vegetale, con un incremento delle aree a bassa o assente vegetazione nel 2020.
# Nel 2024, si osserva una lenta ripresa con un aumento delle zone a ridotta vegetazione
#ma una diminuzione di quelle a vegetazione assente

# Creazione grafici

# Colori per i grafici
cl_barplot <- c("buona" = "red", "ridotta" = "yellow", "assente" = "blue")

# Percentuali Land Cover 2019
png("Percentuali_Land_Cover_2019.png", width = 800, height = 600)
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
png("Percentuali_Land_Cover_2020.png", width = 800, height = 600)
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

# Percentuali Land Cover 2024
png("Percentuali_Land_Cover_2024.png", width = 800, height = 600)
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

####### 6 PCA (Principal Component Analysis) #######

#Utilizzo la PCA per ridurre la dimensionalità dei dati e identificare i pattern
#principali nei valori NDVI.

NDVI_stack_2019_2020 <- stack(NDVI_2019, NDVI_2020)
sample_pixels_2019_2020 <- sampleRandom(NDVI_stack_2019_2020, size = 10000)
PCA_2019_2020 <- prcomp(sample_pixels_2019_2020, scale. = TRUE)
summary(PCA_2019_2020)

# Salvo il plot della prima componente principale (PC1) per 2019-2020 in un file PNG
PCA_projection_2019_2020 <- predict(NDVI_stack_2019_2020, PCA_2019_2020, index = 1)
png("PCA_PC1_2019_2020.png", width = 800, height = 600)
plot(PCA_projection_2019_2020[[1]], main = "PC1 2019-2020", col = viridis(100))
dev.off()

#Importance of components:
#PC1    PC2
#Standard deviation     1.1177 0.8665
#Proportion of Variance 0.6246 0.3754
#Cumulative Proportion  0.6246 1.0000

# PCA su 2020-2024
NDVI_stack_2020_2024 <- stack(NDVI_2020, NDVI_2024)
sample_pixels_2020_2024 <- sampleRandom(NDVI_stack_2020_2024, size = 10000)
PCA_2020_2024 <- prcomp(sample_pixels_2020_2024, scale. = TRUE)
summary(PCA_2020_2024)

# Salvo il plot della prima componente principale (PC1) per 2020-2024 in un file PNG
PCA_projection_2020_2024 <- predict(NDVI_stack_2020_2024, PCA_2020_2024, index = 1)

png("PCA_PC1_2020_2024.png", width = 800, height = 600)
plot(PCA_projection_2020_2024[[1]], main = "PC1 2020-2024", col = viridis(100))
dev.off()

#Importance of components:
#  PC1    PC2
#Standard deviation     1.1825 0.7756
#Proportion of Variance 0.6992 0.3008
#Cumulative Proportion  0.6992 1.0000

#Standard deviation: La deviazione standard per ciascuna componente principale. 
#-Valori più alti indicano una maggiore varianza catturata dalla componente.
#Proportion of Variance: La proporzione di varianza spiegata da ciascuna componente principale.
#Cumulative Proportion: La proporzione cumulativa di varianza spiegata fino 
#-a ciascuna componente principale.
