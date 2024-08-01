# Installazione dei packages necessari
install.packages("raster")  # To work with raster images - functions like 'raster', 'stack', 'crop', 'plotRGB', 'plot', 'values', 'focal', 'aggregate', 'rasterPCA'
install.packages("ggplot2") # To make plots - functions like 'ggplot2', 'geom_bar', 'labs', 'scale_fill_manual', 'geom_text', 'theme_minimal' 
install.packages("patchwork") # To easily pair plots
install.packages("viridis") # To plot images and graphs with color blind friendly palettes

# Caricamento dei packages installati
library(raster)
library(ggplot2)
library(patchwork)
library(viridis)

# Impostare la working directory
setwd("C:/lab/congo_deforestation")


############# 1. IMPORTAZIONE E VISUALIZZAZIONE IMMAGINI Sentinel2 #############

### IMMAGINI 2018
rlist_2018 <- list.files(pattern = "2018-12-22")
rlist_2018

# Applico la funzione raster() all'intera lista
import_2018 <- lapply(rlist_2018, raster)
import_2018

# Unione di tutte le bande presenti nella lista in un solo oggetto
img_2018 <- stack(import_2018)

# Visualizzo le informazioni
img_2018

plot(img_2018)

# Plot di img_2018: colori reali e NIR + esportazione in .pdf
pdf("img_2018.pdf")
par(mfrow = c(1,2))
plotRGB(img_2018,1,2,3, stretch = "lin")
plotRGB(img_2018,5,1,2, stretch = "lin")
dev.off()

# Banda 3  = blu
# Banda 2  = verde
# Banda 1  = rosso
# Banda 5 = NIR

#2019

rlist_2019 <- list.files(pattern = "2019-12-17")
rlist_2019

# Applico la funzione raster() all'intera lista
import_2019 <- lapply(rlist_2019, raster)
import_2019

# Unione di tutte le bande presenti nella lista in un solo oggetto
img_2019 <- stack(import_2019)

# Visualizzo le informazioni
img_2019

plot(img_2019)

# Plot di img_2019: colori reali e NIR + esportazione in .pdf
pdf("img_2019.pdf")
par(mfrow = c(1,2))
plotRGB(img_2019,1,2,3, stretch = "lin")
plotRGB(img_2019,5,1,2, stretch = "lin")
dev.off()

#2020

rlist_2020 <- list.files(pattern = "2020-12-01")
rlist_2020

# Applico la funzione raster() all'intera lista
import_2020 <- lapply(rlist_2020, raster)
import_2020

# Unione di tutte le bande presenti nella lista in un solo oggetto
img_2020 <- stack(import_2020)

# Visualizzo le informazioni
img_2020

plot(img_2020)

# Plot di img_2020: colori reali e NIR + esportazione in .pdf
pdf("img_2020.pdf")
par(mfrow = c(1,2))
plotRGB(img_2020,1,2,3, stretch = "lin")
plotRGB(img_2020,5,1,2, stretch = "lin")
dev.off()

#2021

rlist_2021 <- list.files(pattern = "2021-11-06")
rlist_2021

# Applico la funzione raster() all'intera lista
import_2021 <- lapply(rlist_2021, raster)
import_2021

# Unione di tutte le bande presenti nella lista in un solo oggetto
img_2021 <- stack(import_2021)

# Visualizzo le informazioni
img_2021

plot(img_2021)

# Plot di img_2021: colori reali e NIR + esportazione in .pdf
pdf("img_2021.pdf")
par(mfrow = c(1,2))
plotRGB(img_2021,1,2,3, stretch = "lin")
plotRGB(img_2021,5,1,2, stretch = "lin")
dev.off()

#2022

rlist_2022 <- list.files(pattern = "2022-12-26")
rlist_2022

# Applico la funzione raster() all'intera lista
import_2022 <- lapply(rlist_2022, raster)
import_2022

# Unione di tutte le bande presenti nella lista in un solo oggetto
img_2022 <- stack(import_2022)

# Visualizzo le informazioni
img_2022

plot(img_2022)

# Plot di img_2022: colori reali e NIR + esportazione in .pdf
pdf("img_2022.pdf")
par(mfrow = c(1,2))
plotRGB(img_2022,1,2,3, stretch = "lin")
plotRGB(img_2022,5,1,2, stretch = "lin")
dev.off()

#2023

rlist_2023 <- list.files(pattern = "2023-12-16")
rlist_2023

# Applico la funzione raster() all'intera lista
import_2023 <- lapply(rlist_2023, raster)
import_2023

# Unione di tutte le bande presenti nella lista in un solo oggetto
img_2023 <- stack(import_2023)

# Visualizzo le informazioni
img_2023

plot(img_2023)

# Plot di img_2023: colori reali e NIR + esportazione in .pdf
pdf("img_2023.pdf")
par(mfrow = c(1,2))
plotRGB(img_2023,1,2,3, stretch = "lin")
plotRGB(img_2023,5,1,2, stretch = "lin")
dev.off()

################### 2. CALCOLO E PLOT DEGLI INDICI SPETTRALI ###################

### 2.1 DVI (DIFFERENCE VEGETATION INDEX)

# Calcolo del DVI (Difference Vegetation Index)
# DVI = NIR - rosso
DVI_2018 <- img_2018[[5]] - img_2018[[1]]
DVI_2019 <- img_2019[[5]] - img_2019[[1]]
DVI_2020 <- img_2020[[5]] - img_2020[[1]]
DVI_2021 <- img_2021[[5]] - img_2021[[1]]
DVI_2022 <- img_2022[[5]] - img_2022[[1]]
DVI_2023 <- img_2023[[5]] - img_2023[[1]]


# My Palettes
# My favorite Blind Friendly palettes 
viridis <- colorRampPalette(viridis(7))(255)
magma <- colorRampPalette(magma(7))(255)
inferno <- colorRampPalette(inferno(7))(255)

# Creazione di una palette di colori
####non mi piace questa palette non è color blind friendly
cl <- colorRampPalette(c("blue","darkgrey","yellow"))(100)

# Different scales so 
# Unify scale 
min_value_dvi <- min(min(values(DVI_2018)), min(values(DVI_2019)), min(values(DVI_2020)), min(values(DVI_2021)), min(values(DVI_2022)), min(values(DVI_2023)))
max_value_dvi <- max(max(values(DVI_2018)), max(values(DVI_2019)), max(values(DVI_2020)), max(values(DVI_2021)), max(values(DVI_2022)), max(values(DVI_2023)))

# Multiframe creation to Plot DVI
par(mfrow = c(2,3))
plot(DVI_2018, col = viridis, main = "DVI_2018", zlim = c(min_value_dvi, max_value_dvi))
plot(DVI_2019, col = viridis, main = "DVI_2019", zlim = c(min_value_dvi, max_value_dvi))
plot(DVI_2020, col = viridis, main = "DVI_2020", zlim = c(min_value_dvi, max_value_dvi))
plot(DVI_2021, col = viridis, main = "DVI_2021", zlim = c(min_value_dvi, max_value_dvi))
plot(DVI_2022, col = viridis, main = "DVI_2022", zlim = c(min_value_dvi, max_value_dvi))
plot(DVI_2023, col = viridis, main = "DVI_2023", zlim = c(min_value_dvi, max_value_dvi))
dev.off()

### 2.2 NDVI (NORMALIZED DIFFERENCE VEGETATION INDEX)

# Calcolo del NDVI (Normalized Difference Vegetation Index)
# NDVI = (NIR - rosso) / (NIR + rosso) = DVI / (NIR + rosso)
NDVI_2018 <- DVI_2018 / (img_2018[[5]] + img_2018[[1]])
NDVI_2019 <- DVI_2019 / (img_2019[[5]] + img_2019[[1]])
NDVI_2020 <- DVI_2020 / (img_2020[[5]] + img_2020[[1]])
NDVI_2021 <- DVI_2021 / (img_2021[[5]] + img_2021[[1]])
NDVI_2022 <- DVI_2022 / (img_2022[[5]] + img_2022[[1]])
NDVI_2023 <- DVI_2023 / (img_2023[[5]] + img_2023[[1]])

min_value_ndvi <- min(min(values(NDVI_2018)), min(values(NDVI_2019)), min(values(NDVI_2020)), min(values(NDVI_2021)), min(values(NDVI_2022)), min(values(NDVI_2023)))
max_value_ndvi <- max(max(values(NDVI_2018)), max(values(NDVI_2019)), max(values(NDVI_2020)), max(values(NDVI_2021)), max(values(NDVI_2022)), max(values(NDVI_2023)))

# Plot NDVI
par(mfrow = c(2,3))
plot(NDVI_2018, col = inferno, main = "NDVI_2018", zlim = c(min_value_ndvi, max_value_ndvi))
plot(NDVI_2019, col = inferno, main = "NDVI_2019", zlim = c(min_value_ndvi, max_value_ndvi))
plot(NDVI_2020, col = inferno, main = "NDVI_2020", zlim = c(min_value_ndvi, max_value_ndvi))
plot(NDVI_2021, col = inferno, main = "NDVI_2021", zlim = c(min_value_ndvi, max_value_ndvi))
plot(NDVI_2022, col = inferno, main = "NDVI_2022", zlim = c(min_value_ndvi, max_value_ndvi))
plot(NDVI_2023, col = inferno, main = "NDVI_2023", zlim = c(min_value_ndvi, max_value_ndvi))
dev.off()

# Calcolo la differenza fra NDVI_2018 e NDVI_2023
NDVI_def <- NDVI_2018 - NDVI_2023

# Plot di NDVI_def + esportazione in .pdf
pdf("NDVI_def.pdf")
plot(NDVI_def, col = cl, main = "NDVI_def")
dev.off()

# GRIGIO: aree con differenza di NDVI fra 2018 e 2023 nulla
# GIALLO: perdita in copertura vegetale dal 2018 al 2023
# BLU:    guadagno in copertura vegetale dal 2018 al 2023


# Creare una lista di NDVI
ndvi_list <- list(
  "2018" = NDVI_2018,
  "2019" = NDVI_2019,
  "2020" = NDVI_2020,
  "2021" = NDVI_2021,
  "2022" = NDVI_2022,
  "2023" = NDVI_2023
)

# Calcolare le differenze annuali dell'NDVI
diff_list <- lapply(1:(length(ndvi_list) - 1), function(i) {
  ndvi_list[[i + 1]] - ndvi_list[[i]]
})

# Funzione per convertire un raster in un dataframe per ggplot
raster_to_df <- function(raster_layer) {
  raster_df <- as.data.frame(raster_layer, xy = TRUE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df
}

# Convertire i raster di differenza in dataframes
diff_dfs <- lapply(diff_list, raster_to_df)

# Creare una lista di ggplots per ogni differenza annuale
diff_plots <- lapply(1:length(diff_dfs), function(i) {
  ggplot(diff_dfs[[i]], aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_viridis(option = "viridis") +
    labs(title = paste("NDVI Change", years[i], "-", years[i + 1]), fill = "Change") +
    theme_minimal()
})

# Unire i plot in un unico layout con patchwork
diff_final_plot <- wrap_plots(diff_plots, ncol = 2)

# Visualizzare i plot finali
print(diff_final_plot)

### 2.3 EVI (ENHANCED V)

# Indice introdotto per ridurre l'effetto del suolo nelle misurazioni di vegeta-
# zione. Rispetto all'NDVI, il MSAVI tende a essere meno sensibile all'effetto 
# del suolo e può fornire stime più accurate della copertura vegetale.

# Calcolo del MSAVI (Modified Soil-Adjusted Vegetation Index)
# MSAVI = {2*NIR + 1 - sqrt[(2*NIR + 1)^2 - 8*(NIR - rosso)]} / 2
MSAVI_2018 <- ((2*img_2018[[5]] + 1) - sqrt((2*img_2018[[5]] + 1)^2 - 8 *(img_2018[[5]] - img_2018[[1]]))) / 2
MSAVI_2019 <- ((2*img_2019[[5]] + 1) - sqrt((2*img_2019[[5]] + 1)^2 - 8 *(img_2019[[5]] - img_2019[[1]]))) / 2
MSAVI_2020 <- ((2*img_2020[[5]] + 1) - sqrt((2*img_2020[[5]] + 1)^2 - 8 *(img_2020[[5]] - img_2020[[1]]))) / 2
MSAVI_2021 <- ((2*img_2021[[5]] + 1) - sqrt((2*img_2021[[5]] + 1)^2 - 8 *(img_2021[[5]] - img_2021[[1]]))) / 2
MSAVI_2022 <- ((2*img_2022[[5]] + 1) - sqrt((2*img_2022[[5]] + 1)^2 - 8 *(img_2022[[5]] - img_2022[[1]]))) / 2
MSAVI_2023 <- ((2*img_2023[[5]] + 1) - sqrt((2*img_2023[[5]] + 1)^2 - 8 *(img_2023[[5]] - img_2023[[1]]))) / 2

# Plot MSAVI
par(mfrow = c(2,3))
plot(MSAVI_2018, col = cl, main = "MSAVI_2018")
plot(MSAVI_2019, col = cl, main = "MSAVI_2019")
plot(MSAVI_2020, col = cl, main = "MSAVI_2020")
plot(MSAVI_2021, col = cl, main = "MSAVI_2021")
plot(MSAVI_2022, col = cl, main = "MSAVI_2022")
plot(MSAVI_2023, col = cl, main = "MSAVI_2023")
dev.off()

# Calcolo la differenza fra MSAVI_2018 e MSAVI_2023
MSAVI_def <- MSAVI_2018 - MSAVI_2023

# Plot di NDVI_def + esportazione in .pdf
pdf("MSAVI_def.pdf")
plot(MSAVI_def, col = cl, main = "MSAVI_def")
dev.off()

# GRIGIO: aree con differenza di NDVI fra 2018 e 2023 nulla
# GIALLO: perdita in copertura vegetale dal 2018 al 2023
# BLU:    guadagno in copertura vegetale dal 2018 al 2023



### VERIFICO SE NDVI_def e MSAVI_def DIFFERISCONO SIGNIFICATIVAMENTE

# Plot di NDVI_def e MSAVI_def + esportazione in .pdf
pdf("NDVI_def + MSAVI_def.pdf")
par(mfrow = c(1,2))
plot(NDVI_def, col = cl, main = "NDVI_def")
plot(MSAVI_def, col = cl, main = "MSAVI_def")
dev.off()

# Eseguo un T-test su NDVI_def e MSAVI_def
t_result <- t.test(NDVI_def[], MSAVI_def[], paired = TRUE)
t_result









# Funzione per eseguire la PCA
calc_pca <- function(raster_layer) {
  raster_values <- getValues(raster_layer)     # Estrae i valori del raster in una matrice
  raster_pca <- prcomp(na.omit(raster_values), center = TRUE, scale. = TRUE) # Esegue la PCA sui valori del raster, centrando e scalando i dati
  pca_raster <- raster_layer   # Crea un nuovo raster per il primo componente principale
  values(pca_raster) <- raster_pca$x[, 1] # Usa solo il primo componente principale
  names(pca_raster) <- "PCA1"
  return(pca_raster)   # Restituisce il raster del primo componente principale
}

pca_list <- lapply(ndvi_list, calc_pca)

#Calcolo delle differenze annuali dell'NDVI

diff_list <- list()
years <- names(ndvi_list)
for (i in 1:(length(ndvi_list) - 1)) {
  diff <- ndvi_list[[i + 1]] - ndvi_list[[i]]
  names(diff) <- paste0("diff_", years[i + 1], "_", years[i])
  diff_list[[names(diff)]] <- diff
}



??????????????????
ggplot(data = as.data.frame(NDVI_2018)) +
  geom_raster() +
  scale_fill_gradient(low = "green", high = "red") +
  labs(title = "NDVI2018") +
  theme_minimal()

ggplot(data = as.data.frame(DVI_2018), aes(x = x, y = y)) +
  geom_raster() +
  scale_fill_gradient(low = "green", high = "red") +
  labs(title = "Differenza NDVI nel tempo") +
  theme_minimal()

???????????????????????
  diff_stack <- stack()
for (i in 1:(nlayers(ndvi_stack) - 1)) {
  diff <- ndvi_stack[[i + 1]] - ndvi_stack[[i]]
  names(diff) <- paste0("diff_", names(ndvi_stack)[i + 1], "_", names(ndvi_stack)[i])
  diff_stack <- stack(diff_stack, diff)


# Calcolo la differenza fra NDVI_2018 e NDVI_2023
NDVI_def <- NDVI_2017 - NDVI_2023

# Plot di NDVI_def + esportazione in .pdf
pdf("NDVI_def.pdf")
plot(NDVI_def, col = cl, main = "NDVI_def", zlim = c(min_value_ndvi, max_value_ndvi))
dev.off()

# GRIGIO: aree con differenza di NDVI fra 2018 e 2023 nulla
# GIALLO: perdita in copertura vegetale dal 2018 al 2023
# BLU:    guadagno in copertura vegetale dal 2018 al 2023

??????????????????????????????????????????????????????????????

### MSAVI (MODIFIED SOIL-ADJUSTED VEGETATION INDEX)

# Indice introdotto per ridurre l'effetto del suolo nelle misurazioni di vegeta-
# zione. Rispetto all'NDVI, il MSAVI tende a essere meno sensibile all'effetto 
# del suolo e può fornire stime più accurate della copertura vegetale.

# Calcolo del MSAVI (Modified Soil-Adjusted Vegetation Index)
# MSAVI = {2*NIR + 1 - sqrt[(2*NIR + 1)^2 - 8*(NIR - rosso)]} / 2
#MSAVI_2017 <- (2*congo2017[[5]] + 1 - sqrt[(2*congo2017[[5]] + 1)^2 - 8*(congo2017[[5]]-congo2017[[1]])]) / 2
#MSAVI_2017 <- ((2*congo2017[[5]] + 1) - sqrt((2*congo2017[[5]] + 1)^2 - 8 *(congo2017[[5]] - congo2017[[1]]))) / 2
#MSAVI_2020 <- ((2*congo2020[[5]] + 1) - sqrt((2*congo2020[[5]] + 1)^2 - 8 *(congo2020[[5]] - congo2020[[1]]))) / 2
#MSAVI_2023 <- ((2*congo2023[[5]] + 1) - sqrt((2*congo2023[[5]] + 1)^2 - 8 *(congo2023[[5]] - congo2023[[1]]))) / 2

# Plot MSAVI
#par(mfrow = c(1,3))
#plot(MSAVI_2017, col = cl, main = "MSAVI_2017")
#plot(MSAVI_2020, col = cl, main = "MSAVI_2020")
#plot(MSAVI_2023, col = cl, main = "MSAVI_2023")
#dev.off()

# Calcolo la differenza fra MSAVI_2018 e MSAVI_2023
#MSAVI_def <- MSAVI_2018 - MSAVI_2023

# Plot di NDVI_def + esportazione in .pdf
#pdf("MSAVI_def.pdf")
#plot(MSAVI_def, col = cl, main = "MSAVI_def")
#dev.off()

# GRIGIO: aree con differenza di NDVI fra 2018 e 2023 nulla
# GIALLO: perdita in copertura vegetale dal 2018 al 2023
# BLU:    guadagno in copertura vegetale dal 2018 al 2023



### VERIFICO SE NDVI_def e MSAVI_def DIFFERISCONO SIGNIFICATIVAMENTE

# Plot di NDVI_def e MSAVI_def + esportazione in .pdf
pdf("NDVI_def + MSAVI_def.pdf")
par(mfrow = c(1,2))
plot(NDVI_def, col = cl, main = "NDVI_def")
plot(MSAVI_def, col = cl, main = "MSAVI_def")
dev.off()

# Eseguo un T-test su NDVI_def e MSAVI_def
t_result <- t.test(NDVI_def[], MSAVI_def[], paired = TRUE)
t_result



??????????????????????????????????????????????????????????????????
  


#################### 3. PCA (PRINCIPAL COMPONENT ANALYSIS) #####################



# ImpostO il seme del generatore di numeri casuali
set.seed(1)

# Unisco gli indici NDVI_def e MSAVI_def in un unico oggetto
#box <- stack(NDVI_def)
sample <- sampleRandom(NDVI_def, 10000)
PCA <- prcomp(sample)

# Visualizzazione delle informazioni relative alla PCA
summary(PCA)

# Plot della varianza spiegata da ciascuna delle componenti
plot(PCA)

# Proiezione dell'oggetto box nello spazio creato precedentemente usando le CP
PCI <- predict(NDVI_def, PCA, index = 1:3)

# Plot della PC1
plot(PCI[[2]])

# Conversione di PC1 in un dataframe
PC_fin <- as.data.frame(PCI[[1]], xy = T)

# Plot 
plot_PCA <- ggplot() + 
  geom_raster(PC_fin, mapping = aes(x = x, y = y, fill = PC1)) + 
  scale_fill_viridis(option="magma") +
  labs(title = "PC1")

# Maggior variabilità = valore PC1 più basso
# Minor variabilità   = valore PC1 più alto

# Esportazione di final_plot in .pdf
pdf("PC1.pdf")
print(plot_PCA)
dev.off()

