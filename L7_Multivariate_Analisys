# LESSON 7: MULTIVARIATE ANALYSIS

# Load the required packages
library(raster)
library(ggplot2)
library(viridis)

# Set the working directory
setwd("C:/lab/")

# Import data
sen_1 <- brick("sentinel.png")
sen_1
ncell(sen_1) #633612
plot(sen_1)

# In order to group layers, we have to remove the last one (empty)
sen_2 <- stack(sen_1[[1]], sen_1[[2]], sen_1[[3]])
sen_2
plot(sen_2)

# Pair-wise comparison
pairs(sen_2)

# PCA (Principal Component Analysis)
sample <- sampleRandom(sen_2, 10000)
PCA <- prcomp(sample)

# Variance explained
summary(PCA)
# The first in list is the component with highest vartiability

# Viene calcolata la correlazione tra le componenti principali e le bande originali dell'immagine.
# facendo un summary restituisce "Importance of components"
# nella riga "proportion of Variance" è riportata la percentuale di quanto ogni componente spiega

# in questo caso: ???????????????????????????????????
# prima componente spiega 67,08 %
# seconda componente spiega 32,55 %
# tot: 99,63 % che è pienamente sufficiente 


# Correlation with original bands
PCA

# Pc map: we visualize starting from the analysis of the PCA
PCI <- predict(sen_2, PCA, index=c(1:3)) # or index=c(1:2)
plot(PCI)
plot(PCI[[1]])
plot(PCI[[2]])
plot(PCI[[3]])

# Coerce into a dataframe
PCId <- as.data.frame(PCI[[1]], xy=T)
PCId

# Plot with ggplot()
ggplot() +
  geom_raster(PCId,
              mapping=aes(x=x, y=y, fill=PC1)) +
  scale_fill_viridis() # "direction=-1" in case of reverse colour ramp

# Plot with ggplot() _ colour palette = "magma"
ggplot()+
  geom_raster(PCId,
              mapping=aes(x=x, y=y, fill=PC1))+
  scale_fill_viridis(option="magma")

# Plot with ggplot() _ colour palette = "inferno"
ggplot()+
  geom_raster(PCId,
              mapping=aes(x=x, y=y, fill=PC1))+
  scale_fill_viridis(option="inferno")

# Focal standard deviation
SD3 <- focal(PCI[[1]], matrix(1/9, 3, 3), fun=sd)
#Viene calcolata la deviazione standard focale sulla
#prima componente principale utilizzando la funzione focal()

# Coerce into a dataframe
SD3d <- as.data.frame(SD3, xy=T)
SD3d

# Plot with ggplot()
ggplot() +
  geom_raster(SD3d,
              mapping=aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis()

sen_1

# Speeding up analysis
# Aggregate cells: re-sampling
SEN_res <- aggregate(sen_1, fact=10)
SEN_res
plot(SEN_res)
#L'immagine viene aggregata o campionata 
#con un fattore di 10 utilizzando aggregate() per accelerare l'analisi.

# Then repeat the Principal Component Analysis
