# LESSON 3: TIME SERIES ANALYSIS

# Install the required packages
install.packages("rasterVis")
install.packages("rgdal")

# Load the required packages
library(raster)
library(rasterVis)
library(rgdal)

# EX 1. GREENLAND INCRESE OF TEMPERATURES

# Set the working directory
setwd("C:/lab/greenland_data")

# Import .tif image with raster() function
lst_2000 <- raster("lst_2000.tif")
lst_2005 <- raster("lst_2005.tif")
lst_2010 <- raster("lst_2010.tif")
lst_2015 <- raster("lst_2015.tif")

# See the list of object imported/loaded
ls()

# Plot in a multiframe the 4 images
par(mfrow=c(2,2))
plot(lst_2000)
plot(lst_2005)
plot(lst_2010)
plot(lst_2015)
dev.off()

# List of files
rlist_1 <- list.files(pattern="lst")
rlist_1

# Apply a function over a list/vector
import_1 <- lapply(rlist_1, raster) # Apply a function to many files of a list
import_1

# Stack vectors from a dataframe/list
TGr <- stack(import_1)
TGr # 4 layesr
# Stacking vectors concatenates multiple vectors into a single vector

# Plot TGr vector
plot(TGr) # now we have the 4 images in a single element

# Plot with plotRGB()
plotRGB(TGr, 1, 2, 3, stretch="Lin")
plotRGB(TGr, 2, 3, 4, stretch="Lin")

# Plot using a colour palette
cl_0 <- colorRampPalette(c("blue","lightblue","pink","red"))(100)
plot(TGr, col=cl_0)
dev.off()

# Calculate and plot the differences between 2005 and 2000
dif_05_00 = TGr[[2]] - TGr[[1]]
plot(dif_05_00, col=cl_0)
dev.off()

# EX 2. NO2 DECREASE DURING LOCKDOWN PERIOD

# Set the new working directory
setwd("C:/lab/EN")

# Import and plot the first image file
EN01 <- raster("EN_0001.png")
EN01
plot(EN01) # Jenuary (before lockdown began)

# Plot EN01 using colour palette
cl_1 <- colorRampPalette(c("yellow","orange","red"))(100)
plot(EN01, col=cl_1)

# Import and plot the last image file
EN13 <- raster("EN_0013.png")
EN13
plot(EN13, col=cl_1) # March (after lockdown began)

# confronto
par(mfrow=c(2,1))
plot(EN01)
plot(EN13)

# Import the whole set
rlist_2 <- list.files(pattern="EN")
rlist_2

# Apply a function over a list/vector
import_2 <- lapply(rlist_2, raster) # Apply a function to many files of a list
import_2

# Stack vectors from a dataframe/list
EN <- stack(import_2)
EN
# Stacking vectors concatenates multiple vectors into a single vector

# Plot all the image files
plot(EN, col=cl_1)

# Check by visualizing the plots in a multiframe
par(mfrow=c(1,2))
plot(EN[[1]], col=cl_1)
plot(EN01, col=cl_1)
dev.off()

# Check by looking for differences
dif_check <- EN01 - EN[[1]]
dif_check
plot(dif_check)

# Plot the first and the last images with par() function
par(mfrow=c(1,2))
plot(EN01, col=cl_1)
plot(EN13, col=cl_1)
dev.off()

# Plot the first and the last images with stack() function
EN_01_13 <- stack(EN01, EN13)

# Calculate the differences between EN01 and EN13
dif_01_13 <- EN01 - EN13
cl_2 <- colorRampPalette(c("blue","white","red"))(100)

# Plot the differences between EN01 and EN13 with plot()
plot(dif_01_13, col=cl_2)
dev.off()

# Plot with plotRGB()
par(mfrow=c(1,2))
plotRGB(EN, 1, 7, 13, stretch="lin")
plotRGB(EN, 1, 7, 13, stretch="hist")

