# LESSON 2: VEGETATION INDICES

#Vegetational indices are mathematical combination or transformation of spectral bands that
#accentuates the spectral properties of green plants so that they appear distinct from other image features. 
#VI should:
#1. Indicate the AMOUNT of vegetation (e.g., %cover, LAI, biomass, etc.), 
#2. Distinguish between soil and vegetation
#3. Reduce atmospheric and topographic effects if possible

# Install the raster package
install.packages("raster")
library(raster)

# Setting the working directory
setwd("C:/lab")

# IMAGE FROM 1992

# Import the image.png from 1992
img_1992 <- brick("defor1_.png")
img_1992

# Plot the image with plot() and plotRGB()
plot(img_1992) # 3 layers
plotRGB(img_1992, 1, 2, 3, stretch="Lin")

# What is the NIR layer? NIR is red since the image is red
# NIR = 1
# Red = 2
# Green = 3

# Images exported by NASA with NIR, first band is 1

# Calculate DVI for 1992
#DVI = Difference Vegetation Index calculated as NIR minus red#
dvi_1992 <- img_1992[[1]]-img_1992[[2]] # NIR - Red
dvi_1992

# Plot the DVI (1992)
cl <- colorRampPalette(c("darkblue", "yellow","red","black"))(100)
plot(dvi_1992, col=cl) # the darker the red the healthier the vegetation

plot(img_1992)
# reflectance is between 0 and 255
# so the DVI image range is -255 to 255, because red is 0-255





# IMAGE FROM 2006

# Import the image.png from 2006
img_2006 <- brick("defor2_.png")
img_2006

# Plot the image with plot() and plotRGB()
plot(img_2006)
plotRGB(img_2006, 1, 2, 3, stretch="Lin")

# Calculate DVI for 2006
dvi_2006 <- img_2006[[1]]-img_2006[[2]] # NIR - Red
dvi_2006

# Plot the DVI (2006)
plot(dvi_2006, col=cl)





# TIME DIFFERENCES

# Plot the image from 1992 on top of that of 2006
par(mfrow = c(2, 1))
plotRGB(img_1992, 1, 2, 3, stretch = "Lin")
plotRGB(img_2006, 1, 2, 3, stretch = "Lin")
devo.off()

# Plot the image from 1992 beside that of 2006
par(mfrow = c(1,2))
plotRGB(img_1992, 1, 2, 3, stretch = "Lin")
plotRGB(img_2006, 1, 2, 3, stretch = "Lin")
dev.off()


# MULTITEMPORAL ANALYSIS

# Calculate the difference of DVI from 1992 to 2006
dvi_dif = dvi_1992 - dvi_2006

# Plot the result
cld <- colorRampPalette(c("blue","white","red"))(100)
plot(dvi_dif, col=cld)

# The higher the difference the bigger the deforestation
# If the difference is negative there is a gain in forest cover





# NDVI (NORMALIZED DIFFERENCE VEGETATION INDEX)

# Range DVI (8 bit): -255 a 255
# Range NDVI (8 bit): -1 a 1

# Range DVI (16 bit): -65535 a 65535
# Range NDVI (16 bit): -1 a 1

# Hence, NDVI can be used to compare images with a different radiometric resolu-
# tion

# Calculate NDVI for 1992
ndvi_1992 = dvi_1992 / (img_1992[[1]] + img_1992[[2]])  # standardization

# Plot the result
plot(ndvi_1992, col=cl) # Now the index is standard, from -1 to +1

# Calculate NDVI for 2006
ndvi_2006 = dvi_2006 / (img_2006[[1]] + img_2006[[2]])  # standardization

# Plot the result
plot(ndvi_2006, col=cl) # Now the index is standard, from -1 to +1

# Calculate differences of NDVI between 1992 and 2006
ndvi_dif = ndvi_1992 - ndvi_2006
plot(ndvi_dif, col=cld)
