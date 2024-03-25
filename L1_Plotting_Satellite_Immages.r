# LESSON 1: PLOTTING SATELLITE IMAGES

# Install the raster package and set the work directory
install.packages("raster")
library(raster)
setwd("C:/lab/p224r63_2011")

# Import the data with the function brick()
image_2011 <- brick("p224r63_2011_masked.grd")

# Visualize information
image_2011  # ncell = total pixels

#Plot the data
plot(image_2011)

#Pick a color palette
col <- colorRampPalette(c("red", "orange", "yellow")) (100) # 100 == n° of shades
col_3 <- colorRampPalette(c("red", "orange", "yellow")) (3)

# Plot with the colors palette we chose
plot(image_2011, col = col)
plot(image_2011, col = col_3) # less details = worse image quality

# Plot just one element of the multi-layer image
plot(image_2011[[4]], col = col)

# b1 = blue
# b2 = green
# b3 = red
# b4 = NIR

# Plotting the data in another way: by assignment
nir <- image_2011$B4_sre 
plot(nir, col = col)
plot(nir, col = col_3) # less shades mean less details

# Change of palette
new_palette <- colorRampPalette(c("aquamarine", "cadetblue", "cyan")) (100)
plot(image_2011, col = new_palette)
plot(image_2011[[4]], col = new_palette) # the fourth layer


# Exercise: plot the NIR band

# New colour palette
palette <- colorRampPalette(c("pink", "violet", "darkorchid4")) (100)
plot(image_2011[[4]], col = palette)

dev.off()  # to clean plots

# Function to export graphs in .pdf
pdf("my_1st_graph.pdf")
plot(image_2011[[4]], col = palette) 
dev.off()

# Function to export graphs in .png
png("first.png")
plot(image_2011[[4]], col = palette) 
dev.off()

# Function to plot 2 or more graphs in a multiframe
par(mfrow = c(2, 1))  # a multiframe with 2 rows and 1 column
plot(image_2011[[3]], col = palette) #red
plot(image_2011[[4]], col = palette) #NIR

par(mfrow = c(4, 1))  # a multiframe with 4 rows and 1 column
plot(image_2011[[2]], col = palette) #green
plot(image_2011[[3]], col = palette) #red
plot(image_2011[[2]], col = new_palette) #green
plot(image_2011[[3]], col = new_palette) #red

dev.off() # to remove the multiframe

# Here we can see the differences between 2 different shaded palette
par(mfrow = c(2, 1))
plot(nir, col = col) # 100-shaded palette, more details
plot(nir, col = col_3) # 3-shaded palette, less details

dev.off()

#blue
col_blue <- colorRampPalette(c("blue4", "blue2", "lightblue"))(100)
plot(image_2011[[1]], col = col_blue)

#green
col_green <- colorRampPalette(c("darkgreen", "green", "lightgreen"))(100)
plot(image_2011[[2]], col = col_green)

#aquamarine
col_aquamarine <- colorRampPalette(c("aquamarine4", "aquamarine2", "aquamarine"))(100)
plot(image_2011[[3]], col = col_aquamarine)

#pink
col_pink <- colorRampPalette(c("pink", "violet", "darkorchid4"))(100)
plot(image_2011[[4]], col = col_pink)

# RGB plotting, plot of multi-layered raster object
# 3 bands are combined such that they represent the red, green and blue channel
# This function can be used to make 'true (or false) color images
# from Landsat and other multi-band satellite images.

# Blue = 1
# Green = 2
# Red = 3

plotRGB(image_2011, r = 3, g = 2, b = 1, stretch = "Lin") # natural colors
plotRGB(image_2011, r = 4, g = 3, b = 2, stretch = "Lin") # vegetation is red
plotRGB(image_2011, r = 3, g = 2, b = 4, stretch = "Lin") # vegetation is blue
plotRGB(image_2011, r = 3, g = 4, b = 2, stretch = "Lin") # vegetation is green
# the fourth band is the NIR band, plants strongly reflect NIR

# Multiframe with natural and false colors
par(mfrow = c(2, 1))
plotRGB(image_2011, r = 3, g = 2, b = 1, stretch = "Lin")  # natural colors
plotRGB(image_2011, r = 4, g = 3, b = 2, stretch = "Lin")  # vegetation is red

# Histogram stretching
plotRGB(image_2011, r = 3, g = 2, b = 1, stretch = "Hist")  # natural colors
plotRGB(image_2011, r = 4, g = 3, b = 2, stretch = "Hist")  # vegetation is red

# Mixed stretching, differences between the 2 types of stretch
plotRGB(image_2011, r = 4, g = 3, b = 2, stretch = "Lin")  # vegetation is red
plotRGB(image_2011, r = 4, g = 3, b = 2, stretch = "Hist") # vegetation is red

dev.off()

# EXERCISE: plot the NIR band

# Import the 1988 image
setwd("C:/lab/p224r63_1988")
image_1988 <- brick("p224r63_1988_masked.grd")
image_1988

# Plot in RGB space (natural colours)
plotRGB(image_1988, r = 3, g = 2, b = 1, stretch = "Lin")


# Multiframe to see the differences between 1988 and 2011
par(mfrow = c(2, 1))
plotRGB(image_1988, r = 3, g = 2, b = 1, stretch = "Lin")  # natural colors
plotRGB(image_2011, r = 3, g = 2, b = 1, stretch = "Lin")

dev.off()

# Multiframe with 4 images
par(mfrow = c(2, 2))
plotRGB(image_1988, r = 3, g = 2, b = 1, stretch = "Lin")  # natural colors
plotRGB(image_2011, r = 3, g = 2, b = 1, stretch = "Lin")
plotRGB(image_1988, r = 3, g = 4, b = 2, stretch = "Hist")  
plotRGB(image_2011, r = 3, g = 4, b = 2, stretch = "Hist")

dev.off()
