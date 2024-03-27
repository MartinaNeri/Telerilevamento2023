
# LESSON 4: CLASSIFICATION OF REMOTE SENSING DATA

# Load the raster package
library(raster)

# Set the working directory
setwd("C:/lab")

# IMAGE OF THE SUN

# brick() function: stratified files
# raster() function: image with one layer

# Import the image (.jpg)
sun <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")

# Visualize the image imported
plotRGB(sun, 1, 2, 3, stretch="lin")
plotRGB(sun, 1, 2, 3, stretch="hist")

# Check information about the image
sun





# CLASSIFICATION

# 1. GET VALUES
single_nr <- getValues(sun)
# returns all values or the values for a number of rows of a raster object
single_nr
# From the image with layers to the values of the reflectance of each layer

# 2. CLASSIFY
k_cluster <- kmeans(single_nr, centers=3) # Center = n. of classes/cluster
# Clustering means = centroid of a cluster of pixels
k_cluster

# 3. SET VALUES TO A RASTER ON THE BASIS OF THE SUN IMAGE
sun_class <- setValues(sun[[1]], k_cluster$cluster) # Assign new value to a
                                                    # raster object
# From continuous data to a raster object
# Using the fist layer of the sun image and the cluster component of kmeans
# the first layer is like a box to be filled

# Plot using a colour palette
cl_1 <- colorRampPalette(c("yellow","black","red"))(100)
plot(sun_class, col=cl_1)

# Class 1: highest energy level
# Class 2: medium energy level
# Class 3: lowest energy level

# Claculate frequency of pixels in clusters
frequencies <- freq(sun_class)
frequencies

# value  count
#     1 829341
#     2 920711
#     3 471388

tot <- ncell(sun_class)
tot # = 2221440

percentage <- round((frequencies*100)/tot, digit=5)
percentage # Count colums are the percentage frequencies

#   value    count
# 0.00005 21.21993
# 0.00009 37.33349
# 0.00014 41.44658





# GRAND CANYON

# Import Grand Canyon image
grand_canyon <- brick("dolansprings_oli_2013088_canyon_lrg.jpg")

# Visualize the image imported
plotRGB(grand_canyon, 1, 2, 3, stretch="lin")
plotRGB(grand_canyon, 1, 2, 3, stretch="hist")

# Check information about the image
grand_canyon

# 1. GET VALUES
single_nr_1 <- getValues(grand_canyon)
# returns all values or the values for a number of rows of a raster object
single_nr_1
# From the image with layers to the values of the reflectance of each layer

# 2. CLASSIFY
k_cluster_1 <- kmeans(single_nr_1, centers=3) # Center = n. of classes/cluster
# Clustering means = centroid of a cluster of pixels
k_cluster_1

# 3. SET VALUES TO A RASTER ON THE BASIS OF THE SUN IMAGE
gc_class <- setValues(grand_canyon[[1]], k_cluster_1$cluster) 
# Assign new value to a raster object

# Plot using a colour palette
cl_1 <- colorRampPalette(c("yellow","black","red"))(100)
plot(gc_class, col=cl_1)

# Class 1: volcanic rock
# Class 2: sandstone
# Class 3: conglomerates

# Claculate frequency of pixels in clusters
frequencies_1 <- freq(gc_class)
frequencies_1

# value    count
#     1  9592103
#     2 20602220
#     3 27881825

tot_1 <- ncell(gc_class)
tot_1 # = 58076148

percentage_1 <- round((frequencies_1*100)/tot_1, digit=5)
percentage_1 # Count colums are the percentage frequencies

# value    count
# 0e+00 16.51642
# 0e+00 35.47449
# 1e-05 48.00908





# EXERCISE: classify the map with 4 classes

# 1. GET VALUES
single_nr_2 <- getValues(grand_canyon)
single_nr_2

# 2. CLASSIFY
k_cluster_2 <- kmeans(single_nr_2, centers=4)
k_cluster_2

# 3. SET VALUES TO A RASTER ON THE BASIS OF THE SUN IMAGE
gc_class_2 <- setValues(grand_canyon[[1]], k_cluster_1$cluster) 

# Plot using a colour palette
cl_2 <- colorRampPalette(c("yellow","lightblue","black","red"))(100)
plot(gc_class_2, col=cl_2)

# Claculate frequency of pixels in clusters
frequencies_2 <- freq(gc_class_2)
frequencies_2

# value    count
#     1  9592103
#     2 20602220
#     3 27881825

tot_2 <- ncell(gc_class_2)
tot_2 # = 58076148

percentage_2 <- round((frequencies_2*100)/tot_2, digit=5)
percentage_2

# value    count
# 0e+00 16.51642
# 0e+00 35.47449
# 1e-05 48.00908
