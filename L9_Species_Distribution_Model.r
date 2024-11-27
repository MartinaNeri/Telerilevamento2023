# LESSON 9: SPECIES DISTRIBUTION MODELS

# Install and load the required packages
install.packages("sdm") # Species Distribution Modelling
library(sdm)
library(rgdal)
library(raster)

# Load a file found within the sdm package
file <- system.file("external/species.shp", package="sdm")
file
species_data <- shapefile("C:/Users/Martina/AppData/Local/R/win-library/4.2/sdm/external/species.shp")

# Visualize informations about the dataset
species_data

# Occurrence = Presence/Absence of the species (or 1|0)

# Plot the data about species
plot(species_data)
plot(species_data, pch=19) # pch: pattern of the occurence points

# Looking at the occurrences
species_data$Occurrence

# Plot the presences, 1 = presence
presences <- species_data[species_data$Occurrence==1,]
presences
plot(presences, col="blue", pch=19)

# Plot the absences, 0 = absence
absences <- species_data[species_data$Occurrence==0,]
absences
plot(absences, col="red", pch=19)

# Plotting all the occurences divided by colour
plot(presences, col="blue", pch=19)
points(absences, col="red", pch=19)

# Predictors
path <- system.file("external", package="sdm")
path

# List and stack the predictors
lst <- list.files(path=path, pattern="asc", full.names=T)
lst
predictors <- stack(lst)
predictors

# Plot the predictors
plot(predictors)
plot(predictors$vegetation)

# Remember: we don't like this kind of palette -> from blue to red
cl_1 <- colorRampPalette(c("lightyellow", "yellow", "orange", "darkorange", "darkred"))(100)
plot(predictors$vegetation, col=cl_1)

# Plot predictors and occurences together
plot(predictors$elevation, col=cl_1, main="Elevation")
points(presences, pch=19) # This specie occurs in lower altitude

plot(predictors$precipitation, col=cl_1, main="Precipitation")
points(presences, pch=19) # This specie occurs where it's quite moist

plot(predictors$temperature, col=cl_1, main="Temperature")
points(presences, pch=19) # This species occurs where it's hotter

plot(predictors$vegetation, col=cl_1, main="Vegetation")
points(presences, pch=19) # This specie occurs where the vegetation cover is higher

# Predictors: predict patterns





# CREATE A MODEL

# Setting data for sdm
data_sdm <- sdmData(train=species_data, predictors=predictors)
data_sdm

# Model
m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation,
          data=data_sdm,
          methods="glm") # Widely used in ecology
m1

# Make the raster output layer
p1 <- predict(m1, newdata=predictors) # predictors as extension and grain
p1

# Plot the output
plot(p1, col=cl_1)
points(presences, pch=19)

# Add to the stack
s1 <- stack(predictors, p1)
plot(s1, col=cl_1)

# Change name to the last plot
names(s1)[5] <- c("model")
plot(s1, col=cl_1)
