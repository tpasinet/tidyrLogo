# This script constructs a graph representing a design concept for the R package tidyr
# tidyr is a package that helps analysts transform mishapen data in a "tidy" format ready for analysis and visualization
# The logo reflects the goal of tidyr by forming the word tidyr with uniform shapes of the same color amid a field of randomly plotted shapes rndomly colored

# Load libraries
library("ggplot2")

# Set the range of the xy plane
xRange <- 19
yRange <- 9

# Generate x and y values for the xy plane for each column-row intersection
xValues <- rep(1:xRange, yRange)
yValues <- rep(1:yRange, each = xRange)

# Create a data frame with the x and y values where on row contains a coordinate for a column-row intersection in the plane
logoPlane <- data.frame(xValue = xValues, yValue = yValues)

# Generate x and y values for the xy plane for each column-row intersections that create "tidyr" 
logoXValues <- c(2,3,3,3,3,3,4,6,6,6,6,8,8,8,9,9,10,10,10,10,10,12,12,12,12,13,13,14,14,14,14,14,16,16,16,17,18)
logoYValues <- c(6,4,5,6,7,8,6,4,5,6,8,4,5,6,4,6,4,5,6,7,8,2,4,5,6,2,4,2,3,4,5,6,4,5,6,6,6)

# Create a data frame with the x and y values where one row contains a coordinate for a column-row intersection that create "tidyr"
# Create a new column that indicates these rows are used to create the logo
logoPoints <- data.frame(xValue = logoXValues, yValue = logoYValues, isLogoCoordinate = TRUE)

# Merger the plane and point data frames so that each coordiante that is used in word is distinguidhed from the general plane
logoPlane <- merge(x = logoPlane, y = logoPoints, by = c("xValue","yValue"), all = TRUE)
# Set the values of rows that are not used in the word from NA to FALSE
logoPlane$isLogoCoordinate[is.na(logoPlane$isLogoCoordinate)] <- FALSE

# Set the color and shape codes used for the plane coordinates
logoPlaneColor <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00")
logoPlaneShape <- c(0,1,2,5,6)

# Add color and shape columns to the plane data frame. Randomly set color and shape values using the colors and shapes previous selected 
logoPlane$logoColor <- sapply(1:(xRange * yRange), function(x) sample(logoPlaneColor,1))
logoPlane$logoShape <- sapply(1:(xRange * yRange), function(x) sample(logoPlaneShape,1))

# Set the color and shape of the word coordinates to a specific color and a specific shape not previous set in the color and shape sets
logoPlane$logoColor[which(logoPlane$isLogoCoordinate == TRUE)] <- "#000000" 
logoPlane$logoShape[which(logoPlane$isLogoCoordinate == TRUE)] <- 15

# Construct the plot so that the color and shape of each coodinate is set from the assigned values in the data frame. Additionally, make the letter coordinate shapes bigger than the plane shapes.
tidyrLogo <- ggplot() +
  scale_y_continuous(name="") +
  scale_x_continuous(name="") +
  scale_shape_identity() +
  scale_colour_identity() +
  geom_point(data=logoPlane[which(logoPlane$isLogoCoordinate == FALSE),], mapping=aes(x=xValue, y=yValue, shape=logoShape, color=logoColor), size=10, position=position_jitter(width = 0.25, height = 0.25)) +
  geom_point(data=logoPlane[which(logoPlane$isLogoCoordinate == TRUE),], mapping=aes(x=xValue, y=yValue, shape=logoShape, color=logoColor), size=15)

# Remove almost all plot area elements and return the plot
tidyrLogo + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
