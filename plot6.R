# 6. Compare emissions from motor vehicle sources in Baltimore City with
# emissions from motor vehicle sources in Los Angeles County, California
# (fips == "06037"). Which city has seen greater changes over time in motor
# vehicle emissions?

# library(ggplot2)
# 
# # read in data
# NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
# SCC <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")
# 
# # filter down to only motor vehicle data from Baltimore City, MD and LA, CA
# onroadRows <- NEI[, "type"] == "ON-ROAD"
# motorVehicleNEI <- NEI[onroadRows, ]
# baltimoreRows <- motorVehicleNEI[, "fips"] == "24510"
# baltimoreNEI <- motorVehicleNEI[baltimoreRows, ]
# losAngelesRows <- motorVehicleNEI[, "fips"] == "06037"
# losAngelesNEI <- motorVehicleNEI[losAngelesRows, ]

# Create a function to separate printing to a PNG from creating the plot
outputToPNG <- function(fileName, createPlot) {
    png(file = fileName)
    createPlot()
    dev.off()
}

createPlot <- function() {
    
    # calculate the total emissions by year for the two counties
    years <- vector(mode = "character")
    emissions <- vector(mode = "numeric")
    location <- vector(mode = "character")
    laSplitNEI <- split(losAngelesNEI, losAngelesNEI$year)
    for(year in names(laSplitNEI)) {
        yearDataFrame <- laSplitNEI[[year]]
        yearEmissions <- sum(yearDataFrame$Emissions)
        years <- append(years, year)
        emissions <- append(emissions, yearEmissions)
    }
    numYears <- length(years)
    location <- rep("Los Angeles", numYears)
    bcSplitNEI <- split(baltimoreNEI, baltimoreNEI$year)
    for(year in names(bcSplitNEI)) {
        yearDataFrame <- bcSplitNEI[[year]]
        yearEmissions <- sum(yearDataFrame$Emissions)
        years <- append(years, year)
        emissions <- append(emissions, yearEmissions)
    }
    location <- append(location, rep("Baltimore", numYears))
    
    # construct the data frame and plot it
    emissionData <- data.frame(years, emissions, location)
    g <- qplot(years, emissions, data = emissionData) # , facets = .~location
    print(g + geom_point(aes(color = location))
          + geom_smooth(
              mapping = aes(group = 1), method = "lm",
              data = emissionData[emissionData$location == "Los Angeles", ])
          + geom_smooth(
              mapping = aes(group = 1), method = "lm",
              data = emissionData[emissionData$location == "Baltimore", ])
          + labs(title = expression('Motor Vehicle Emissions in Baltimore and Los Angeles'),
                 x = "Year",
                 y = "Emissions (tons)")
          )
}

createPlot()
outputToPNG("plot6.png", createPlot)
