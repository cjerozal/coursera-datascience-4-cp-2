# 3. Of the four types of sources indicated by the type (point, nonpoint,
# onroad, nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999–2008 for Baltimore City? Which have seen increases in
# emissions from 1999–2008? Use the ggplot2 plotting system to make a plot
# answer this question.

library(ggplot2)

# read in data
# NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

# Create a function to separate printing to a PNG from creating the plot
outputToPNG <- function(fileName, createPlot) {
    png(file = fileName)
    createPlot()
    dev.off()
}

createPlot <- function() {
    
    # filter down to only data from Baltimore City, Maryland
    baltimoreRows <- NEI[, "fips"] == "24510"
    baltimoreNEI <- NEI[baltimoreRows,]
    
    # calculate the total emissions by year and type
    emissionTypes <- vector(mode = "character") # TODO could be factor?
    years <- vector(mode = "character") # TODO could be factor?
    emissions <- vector(mode = "numeric")
    splitNEIByType <- split(baltimoreNEI, baltimoreNEI$type)
    for(emissionType in names(splitNEIByType)) {
        typeDataFrame <- splitNEIByType[[emissionType]]
        splitNEIByYear <- split(typeDataFrame, typeDataFrame$year)
        for(year in names(splitNEIByYear)) {
            currentYearAndTypeDF <- splitNEIByYear[[year]]
            yearTypeEmissions <- sum(currentYearAndTypeDF$Emissions)
            emissionTypes <- append(emissionTypes, emissionType)
            years <- append(years, year)
            emissions <- append(emissions, yearTypeEmissions)
        }
    }
    
    
    # construct the data frame and plot it
    emissionData <- data.frame(emissionTypes, years, emissions)
    print(emissionData)
}

createPlot()
# outputToPNG("plot3.png", createPlot)
