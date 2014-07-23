# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to
# 2008? Using the base plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.

library(datasets)

# read in data
NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

# Create a function to separate printing to a PNG from creating the plot
outputToPNG <- function(fileName, createPlot) {
    png(file = fileName)
    createPlot()
    dev.off()
}

createPlot <- function() {

    # calculate the total emissions by year
    years <- vector(mode = "character")
    emissions <- vector(mode = "numeric")
    split_NEI <- split(NEI, NEI$year)
    for(year in names(split_NEI)) {
        yearDataFrame <- split_NEI[[year]]
        yearEmissions <- sum(yearDataFrame$Emissions)
        years <- append(years, year)
        emissions <- append(emissions, yearEmissions)
    }

    # construct the data frame and plot it
    emissionData <- data.frame(years, emissions)
    emissionData$emissionsInMillionsOfTons <- 
        apply(emissionData, 1, function(row) {
            as.numeric(row[["emissions"]])/1000000
        })
    with(emissionData,
         plot(years, emissionsInMillionsOfTons,
              type = "l",
              main = expression('Total U.S. PM'[2.5]*' Emissions'),
              xlab = "Year",
              ylab = "Emissions (millions of tons)"),
              pch = 1)
}

createPlot()
outputToPNG("plot1.png", createPlot)
