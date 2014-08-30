# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to
# 2008? Using the base plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.

library(datasets)

# read in data
NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

# Create a function to separate printing to a PNG from creating the plot
outputToPNG <- function(fileName, createPlot) {
    png(file = fileName)
    createPlot()
    dev.off()
}

createPlot <- function() {

    # calculate the total emissions by year
    totalPM25ByYear <- tapply(NEI$Emissions, NEI$year, sum)
    # convert values to millions of tons for clarity of display
    emissionsInMillionsOfTons <- lapply(totalPM25ByYear, function(e) { e/1000000 })

    plot(names(totalPM25ByYear), emissionsInMillionsOfTons,
         type = "l",
         main = expression('Total U.S. PM'[2.5]*' Emissions'),
         xlab = "Year",
         ylab = "Emissions (millions of tons)")
}

createPlot()
outputToPNG("plot1.png", createPlot)
