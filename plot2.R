# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a
# plot answering this question.

library(datasets)

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
    
    # calculate the total emissions by year
    years <- vector(mode = "character")
    emissions <- vector(mode = "numeric")
    split_NEI <- split(baltimoreNEI, baltimoreNEI$year)
    for(year in names(split_NEI)) {
        yearDataFrame <- split_NEI[[year]]
        yearEmissions <- sum(yearDataFrame$Emissions)
        years <- append(years, year)
        emissions <- append(emissions, yearEmissions)
    }
    
    # construct the data frame and plot it
    emissionData <- data.frame(years, emissions)
    with(emissionData,
         plot(years, emissions,
              type = "l",
              main = expression('Total Baltimore PM'[2.5]*' Emissions'),
              xlab = "Year",
              ylab = "Emissions (tons)"))
    
    # TODO add a trendline!
}

createPlot()
outputToPNG("plot2.png", createPlot)
