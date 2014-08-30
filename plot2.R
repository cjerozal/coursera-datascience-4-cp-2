# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a
# plot answering this question.

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
    
    # filter down to only data from Baltimore City, Maryland
    baltimoreRows <- NEI[, "fips"] == "24510"
    baltimoreNEI <- NEI[baltimoreRows,]

    # calculate the total emissions by year
    totalPM25ByYear <- tapply(baltimoreNEI$Emissions, baltimoreNEI$year, sum)

    plot(names(totalPM25ByYear), totalPM25ByYear,
         type = "l",
         main = expression('Total Baltimore PM'[2.5]*' Emissions'),
         xlab = "Year",
         ylab = "Emissions (tons)")
    
    # add a trendline
    line <- lm(totalPM25ByYear ~ as.numeric(names(totalPM25ByYear)))
    abline(a = coef(line)[1], b = coef(line)[2], col = "blue")
}

createPlot()
outputToPNG("plot2.png", createPlot)
