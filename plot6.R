# 6. Compare emissions from motor vehicle sources in Baltimore City with
# emissions from motor vehicle sources in Los Angeles County, California
# (fips == "06037"). Which city has seen greater changes over time in motor
# vehicle emissions?

library(ggplot2)

# read in data
# NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

# Filter down to only motor vehicle data. According to data description at
# http://www.epa.gov/ttn/chief/net/2008neiv3/2008_neiv3_tsd_draft.pdf
# the onroad type of emission data reports on "passenger cars, motorcycles,
# minivans, sport utility vehicles, light duty trucks, heavy duty trucks, and buses"
onroadRows <- NEI[, "type"] == "ON-ROAD"
motorVehicleNEI <- NEI[onroadRows, ]

# filter down to only data from Baltimore City, MD or LA, CA
baltimoreRows <- motorVehicleNEI[, "fips"] == "24510"
baltimoreNEI <- motorVehicleNEI[baltimoreRows, ]
losAngelesRows <- motorVehicleNEI[, "fips"] == "06037"
losAngelesNEI <- motorVehicleNEI[losAngelesRows, ]

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
          + geom_text(x = 2.5, y = 2100, label = "Abs value of the slope of the fitted line")
          + geom_text(x = 2.5, y = 1900, label = "is greater for LA -> Greater change")
          )
}

createPlot()
outputToPNG("plot6.png", createPlot)
