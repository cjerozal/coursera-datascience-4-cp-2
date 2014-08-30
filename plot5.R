# 5. How have emissions from motor vehicle sources changed from 1999–2008 in
# Baltimore City?

library(ggplot2)

# read in data
NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

# filter down to only data from Baltimore City, Maryland
baltimoreRows <- NEI[, "fips"] == "24510"
filteredNEI <- NEI[baltimoreRows, ]

# Filter down to only motor vehicle data. According to data description at
# http://www.epa.gov/ttn/chief/net/2008neiv3/2008_neiv3_tsd_draft.pdf
# the onroad type of emission data reports on "passenger cars, motorcycles,
# minivans, sport utility vehicles, light duty trucks, heavy duty trucks, and buses"
onroadRows <- filteredNEI[, "type"] == "ON-ROAD"
twiceFilteredNEI <- filteredNEI[onroadRows, ]

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
    split_NEI <- split(twiceFilteredNEI, twiceFilteredNEI$year)
    for(year in names(split_NEI)) {
        yearDataFrame <- split_NEI[[year]]
        yearEmissions <- sum(yearDataFrame$Emissions)
        years <- append(years, year)
        emissions <- append(emissions, yearEmissions)
    }
    
    # construct the data frame and plot it
    emissionData <- data.frame(years, emissions)
    g <- qplot(years, emissions, data = emissionData,
               ylim = c(0, 400))  # start the axis at 0 since emission totals cannot be negative)
    print(g + geom_point()
          + geom_smooth(mapping = aes(group = 1), method = "lm")
          + labs(title = expression('Motor Vehicle Emissions in Baltimore City'),
                 x = "Year",
                 y = "Emissions (tons)")
          + theme_bw(base_family = "Times"))
}

createPlot()
outputToPNG("plot5.png", createPlot)
