# 4. Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?

library(ggplot2)

# read in data
NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

# find SCC ids for coal combustion sources
SCC_ids <- vector(mode = "character")
apply(SCC, 1, function(row) {
    if (grepl("Coal", row[["Short.Name"]], fixed = TRUE)) {
        SCC_ids <<- append(SCC_ids, row[["SCC"]])
    }
})

# Create a function to separate printing to a PNG from creating the plot
outputToPNG <- function(fileName, createPlot) {
    png(file = fileName)
    createPlot()
    dev.off()
}

createPlot <- function() {

    # remove data with SCCs that don't deal with coal
    coalSCCs <- NEI[, "SCC"] %in% SCC_ids
    coalNEI <- NEI[coalSCCs, ]
    
    # calculate the total emissions by year
    years <- vector(mode = "character")
    emissions <- vector(mode = "numeric")
    split_NEI <- split(coalNEI, coalNEI$year)
    for(year in names(split_NEI)) {
        yearDataFrame <- split_NEI[[year]]
        yearEmissions <- sum(yearDataFrame$Emissions)
        years <- append(years, year)
        emissions <- append(emissions, yearEmissions)
    }
    
    # construct the data frame and plot it
    emissionData <- data.frame(years, emissions)
    emissionData$emissionsInThousandsOfTons <-
        apply(emissionData, 1, function(row) {
            as.numeric(row[["emissions"]])/1000
        })
    g <- qplot(years, emissionsInThousandsOfTons, data = emissionData)
    print(g + geom_point()
          + geom_smooth(mapping = aes(group = 1), method = "lm")
          + labs(title = expression('U.S. Emissions from Coal Combustion'),
                 x = "Year",
                 y = "Emissions (thousands of tons)")
          + theme_bw(base_family = "Times"))
}

createPlot()
outputToPNG("plot4.png", createPlot)
