# 4. Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?

# TODO lib import

# # read in data
# NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
# SCC <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")
# 
# # find SCC ids for coal combustion sources
# SCC_ids <- vector(mode = "character")
# apply(SCC, 1, function(row) {
#     if (grepl("Coal", row[["Short.Name"]], fixed = TRUE)) {
#         SCC_ids <<- append(SCC_ids, row[["SCC"]])
#     }
# })

# Create a function to separate printing to a PNG from creating the plot
outputToPNG <- function(fileName, createPlot) {
    png(file = fileName)
    createPlot()
    dev.off()
}

createPlot <- function() {
    print(class(SCC_ids)) ## TODO remove
    ## remove data with SCCs that don't deal with coal
    coalSCCs <- NEI[, "SCC"] %in% SCC_ids
    coalNEI <- NEI[coalSCCs, ]
    print("hi2") ## TODO remove
    
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
    
    # TODO change to thousands of tons
    
    # construct the data frame and plot it
    emissionData <- data.frame(years, emissions)
    g <- qplot(years, emissions, data = emissionData #,
               #ylim = c(0, 2500)  # start the axis at 0 since emission totals cannot be negative
    )
    print(g + geom_point()
          + geom_smooth(mapping = aes(group = 1), method = "lm")
          + labs(title = expression('Baltimore PM'[2.5]*' Emissions by Type and Year'), # TODO change
                 x = "",
                 y = "Emissions (tons)")
          + theme_bw(base_family = "Times"))
}

createPlot()
#outputToPNG("plot4.png", createPlot)