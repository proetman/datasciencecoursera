library(ggplot2)

# Clear all previous data

rm(list=ls())


# determine the current working directory
cwd <- getSrcDirectory(function(x) {x})
if ( nchar(cwd) < 1 ){
        print("ERROR: Unable to determine current working directory")
} else {
        print(paste("Working directory :", cwd))
}

# Setup variables for the various files
data_url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
zip_file <- paste(sep='/', cwd, 'exdata%2Fdata%2FNEI_data.zip')
nei_rds <- paste(sep='/', cwd, 'summarySCC_PM25.rds')
scc_rds <- paste(sep='/', cwd, 'Source_Classification_Code.rds')
target_file <- paste(sep='/', cwd, 'plot3.png')
title <- "Annual PM2.5 emissions for Baltimore by Type\nfor all sources (Thousands of tons)"

# Download the data, if required - and unzip
if (! file.exists(zip_file)) {
        print("    Downloading file....")
        download.file(data_url, destfile = zip_file, method = 'curl')
        print("    Unzipping...")
        unzip(zip_file, exdir = cwd)
}

# Read the RDS data
NEI <- readRDS(nei_rds)
SCC <- readRDS(scc_rds)





# Question 3
# Totals for Baltimore by source (point, nonpoint, onroad, nonroad)
# Use ggplot

NEI_baltimore <- subset(NEI, fips == "24510")

baltimore_point    <- subset(NEI_baltimore, type=="POINT")
baltimore_nonpoint <- subset(NEI_baltimore, type=="NONPOINT")
baltimore_onroad   <- subset(NEI_baltimore, type=="ON-ROAD")
baltimore_nonroad  <- subset(NEI_baltimore, type=="NON-ROAD")

b_point_by_year    <- aggregate(baltimore_point$Emissions,     by=list(baltimore_point$year),    FUN=sum)
b_nonpoint_by_year <- aggregate(baltimore_nonpoint$Emissions,  by=list(baltimore_nonpoint$year), FUN=sum)
b_onroad_by_year   <- aggregate(baltimore_onroad$Emissions,    by=list(baltimore_onroad$year),   FUN=sum)
b_nonroad_by_year  <- aggregate(baltimore_nonroad$Emissions,   by=list(baltimore_nonroad$year),  FUN=sum)

names(b_point_by_year) <- c('Year','total.emissions')
names(b_nonpoint_by_year) <- c('Year','total.emissions')
names(b_onroad_by_year) <- c('Year','total.emissions')
names(b_nonroad_by_year) <- c('Year','total.emissions')

b_point_by_year$type <- "point"
b_nonpoint_by_year$type <- "nonpoint"
b_onroad_by_year$type <- "onroad"
b_nonroad_by_year$type <- "nonroad"

balt_em <- rbind(b_point_by_year, b_nonpoint_by_year, b_onroad_by_year, b_nonroad_by_year)


png(filename = target_file)

baltimore_plot <- qplot(Year, total.emissions, data=balt_em, facets=.~type) + geom_line()

baltimore_plot <- baltimore_plot + ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5))

print(baltimore_plot)

dev.off()
print(paste("Output file: ", target_file))
