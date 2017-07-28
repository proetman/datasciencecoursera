# Clear all previous data

rm(list=ls())


cwd <- getSrcDirectory(function(x) {x})
if ( nchar(cwd) < 1 ){
        print("ERROR: Unable to determine current working directory")
} else {
        print(paste("Working directory :", cwd))
}

data_url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
zip_file <- paste(sep='/', cwd, 'exdata%2Fdata%2FNEI_data.zip')
nei_rds <- paste(sep='/', cwd, 'summarySCC_PM25.rds')
scc_rds <- paste(sep='/', cwd, 'Source_Classification_Code.rds')

target_file <- paste(sep='/', cwd, 'plot1.png')

if (! file.exists(zip_file)) {
        print("    Downloading file....")
        download.file(data_url, destfile = zip_file, method = 'curl')
        print("    Unzipping...")
        unzip(zip_file, exdir = cwd)
}

NEI <- readRDS(nei_rds)
NEI_small <- NEI[1:100,]
SCC <- readRDS(scc_rds)

# Add lookup column for EI.Sector
NEI$EI.Sector <- SCC$EI.Sector[match(NEI$SCC, SCC$SCC)]

# Question 1
# PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
totals_by_year <- aggregate(NEI$Emissions, by=list(NEI$year), FUN=sum)
names(totals_by_year) <- c('Year','Total.Emissions')
plot(totals_by_year)

# Question 2
# totals for Baltimore

NEI_baltimore <- subset(NEI, fips == "24510")
NEI_small <- NEI_baltimore[1:100,]
total_baltimore_by_year <- aggregate(NEI_baltimore$Emissions, by=list(NEI_baltimore$year), FUN=sum)
names(total_baltimore_by_year) <- c('Year','Total.Emissions.Baltimore')
plot(total_baltimore_by_year)

# Question 3
# Totals for Baltimore by source (point, nonpoint, onroad, nonroad)
# Use ggplot

baltimore_point    <- subset(NEI_baltimore, type=="POINT")
baltimore_nonpoint <- subset(NEI_baltimore, type=="NONPOINT")
baltimore_onroad   <- subset(NEI_baltimore, type=="ON-ROAD")
baltimore_nonroad  <- subset(NEI_baltimore, type=="NON-ROAD")

b_point_by_year    <- aggregate(baltimore_point$Emissions,     by=list(baltimore_point$year),    FUN=sum)
b_nonpoint_by_year <- aggregate(baltimore_nonpoint$Emissions,  by=list(baltimore_nonpoint$year), FUN=sum)
b_onroad_by_year   <- aggregate(baltimore_onroad$Emissions,    by=list(baltimore_onroad$year),   FUN=sum)
b_nonroad_by_year  <- aggregate(baltimore_nonroad$Emissions,   by=list(baltimore_nonroad$year),  FUN=sum)

names(b_point_by_year) <- c('Year','teb')
names(b_nonpoint_by_year) <- c('Year','teb')
names(b_onroad_by_year) <- c('Year','teb')
names(b_nonroad_by_year) <- c('Year','teb')

b_point_by_year$type <- "point"
b_nonpoint_by_year$type <- "nonpoint"
b_onroad_by_year$type <- "onroad"
b_nonroad_by_year$type <- "nonroad"

b <- rbind(b_point_by_year, b_nonpoint_by_year, b_onroad_by_year, b_nonroad_by_year)

library(dplyr)
library(ggplot2)

qplot(Year, teb, data=b, facets=.~type)

# The data is ok, but the headings are messed up by the merge.

# this is not the answer as it uses base, need to use ggplot.
par(mfrow=c(1,4), mar=c(4,4,2,1))
plot(total_baltimore_point_by_year)
plot(total_baltimore_nonpoint_by_year)
plot(total_baltimore_onroad_by_year)
plot(total_baltimore_nonroad_by_year)




# Question 4


