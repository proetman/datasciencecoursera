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




# Question 4: Across the United States, how have emissions from coal
#             combustion-related sources changed from 1999-2008?

# Add lookup column for EI.Sector
NEI$EI.Sector <- SCC$EI.Sector[match(NEI$SCC, SCC$SCC)]

# pull out all coal related sources
coal <- subset(NEI, grepl("coal", EI.Sector, ignore.case = TRUE))

# Recalculate the factors
coal$EI.Sector <- factor(coal$EI.Sector)

coal_by_year <- aggregate(coal$Emissions, by=list(coal$year),  FUN=sum)

names(coal_by_year) <- c('Year','Total.Emissions')
plot(coal_by_year)

# Question 5: How have emissions from motor vehicle sources
#             changed from 1999-2008 in Baltimore City?
#
NEI_baltimore <- subset(NEI, fips == "24510")
baltimore_onroad   <- subset(NEI_baltimore, type=="ON-ROAD")
balt_mot_veh <- aggregate(baltimore_onroad$Emissions,
                          by=list(baltimore_onroad$year),
                          FUN=sum)
names(balt_mot_veh) <- c('Year','Total.Emissions')

plot(balt_mot_veh)

# Question 6: compare Baltimore to California
#             Which city has seen greater changes over time in motor vehicle emissions?
#
NEI_baltimore <- subset(NEI, fips == "24510")
baltimore_onroad   <- subset(NEI_baltimore, type=="ON-ROAD")
balt_mot_veh <- aggregate(baltimore_onroad$Emissions,
                          by=list(baltimore_onroad$year),
                          FUN=sum)
names(balt_mot_veh) <- c('Year','Total.Emissions')

NEI_california <- subset(NEI, fips == "06037")
california_onroad   <- subset(NEI_california, type=="ON-ROAD")
cal_mot_veh <- aggregate(california_onroad$Emissions,
                          by=list(california_onroad$year),
                          FUN=sum)
names(cal_mot_veh) <- c('Year','Total.Emissions')

# Now merge the data

balt_mot_veh$location <- "Baltimore"
cal_mot_veh$location <- "California"
compare_cities <- rbind(balt_mot_veh, cal_mot_veh)
library(ggplot2)

qplot(Year, Total.Emissions, data=compare_cities, facets=.~location)

cal_diff <- data.frame(tail(cal_mot_veh$Total.Emissions, -1) - head(cal_mot_veh$Total.Emissions, -1))
balt_diff <- data.frame(tail(balt_mot_veh$Total.Emissions, -1) - head(balt_mot_veh$Total.Emissions, -1))

names(cal_diff) <- "Emission.Difference"
names(balt_diff) <- "Emission.Difference"

cal_diff$location <- "California"
balt_diff$location <- "Baltimore"

cal_diff$Year <- tail(cal_mot_veh$Year, -1)
balt_diff$Year <- tail(balt_mot_veh$Year, -1)

compare_cities_diff <- rbind(cal_diff,balt_diff )

qplot(Year, Emission.Difference, data=compare_cities_diff, facets=.~location)


