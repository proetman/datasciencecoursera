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
target_file <- paste(sep='/', cwd, 'plot6.png')

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

png(filename = target_file)
p6 <- qplot(Year, Emission.Difference, data=compare_cities_diff, facets=.~location)
print(p6)
dev.off()
print(paste("Output file: ", target_file))

