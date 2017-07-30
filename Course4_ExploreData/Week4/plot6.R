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
title <- paste("Comparison of PM2.5 emissions for Motor Vehicle sources\n",
               "in Baltimore and California (Thousands of tons)")

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

# subset the Baltimore and "ON-ROAD" data
NEI_baltimore <- subset(NEI, fips == "24510")
baltimore_onroad   <- subset(NEI_baltimore, type=="ON-ROAD")

# Aggregate
balt_mot_veh <- aggregate(baltimore_onroad$Emissions,
                          by=list(baltimore_onroad$year),
                          FUN=sum)
names(balt_mot_veh) <- c('Year','Total.Emissions')

# subset the California and "ON-ROAD" data
NEI_california <- subset(NEI, fips == "06037")
california_onroad   <- subset(NEI_california, type=="ON-ROAD")

# Aggregate
cal_mot_veh <- aggregate(california_onroad$Emissions,
                          by=list(california_onroad$year),
                          FUN=sum)
names(cal_mot_veh) <- c('Year','Total.Emissions')


# Now merge the data
balt_mot_veh$location <- "Baltimore"
cal_mot_veh$location <- "California"
compare_cities <- rbind(balt_mot_veh, cal_mot_veh)
library(ggplot2)

png(filename = target_file)
plot6 <- qplot(Year, Total.Emissions, data=compare_cities, facets=.~location) +
        geom_line()  + ggtitle(title)  +
        theme(plot.title = element_text(hjust = 0.5)) +
        ylab("Total Emissions")

print(plot6)
dev.off()
print(paste("Output file: ", target_file))


