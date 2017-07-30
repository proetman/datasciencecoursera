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
target_file <- paste(sep='/', cwd, 'plot5.png')
title <- paste("Annual PM2.5 emissions for Motor Vehicle sources\n",
               "in Baltimore (Thousands of tons)")


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


# Question 5: How have emissions from motor vehicle sources
#             changed from 1999-2008 in Baltimore City?
#

# subset Baltimore and "ON-ROAD" data
NEI_baltimore <- subset(NEI, fips == "24510")
baltimore_onroad   <- subset(NEI_baltimore, type=="ON-ROAD")

# Aggregate the data
balt_mot_veh <- aggregate(baltimore_onroad$Emissions,
                          by=list(baltimore_onroad$year),
                          FUN=sum)
names(balt_mot_veh) <- c('Year','Total.Emissions')

# Plot the data
png(filename = target_file)

plot(balt_mot_veh,
     main=title,
     pch=19,
     ylab = "Total Emissions",
     xlim=c(1998, 2009))

lines(balt_mot_veh, lwd=2)


dev.off()
print(paste("Output file: ", target_file))
