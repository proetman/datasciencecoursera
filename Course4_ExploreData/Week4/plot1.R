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
target_file <- paste(sep='/', cwd, 'plot1.png')
title <- "Annual PM2.5 emissions for all sources \n(Thousands of tons)"

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


# Question 1 : Have tot emissions of PM2.5 decreased from all sources
#              for each of  the years 1999, 2002, 2005, and 2008.
#              Use Base plotting system.

# Aggregate the data
totals_by_year <- aggregate(NEI$Emissions, by=list(NEI$year), FUN=sum)

# Set column headings
names(totals_by_year) <- c('Year','Total.Emissions')
totals_by_year$Total.Emissions <- totals_by_year$Total.Emissions / 1000

# Plot the data
png(filename = target_file)

plot(totals_by_year,
     pch=19,
     main=title,
     ylab = "Total Emissions",
     xlim=c(1998, 2009))

lines(totals_by_year, lwd=2)

dev.off()
print(paste("Output file: ", target_file))
