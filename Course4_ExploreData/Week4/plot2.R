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
target_file <- paste(sep='/', cwd, 'plot2.png')

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

# Question 2 : Have tot emissions of PM2.5 decreased from all sources
#              for each of  the years 1999, 2002, 2005, and 2008
#              in Baltimore city, Maryland.
#              Use Base plotting system.

NEI_baltimore <- subset(NEI, fips == "24510")
total_baltimore_by_year <- aggregate(NEI_baltimore$Emissions, by=list(NEI_baltimore$year), FUN=sum)
names(total_baltimore_by_year) <- c('Year','Total.Emissions.Baltimore')

png(filename = target_file)

plot(total_baltimore_by_year,
     pch=19,
     main="Annual PM2.5 emissions for Baltimore \nfor all sources \n(Thousands of tons)",
     xlim=c(1998, 2009))

lines(total_baltimore_by_year, lwd=2)


dev.off()
print(paste("Output file: ", target_file))
