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
target_file <- paste(sep='/', cwd, 'plot4.png')

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


png(filename = target_file)

plot(coal_by_year)
dev.off()
print(paste("Output file: ", target_file))
