
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                        Global Variables
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


# dir_proj_working <- 'C:/work/R/course4_week1_ExData_plotting/ExData_Plotting1'
# data_dir <- 'c:/temp/week1'

cwd <- getSrcDirectory(function(x) {x})
if ( nchar(cwd) < 1 ){
        print("ERROR: Unable to determine current working directory")
} else {
        print(paste("Working directory :", cwd))
}

data_url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
zip_file <- paste(sep='/', cwd, 'household_power_consumption.zip')
raw_data_file <- 'household_power_consumption.txt'
target_file <- paste(sep='/', cwd, 'plot3.png')

if (! file.exists(zip_file)) {
        print("    Downloading file....")
        download.file(data_url, destfile = zip_file, method = 'curl')
        print("    Unzipping...")
        unzip(zip_file, exdir = cwd)
}

data_file <- paste(sep="/", cwd, raw_data_file )

print("    Reading data from data file...")
home_data <- read.csv(file=data_file,
                 sep=';',
                 na.strings = "?",
                 header=TRUE)

print("    Generating plot....")
names(home_data) <- tolower(names(home_data))

# Subset, then clean up the levels.
home_data <- subset(home_data, date == "1/2/2007" | date == "2/2/2007" )
home_data$date <- as.factor(as.character(home_data$date))

# Convert the Date and time strings into a proper DateTime variable.
home_data$newdate1 <- paste(sep=" ", home_data$date, home_data$time)
home_data$datetime <- strptime(home_data$newdate1, format = "%d/%m/%Y %H:%M:%S")
home_data$newdate1 <- NULL


# Generate Plot3.png

png(filename = target_file)

with(home_data, plot(datetime,
                     sub_metering_1,
                     type="l",
                     xlab="",
                     ylab="Energy sub metering"))
legend("topright",
       lty=c(1,1),
       lwd=c(2.5,2.5),
       col=c("black", "blue", "red"),
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
with(home_data, lines(datetime, sub_metering_2, col="red"))
with(home_data, lines(datetime, sub_metering_3, col="blue"))

dev.off()
print(paste("Output file: ", target_file))


