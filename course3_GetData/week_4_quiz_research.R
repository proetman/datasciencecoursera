# result 5 out of 5, YAY!
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                        Global Variables
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

dir_proj_working <- 'C:/work/R/week 3 research'
tmp_dir <- 'c:/temp'

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Set Default Working Directory
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Change directory to the global working directory defined for this project.

dir_set_default_working <- function(){
        # Change to the default working directory for
        # this weeks project/assignment.
        #
        setwd(dir_proj_working)
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Question 1
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# question 1

qu1 <- function() {

        source_file <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv'
        code_book <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf'

        csv_file <- paste(sep="/", tmp_dir, 'qu1.csv')
        if(! file.exists(csv_file)) {
                download.file(source_file, destfile=csv_file, method="curl")
        }

        pdf_file <- paste(sep="/", tmp_dir, 'qu1.pdf')
        if(! file.exists(pdf_file)) {
                download.file(code_book, destfile=pdf_file, method="curl")
        }

        data <- read.csv(csv_file)
        name_vector <- names(data)
        split_names <- strsplit(name_vector, "wgtp")
        print(split_names[[123]])

        # "" "15"

        file.remove(csv_file)
        file.remove(pdf_file)
        return(TRUE)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Question 2
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# question 2

qu2 <- function() {

        source_file <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'

        csv_file <- paste(sep="/", tmp_dir, 'qu2.csv')
        if(! file.exists(csv_file)) {
                download.file(source_file, destfile=csv_file, method="curl")
        }

        # data <- read.csv(csv_file, skip=4)
        gdp_data <- read.csv(csv_file, skip=3)
        gdp_data <- subset(gdp_data, select = c('X','Ranking','Economy','US.dollars.'))
        names(gdp_data) <- c('CountryCode','Ranking','Economy','USdollars')
        gdp_data <- gdp_data[(!is.na(gdp_data$CountryCode) &
                                      gdp_data$CountryCode != '' &
                                      !is.na(gdp_data$Ranking) &
                                      gdp_data$Ranking != '' ), ]


        gdp_dollars <- gsub(',', '', gdp_data$USdollars)
        print(head(gdp_dollars))
        gdp_dollars2 <- as.factor(gdp_dollars)
        gdp_dollars_n <- as.numeric(as.character(gdp_dollars2))
        head(gdp_dollars_n)
        print(mean(gdp_dollars_n))
        file.remove(csv_file)
        # answer  377652.4
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Question 3
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# question 3

qu3 <- function() {

        source_file <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'

        csv_file <- paste(sep="/", tmp_dir, 'qu2.csv')
        if(! file.exists(csv_file)) {
                download.file(source_file, destfile=csv_file, method="curl")
        }

        # data <- read.csv(csv_file, skip=4)
        gdp_data <- read.csv(csv_file, skip=3)
        gdp_data <- subset(gdp_data, select = c('X','Ranking','Economy','US.dollars.'))
        names(gdp_data) <- c('CountryCode','Ranking','Economy','USdollars')
        gdp_data <- gdp_data[(!is.na(gdp_data$CountryCode) &
                                      gdp_data$CountryCode != '' &
                                      !is.na(gdp_data$Ranking) &
                                      gdp_data$Ranking != '' ), ]

        # grep("^United",gdp_data$Economy), 3   <- [1] "United States"        "United Kingdom"       "United Arab Emirates"
        # grep("^United",gdp_data$Economy), 4
        # grep("*United",gdp_data$Economy), 2
        # grep("United$",gdp_data$Economy), 3
        print(length(grep("^United",gdp_data$Economy)))
        file.remove(csv_file)
        # answer  a
}




# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Question 4
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# question 4

qu4 <- function() {

        source_file <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'

        csv_file <- paste(sep="/", tmp_dir, 'qu4.csv')
        if(! file.exists(csv_file)) {
                download.file(source_file, destfile=csv_file, method="curl")
        }

        # data <- read.csv(csv_file, skip=4)
        gdp_data <- read.csv(csv_file, skip=3)
        gdp_data <- subset(gdp_data, select = c('X','Ranking','Economy','US.dollars.'))
        names(gdp_data) <- c('CountryCode','Ranking','Economy','USdollars')
        gdp_data <- gdp_data[(!is.na(gdp_data$CountryCode) &
                                      gdp_data$CountryCode != '' &
                                      !is.na(gdp_data$Ranking) &
                                      gdp_data$Ranking != '' ), ]

        source_file <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv'
        edu_file <- paste(sep="/", tmp_dir, 'qu4_EDU.csv')

        if(! file.exists(edu_file)) {
                download.file(source_file, destfile=edu_file, method="curl")
        }
        edu_data <- read.csv(edu_file)
        edu_data <- edu_data[(edu_data$Income.Group != ''), ]

        print('start merge')
        m <- merge(gdp_data, edu_data, by = 'CountryCode')

        grep('Fiscal year end.*June', m$Special.Notes, value=TRUE, ignore.case=TRUE)
        print(length(grep('Fiscal year end.*June', m$Special.Notes, ignore.case=TRUE)))

        # result 13

        file.remove(csv_file)
        file.remove(edu_file)
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Question 5
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# question 5

qu5 <- function() {

        library(quantmod)
        library(lubridate)

        amzn = getSymbols('AMZN', auto.assign=FALSE)
        sampleTimes=index(amzn)

        print(length(grep('^2012', sampleTimes)))
        all_results <- grep('^2012', sampleTimes, value=TRUE)
        all_results_d <- as.Date(all_results)

        length(grep('Mon', wday(all_results_d, label=TRUE)))

        # result: 250,  47
}

