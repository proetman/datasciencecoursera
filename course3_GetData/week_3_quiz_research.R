# result 5 out of 5!

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
        data <- subset(data, select = c('ACR','AGS'))
        print(head(data))
        # data <- data[complete.cases(data),]
        print(head(data))
        agricultureLogical = data$ACR=='3' & data$AGS == '6'

        print('---------- head ------------')
        print(head(which(agricultureLogical),3))
        print('---------- head ------------')

        file.remove(csv_file)
        file.remove(pdf_file)
        return(TRUE)

        # answer 125 238 262

}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Question 2
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# question 2

qu2 <- function() {
        library('jpeg')
        source_file <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg'
        jpeg_file <- paste(sep="/", tmp_dir, 'qu2.jpg')
        print(jpeg_file)
        if(! file.exists(jpeg_file)) {
                download.file(source_file,
                              destfile=jpeg_file,
                              mode="wb",
                              method="curl")
        }

        jeff_jpeg = readJPEG(jpeg_file, native=TRUE)
        # print(summary(jeff_jpeg))

        print(quantile(jeff_jpeg, probs=c(0.3, 0.8)))


        file.remove(jpeg_file)

        # answer -15259150 -10575416
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Question 3
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# question 3
#
qu3 <- function() {

        source_file <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'
        gdp_file <- paste(sep="/", tmp_dir, 'qu3_GDP.csv')
        print(gdp_file)
        if(! file.exists(gdp_file)) {
                download.file(source_file, destfile=gdp_file, method="curl")
        }

        source_file <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv'
        edu_file <- paste(sep="/", tmp_dir, 'qu3_EDU.csv')
        print(edu_file)
        if(! file.exists(edu_file)) {
                download.file(source_file, destfile=edu_file, method="curl")
        }

        gdp_data <- read.csv(gdp_file, skip=3)
        gdp_data <- subset(gdp_data, select = c('X','Ranking','Economy','US.dollars.'))
        names(gdp_data) <- c('CountryCode','Ranking','Economy','US.dollars.')

        gdp_data <- gdp_data[(!is.na(gdp_data$CountryCode) &
                             gdp_data$CountryCode != '' &
                             !is.na(gdp_data$Ranking) &
                             gdp_data$Ranking != '' ), ]
        # print(summary(gdp_data))
        # print(head(gdp_data))
        # print(tail(gdp_data))

        edu_data <- read.csv(edu_file)
        edu_data <- edu_data[(edu_data$Income.Group != ''), ]

        edu_cc <- edu_data$CountryCode
        gdp_cc <- gdp_data$CountryCode

        res2 <- gdp_cc[!gdp_cc %in% edu_cc]
        print('--- RES2 ---')
        print(head(res2))
        print('--- RES2 ---')

        res3 <- edu_cc[!edu_cc %in% gdp_cc]
        print('--- RES3 ---')
        print(head(res3))
        print('--- RES3 ---')


        print(head(edu_cc))
        print(head(gdp_cc))
        common_cc <- intersect(edu_cc, gdp_cc)
        common_cc <- intersect(gdp_cc, edu_cc)
        diff_cc <- setdiff(edu_cc, gdp_cc)
        diff_cc <- setdiff(gdp_cc, edu_cc)

        print(length(edu_cc))
        print('--- common ---')
        print(common_cc)
        print(length(common_cc))
        print(names(common_cc))

        # print(names(gdp_data))
        # print(names(edu_data))
        print('start merge')
        m <- merge(gdp_data, edu_data, by = 'CountryCode')

        m$high_income_rank <- as.factor(m$Ranking)
        m$high_income_rank_n <- as.numeric(as.character(m$high_income_rank))


        # print(names(m))
        print('result')

        print('end merge')
        # print('--- diff ---')
        # print(diff_cc)
        # print(length(diff_cc))
        # print('--- end ---')
        # print(tail(edu_data$CountryCode))


        file.remove(gdp_file)
        file.remove(edu_file)
        return(TRUE)

        # result is 189 and St Kitts and Nevis
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Question 4
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# question 4
#
qu4 <- function() {

        library(dplyr)

        source_file <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'
        gdp_file <- paste(sep="/", tmp_dir, 'qu3_GDP.csv')
        print(gdp_file)
        if(! file.exists(gdp_file)) {
                download.file(source_file, destfile=gdp_file, method="curl")
        }

        source_file <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv'
        edu_file <- paste(sep="/", tmp_dir, 'qu3_EDU.csv')
        print(edu_file)
        if(! file.exists(edu_file)) {
                download.file(source_file, destfile=edu_file, method="curl")
        }

        gdp_data <- read.csv(gdp_file, skip=3)
        gdp_data <- subset(gdp_data, select = c('X','Ranking','Economy','US.dollars.'))
        names(gdp_data) <- c('CountryCode','Ranking','Economy','US.dollars.')

        gdp_data <- gdp_data[(!is.na(gdp_data$CountryCode) &
                                      gdp_data$CountryCode != '' &
                                      !is.na(gdp_data$Ranking) &
                                      gdp_data$Ranking != '' ), ]
        # print(summary(gdp_data))
        # print(head(gdp_data))
        # print(tail(gdp_data))

        edu_data <- read.csv(edu_file)
        edu_data <- edu_data[(edu_data$Income.Group != ''), ]

        # print(names(edu_data))
        # print(edu_data[grep('group aggregate', edu_data$Special.Notes), ]$Special.Notes)
        print(edu_data[edu_data$Income.Group == '', ]$Special.Notes)

        print(dim(gdp_data))
        print(dim(edu_data))

        merged_data <- merge(edu_data, gdp_data,
                             by.x="CountryCode",
                             by.y="CountryCode",
                             all=FALSE)

        print(dim(merged_data))

        high_income <- merged_data[merged_data$Income.Group == 'High income: OECD', ]

        print(' mean high income rank')
        high_income_rank <- as.factor(high_income$Ranking)
        high_income_rank_n <- as.numeric(as.character(high_income_rank))
        print(mean(high_income_rank_n))

        high_nonoecd_income <- merged_data[merged_data$Income.Group == 'High income: nonOECD', ]

        print(' mean high nonOECD income rank')
        high_nonoecd_income_rank <- as.factor(high_nonoecd_income$Ranking)
        high_nonoecd_income_rank_n <- as.numeric(as.character(high_nonoecd_income_rank))
        print(mean(high_nonoecd_income_rank_n))

        #  file.remove(gdp_file)
        # file.remove(edu_file)
        return(TRUE)
        # answer " mean high income rank" 32.96667
        #         " mean high nonOECD income rank"  91.91304
}



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Question 5
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# question 5
#
qu5 <- function() {

        library(dplyr)
        library(Hmisc)


        source_file <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'
        gdp_file <- paste(sep="/", tmp_dir, 'qu3_GDP.csv')
        print(gdp_file)
        if(! file.exists(gdp_file)) {
                download.file(source_file, destfile=gdp_file, method="curl")
        }

        source_file <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv'
        edu_file <- paste(sep="/", tmp_dir, 'qu3_EDU.csv')
        print(edu_file)
        if(! file.exists(edu_file)) {
                download.file(source_file, destfile=edu_file, method="curl")
        }

        print('1')
        gdp_data <- read.csv(gdp_file, skip=3)
        gdp_data <- subset(gdp_data, select = c('X','Ranking','Economy','US.dollars.'))
        names(gdp_data) <- c('CountryCode','Ranking','Economy','US.dollars.')

        gdp_data <- gdp_data[(!is.na(gdp_data$CountryCode) &
                                      gdp_data$CountryCode != '' &
                                      !is.na(gdp_data$Ranking) &
                                      gdp_data$Ranking != '' ), ]

        edu_data <- read.csv(edu_file)
        edu_data <- edu_data[(edu_data$Income.Group != ''), ]

        print('---special notes---')
        print(edu_data[edu_data$Income.Group == '', ]$Special.Notes)

        #print(dim(gdp_data))
        # print(dim(edu_data))

        mdata <- merge(edu_data, gdp_data,
                             by.x="CountryCode",
                             by.y="CountryCode",
                             all=FALSE)

        # print(mdata[, c('Ranking',"Income.Group") ])

        mdata$rank <- as.factor(mdata$Ranking)
        mdata$rank_n <- sort(as.numeric(as.character(mdata$rank)))

        print(mdata[, c('rank',"Income.Group") ])
        # print(arrange(m, desc(high_income_rank_n))[13, c('Ranking','Economy')])
        s <- arrange(mdata, desc(rank_n))
        # print(s[, c('rank_n',"Income.Group") ])
        print('-- arrange done --- ')
        # print(length(rank_n))
        #
        s$RGroups <- cut2(as.numeric(as.character(s$rank)), g=5)
        # print('rgroups')
        print(s$RGroups)
        # print('income')
        # print(names(mdata))
        print(s$"Income.Group")
        print("Table")
        print(table(s$RGroups, s$"Income.Group"))
        print('the end')
        # breaks <- quantile(mdata$rank_n, probs=c(0, .2, .4, .6, .8, 1), na.rm=TRUE)
        # print(breaks)
        # mdata$qGroup <- cut(mdata$rank_n, breaks=breaks)
        # print(mdata[mdata$Income.Group == "Lower middle income", by=c("Income.Group","qGroup")])
        # 20%   40%   60%   80%
        # 38.6  76.2 113.8 152.4

        # mdata1 <- mdata[mdata$Ranking < 38.6, ]
        # print(length(mdata1))

        return(TRUE)

        # Answer
        #
        # 5
        #
        #                  High income: nonOECD High income: OECD Low income Lower middle income Upper middle income
        # [  1, 39)  0                    4                18          0                   5                  11
        # [ 39, 77)  0                    5                10          1                  13                   9
        # [ 77,115)  0                    8                 1          9                  12                   8
        # [115,154)  0                    5                 1         16                   8                   8
        # [154,190]  0                    1                 0         11                  16                   9
        # [
}
