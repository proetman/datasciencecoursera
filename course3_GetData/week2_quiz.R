a

4/5   PASS
1. The American Community Survey distributes downloadable data about United States communities. Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here:
https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
and load the data into R. The code book, describing the variable names is here:
https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
How many properties are worth $1,000,000 or more?
164
25
2076
53

setwd('C:/work/R/datasciencecoursera/course3_GetData/week1_quiz')
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileURL, destfile = "microdata.csv", method = "curl")
list.files(".")
myData <- read.table("microdata.csv", sep=",", header = TRUE)
head(myData )
x <- split(myData, myData$VAL)
y <- x$"24"
dim(y)

method2
z <- myData[!is.na(myData$VAL),]
z2 <- z[z[,"VAL"]=="24",]
dim(z2)

2. 
Use the data you loaded from Question 1. Consider the variable FES in the code book. Which of the "tidy data" principles does this variable violate?
	• Numeric values in tidy data can not represent categories.
	• Tidy data has variable values that are internally consistent.
	• Each variable in a tidy data set has been transformed to be interpretable.   OK
	• Tidy data has one variable per column.    

FES 1     Family type and employment status 
b .N/A (GQ/vacant/not a family) 
1 .Married-couple family: Husband and wife in LF 
2 .Married-couple family: Husband in labor force, wife .not in LF 
3 .Married-couple family: Husband not in LF, .wife in LF 
4 .Married-couple family: Neither husband nor wife in .LF 
5 .Other family: Male householder, no wife present, in .LF 
6 .Other family: Male householder, no wife present, .not in LF
 7 .Other family: Female householder, no husband .present, in LF 
8 .Other family: Female householder, no husband .present, not in LF 
3. Download the Excel spreadsheet on Natural Gas Aquisition Program here:
https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx
Read rows 18-23 and columns 7-15 into R and assign the result to a variable called:
dat
What is the value of:
sum(dat$Zip*dat$Ext,na.rm=T)
(original data source: http://catalog.data.gov/dataset/natural-gas-acquisition-program)
	• 0
	• NA
	• 154339
	• 36534720

rm(list=ls())
install.packages('xlsx')
library(xlsx)
setwd('C:/work/R/datasciencecoursera/course3_GetData/week1_quiz')
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileURL, destfile = "qu3.xlsx", method = "curl")
list.files(".")

mydata <- read.xlsx("qu3.xlsx", sheetIndex=1)

myspecdata <- mydata[18:23, 7:15]
cols <- c('Zip','CuCurrent','PaCurrent','PoCurrent','Contact','Ext','Fax','email','Status')
dat$Zip <- as.numeric(as.character(dat$Zip))
dat$Ext <- as.numeric(as.character(dat$Ext))
sum(dat$Zip*dat$Ext,na.rm=T)
36534720

Zip	CuCurrent	PaCurrent	PoCurrent	Contact	Ext	Fax	email	Status
74136	0	1	0	918-491-6998	0	918-491-6659		1
30329	1	0	0	404-321-5711				1
74136	1	0	0	918-523-2516	0	918-523-2522		1
80203	0	1	0	303-864-1919	0			1
80120	1	0	0	345-098-8890	456			1



4. Read the XML data on Baltimore restaurants from here:
https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml
How many restaurants have zipcode 21231?
	• 156
	• 127
	• 17
	• 100

rm(list=ls())
install.packages('XML')
library(XML)
setwd('C:/work/R/datasciencecoursera/course3_GetData/week1_quiz')
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
download.file(fileURL, destfile = "qu4.xml", method = "curl")
list.files(".")

doc <- xmlTreeParse("qu4.xml", useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)

zips = xpathSApply(rootNode,"//zipcode",xmlValue)
newzips <- zips[zips == '21231']
summary(newzips)  --> 127
length(newzips)  --> 127



5. 
The American Community Survey distributes downloadable data about United States communities. Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here:
https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv
using the fread() command load the data into an R object
DT
The following are ways to calculate the average value of the variable
pwgtp15
broken down by sex. Using the data.table package, which will deliver the fastest user time?

16.65 16.67   system.time(for(i in 1:1000) {mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)})
0.39 0.40 system.time(for(i in 1:1000) {tapply(DT$pwgtp15,DT$SEX,mean)})   
0.01 0.03 system.time(for(i in 1:1000) {mean(DT$pwgtp15,by=DT$SEX)})    
0.34 0.33 system.time(for(i in 1:1000) {sapply(split(DT$pwgtp15,DT$SEX),mean)})
0.58 0.57 system.time(for(i in 1:1000) {DT[,mean(pwgtp15),by=SEX]})
system.time(for(i in 1:1000) {rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]})

rm(list=ls())
install.packages('data.table')
library(data.table)
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
fread(fileURL)


check results…. got this one wrong.


