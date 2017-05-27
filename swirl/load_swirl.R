install.packages("swirl")
packageVersion("swirl")                      # verify greater than 2.2
install_from_swirl("R Programming")          # install R Programming from Swirl
swirl()

To load:
rm(list=ls())
library(swirl)
swirl()

install_from_swirl("Getting and Cleaning Data")



# Note: to exit
#     press escape
#     type  bye()
#
# other notes:
#   skip()   to skip current question
#   play()   play in R on your own
#   nxt()    return back to swirl

#   main()   return to main menu
#   info()   display these options

#   help.start()    loads help as either web page or within R studio

question 18
unzip("quiz1_data.zip")
data <- read.csv("hw1_data.csv", header=TRUE)
data[152:153,]
data[47,]$Ozone   == data[47,1]

mean(data$Ozone[!is.na(data[1])])

good = complete.cases(data)
new_data <- data[good,]
keepers <- new_data$Ozone > 31 & new_data$Temp > 90
keep_data <- new_data[keepers,"Solar.R"]
res = keep_data[!is.na(keep_data)]
mean(res)


data <- read.csv("hw1_data.csv", header=TRUE)
keepers <- data$Ozone > 31 & data$Temp > 90
keep_data <- data[keepers,"Solar.R"]
res = keep_data[!is.na(keep_data)]
mean(res)



Question 19
rm(list=ls())
data <- read.csv("hw1_data.csv", header=TRUE)
mon6 <- data$Month == 6
mon6_temp <- data[mon6,"Temp"]
mean(mon6_temp) ==> 79.1



Question 20
rm(list=ls())
data <- read.csv("hw1_data.csv", header=TRUE)
mon5 <- data$Month == 5
mon5_ozone <- data[mon5,"Ozone"]
mon5_ozone_clean <- mon5_ozone[!is.na(mon5_ozone)]





