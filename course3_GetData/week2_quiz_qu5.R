file_name <- 'C:/work/R/datasciencecoursera/course3_GetData/getdata_wksst8110.for'

data <- read.fwf(
        file=file_name,
        skip=4,
        widths=c(10,9,4, 9,4, 9,4, 9,4 ))
print(head(data$V4))
print(tail(data$V4))

# verify no NA values
sapply(data, function(x) sum(is.na(x)))
myv4 <- as.vector(data$V4)
result <- sum(myv4)
print(result)
