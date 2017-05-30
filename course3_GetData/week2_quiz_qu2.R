library('sqldf')

csv <- 'C:/work/R/datasciencecoursera/course3_GetData/getdata_data_ss06pid.csv'
acs <- read.csv(csv)
options(gsubfn.engine = "R")
res <- sqldf("select pwgtp1 from acs where AGEP < 50")
res2 <- sqldf("select AGEP, pwgtp1 from acs where AGEP < 50")
