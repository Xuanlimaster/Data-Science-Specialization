#1. How many properties are worth $1,000,000 or more?
data <- read.csv("getdata_data_ss06hid.csv")
sum(data[,"VAL"]>=24,na.rm = TRUE)

#2. Use the data you loaded from Question 1. Consider the variable FES in the 
### code book. Which of the "tidy data" principles does this variable violate?

# Answer: Tidy data has one variable per column.

#3.
library(xlsx)
dat <- read.xlsx("getdata_data_DATA.gov_NGAP.xlsx", sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15)
sum(dat$Zip*dat$Ext,na.rm=T)

#4. How many restaurants have zip code 21231?
library(XML)
datXML <- xmlTreeParse("getdata_data_restaurants.xml", useInternalNodes = TRUE)
rootNode <- xmlRoot(datXML)
sum(xpathSApply(rootNode, "//zipcode",xmlValue)==21231)

#5. 
library(data.table)
DT <- fread("getdata_data_ss06pid.csv")
system.time(DT[,mean(pwgtp15),by=SEX])
