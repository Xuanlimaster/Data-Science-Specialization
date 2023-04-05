#Q1. 
fileurl1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileurl1, destfile = "./data1",method = "curl")
data1 <- read.csv("data1")
agricultureLogical <- data1$ACR == 3 & data1$AGS == 6
which(agricultureLogical) 

#Q2.
library(jpeg)
Q2 <- readJPEG("getdata_jeff.jpg", native = TRUE)
quantile(Q2,probs = c(0.3, 0.8))

#Q3.
library(dplyr)
library(plyr)
library(data.table)
fileurl2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
fileurl3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
data2 <- read.csv(fileurl2)
data3 <- read.csv(fileurl3)
Q3GDP <- fread(fileurl2, skip = 5, nrows = 190, select = c(1,2,4,5), col.names = c("CountryCode", "Rank", "CountryName", "GDP"))
Q3Merge <- merge(Q3GDP,data3, by = "CountryCode")
Q3Merge <- arrange(Q3Merge, desc(Rank))
Q3Merge$CountryName[13]

#Q4.
rank <- filter(Q3Merge, Income.Group == "High income: nonOECD" | Income.Group == "High income: OECD")
rankk <- tapply(rank$Rank, rank$Income.Group, mean)
ddply(rank, .(Income.Group), summarize, Average = mean(Rank)) #Another method to get the result
rankk

#Q5.
Q3Merge$Rankgroup <- cut(Q3Merge$Rank, breaks = 5)
tb <- table(Q3Merge$Rankgroup,Q3Merge$Income.Group)
tb[1,"Lower middle income"]




