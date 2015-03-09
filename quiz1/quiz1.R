library(RCurl)
library(dplyr)
library(xlsx)
library(XML)
library(data.table)

getwd()
setwd("~/Desktop/marcus/getting_and_cleaning_data/quiz1")
￼￼￼￼￼￼￼￼￼￼￼￼￼￼￼￼
if (!file.exists("data")) {
  dir.create("data")
}

### Question 1 ---

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "./data/2Fss06hid.csv", method = "curl")
list.files("./data")

data <- read.csv("./data/2Fss06hid.csv", header = TRUE, sep = ",")

data2 <- data %>%
  select(VAL) %>%
  filter(VAL == 24)

data[which(data$VAL == 24), "VAL"]

### Question 2----

data3 <- data %>%
  select(FES)

### Question 3 ----

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl, destfile = "./data/2FDATA.gov_NGAP.xlsx", method = "curl")
list.files("./data")

colIndex <- 7:15
rowIndex <- 18:23
data <- read.xlsx("./data/2FDATA.gov_NGAP.xlsx", sheetIndex = 1,
                              colIndex = colIndex, rowIndex = rowIndex)

sum(data$Zip*data$Ext, na.rm=T) 

### Question 4 ----

fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
rootNode[[1]]
rootNode[[1]][[3]][[2]]

data <- xpathSApply(rootNode, "//zipcode", xmlValue)
data <- as.data.frame(data)

data2 <- data %>%
  filter(data == 21231)

### Question 5 ----

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv "
download.file(fileUrl, destfile = "./data/2Fss06pid.csv", method = "curl")
list.files("./data")
DT <- fread("./data/2Fss06pid.csv") 

rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2] #OUT
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
system.time(DT[,mean(pwgtp15),by=SEX])
system.time(
  {
  mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
}
  )
mean(DT$pwgtp15,by=DT$SEX) #OUT

race = 1000
Frodo = replicate(race, system.time(sapply(split(DT$pwgtp15,DT$SEX),mean)))[1, 1:1000]
Pippin = replicate(race, system.time(tapply(DT$pwgtp15,DT$SEX,mean)))[1, 1:1000]
Merry = replicate(race, system.time(DT[,mean(pwgtp15),by=SEX]))[1, 1:1000]

Frodo_av = cumsum(Frodo) / seq_along(Frodo)
Pippin_av = cumsum(Pippin) / seq_along(Pippin)
Merry_av = cumsum(Merry) / seq_along(Merry)

topY = max(Frodo_av, Pippin_av, Merry_av) #making sure the y axis is the right height
lowY = min(Frodo_av, Pippin_av, Merry_av) #making sure the y axis is the right height
plot(Frodo_av, type="l", col="#FF000099", ylim=c(lowY,topY), xlab="distance", ylab="average time")
lines(Pippin_av, col="#0000FF99")
lines(Merry_av, col="#00000099")
