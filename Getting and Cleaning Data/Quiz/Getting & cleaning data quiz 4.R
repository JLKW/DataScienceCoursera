
# Question 1 --------------------------------------------------------------
if(FALSE){
if (!file.exists("./quiz4")){dir.create("./quiz4")}

url1<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url1,"./quiz4/hdata.csv")
hdata <- read.csv("./quiz4/hdata.csv")
splitnames <- strsplit(names(hdata),"wgtp")
print(c("Qn 1 solution: ",splitnames[[123]]))



# Question 2 --------------------------------------------------------------
library(dplyr)
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url2,"./quiz4/GDPdata.csv")
GDPdata <- tbl_df(read.csv("./quiz4/GDPdata.csv",stringsAsFactors=FALSE,skip=4))
GDPdata <- select(GDPdata,c(1,2,4,5))[1:190,]
colnames(GDPdata)<-c("CountryCode","Rank","Country","GDP")
GDPdata$GDP <- gsub(",","",GDPdata$GDP)
GDPdata$GDP <- as.numeric(GDPdata$GDP)
#solution:
print(c("Qn 2 solution: ", mean(GDPdata$GDP[!is.na(GDPdata$GDP)])))


# Question 3 --------------------------------------------------------------

countryNames <- GDPdata$Country
#solution:
matches <- grep("^United",countryNames,value=TRUE)
print(c("Qn 3 solution: ",matches))


# Question 4 --------------------------------------------------------------

url3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url3,"./quiz4/edudata.csv")
edudata <- read.csv("./quiz4/edudata.csv",stringsAsFactors=FALSE)
mergedata <- tbl_df(merge(GDPdata,edudata,by.x="CountryCode",by.y="CountryCode"))
colnames(edudata) <- make.names(names(edudata))
FYE <- grep("Fiscal year end: June",edudata$Special.Notes,value=TRUE) #fiscal year ends
print(c("Qn 4 solution", length(FYE)))
}

# Question 5 --------------------------------------------------------------

library(quantmod);library(lubridate);library(dplyr)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 
datatwentytwelve <- grep("2012",sampleTimes,value=TRUE)


sampleDays <- cbind(wday(datatwentytwelve,label=TRUE),rep(1,length(datatwentytwelve)))
sampleDays <- tbl_df(data.frame(sampleDays))
colnames(sampleDays)<-c("Day","Count")
summary <- group_by(sampleDays,Day)%>%
            summarize(sum(Count))

print(c("Qn 5 solution is: ",length(datatwentytwelve),summary[summary$Day==2,2]))


