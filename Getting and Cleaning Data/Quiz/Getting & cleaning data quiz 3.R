
# Question 1 --------------------------------------------------------------

if(FALSE){
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url,"./question1.csv")
data <- tbl_df(read.csv("./question1.csv"))
logical1 <- data$ACR == 3
logical2 <- data$AGS == 6
result <- mapply(function(x,y){x&y},logical1,logical2)
print(which(result))


# Question 2 --------------------------------------------------------------
library(jpeg)
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(url2,"./image.jpeg",mode="wb")
data2 <- readJPEG("./image.jpeg",native=TRUE)
print(quantile(data2,probs=c(0.3,0.8)))
}


# Question 3 --------------------------------------------------------------
library(dplyr); library(data.table)

if(!file.exists("./GDPdata.csv"))
{
  url3a <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
  download.file(url3a,"./GDPdata.csv")
}

#read GDPdata
GDPdata <- tbl_df(read.csv("./GDPdata.csv",stringsAsFactors=FALSE,skip=4))
GDPdata <- select(GDPdata,c(1,2,4,5))
colnames(GDPdata)<- c("CountryCode","Ranking","Economy","GDP")
GDPdata <- filter(GDPdata,Ranking!="")

if(!file.exists("./edudata.csv"))
{
  url3b <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
  download.file(url3b,"./edudata.csv")
}

edudata <- tbl_df(read.csv("./edudata.csv",stringsAsFactors=FALSE))

#count of number of matches of countries
matches <- intersect(GDPdata$CountryCode,edudata$CountryCode)
matches_count <- length(matches)

#merge the 2 data frames together
mergedata <- tbl_df(merge(GDPdata,edudata,by.x="CountryCode",by.y="CountryCode"))
mergedata$Ranking <- as.numeric(mergedata$Ranking)
mergedata <- arrange(mergedata,desc(Ranking))

#solution:
print(matches_count)
print(mergedata[13,"Economy"])


# Question 4 --------------------------------------------------------------

#Requires mergedata from qn 3
orderedata <- group_by(mergedata,Income.Group)
#Solution:
print(summarize(orderedata,mean(Ranking)))


# Question 5 --------------------------------------------------------------
orderedata$GDP <- as.numeric(gsub(",","",orderedata$GDP))
top_quantile <- quantile(orderedata$GDP,probs=0.8)
result <- filter(orderedata,GDP>=top_quantile & Income.Group=="Lower middle income")
print (result)
#solution
print (nrow(result))
