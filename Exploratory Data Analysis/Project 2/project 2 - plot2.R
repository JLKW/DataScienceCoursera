if(!file.exists(file.path(getwd(),"Project 2"))){
  unzip(file.path(getwd(),"exdata-data-NEI_data.zip"),
        exdir=file.path(getwd(),"Project 2"))
}

# read data from the 2 RDS files in the zip file
NEI <- readRDS(file.path(getwd(),"Project 2","summarySCC_PM25.rds"))
SCC <- readRDS(file.path(getwd(),"Project 2","Source_Classification_Code.rds"))

# tabulate the annual emissions of Baltimore City  
library(dplyr)
NEI <- tbl_df(NEI)
BaltimoreEmissions <- filter(NEI,fips=="24510") %>%
                        group_by(year) %>%
                        summarize(totalemissions=sum(Emissions))


# plot the emissions per year in a bar graph 
png(file="plot2.png")
barplot(height=BaltimoreEmissions$totalemissions,
        names.arg = BaltimoreEmissions$year,col=c("red","blue","green","purple"),
        main="Total Emissions from PM2.5 in Baltimore City")
dev.off()