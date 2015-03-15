if(!file.exists(file.path(getwd(),"Project 2"))){
  unzip(file.path(getwd(),"exdata-data-NEI_data.zip"),
        exdir=file.path(getwd(),"Project 2"))
}

# read data from the 2 RDS files in the zip file
NEI <- readRDS(file.path(getwd(),"Project 2","summarySCC_PM25.rds"))
SCC <- readRDS(file.path(getwd(),"Project 2","Source_Classification_Code.rds"))
               
# tabulate the total emission from all sources for each of the years 1999, 2002, 2005, 2008
library(dplyr)
NEI <- tbl_df(NEI)
yearlyEmissions <- group_by(NEI,year)%>%
                    summarize(totalEmissions=sum(Emissions))

# create the bar plot of total emissions for each year
png(file="plot1.png")
barplot(height=yearlyEmissions$totalEmissions,names.arg=yearlyEmissions$year,
        main="Total emissions for each year (tons)")
dev.off()