if(!file.exists(file.path(getwd(),"Project 2"))){
  unzip(file.path(getwd(),"exdata-data-NEI_data.zip"),
        exdir=file.path(getwd(),"Project 2"))
}

# read data from the 2 RDS files in the zip file
NEI <- readRDS(file.path(getwd(),"Project 2","summarySCC_PM25.rds"))
SCC <- readRDS(file.path(getwd(),"Project 2","Source_Classification_Code.rds"))


library(dplyr)

#find the list of SCC from coal sources
matches <- grepl("coal",SCC$Short.Name,ignore.case = TRUE)
SCC_coalSources <- tbl_df(SCC[matches,])

# subset the data set based on the SCCs of coal sources
NEI_coalSources <- tbl_df(NEI[NEI$SCC %in% SCC_coalSources$SCC,])

aggNEI_coalSources <- group_by(NEI_coalSources,year) %>%
                        summarize(totalEmissions=sum(Emissions))
  
# create the plot to show how the total emissions across the US changes across the years
library(ggplot2)
qplot(year,totalEmissions,data=aggNEI_coalSources,geom=c("point","line"),
      xlab="Year",ylab="Amount of PM2.5 Emissions (tons)",
      main="Total Emissions of PM2.5 across the US (tons)")
ggsave("plot4.png")


