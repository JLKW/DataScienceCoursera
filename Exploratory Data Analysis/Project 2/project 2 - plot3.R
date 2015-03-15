if(!file.exists(file.path(getwd(),"Project 2"))){
  unzip(file.path(getwd(),"exdata-data-NEI_data.zip"),
        exdir=file.path(getwd(),"Project 2"))
}

# read data from the 2 RDS files in the zip file
NEI <- readRDS(file.path(getwd(),"Project 2","summarySCC_PM25.rds"))
SCC <- readRDS(file.path(getwd(),"Project 2","Source_Classification_Code.rds"))

library(dplyr)
NEI <- tbl_df(NEI)
grpDataBySource <- filter(NEI,fips=="24510") %>%
                    group_by(type,year)%>%
                    summarize(totalEmissions=sum(Emissions))


# plot 4 line graphs (1 for each pollutant source)
library(ggplot2)

qplot(year,totalEmissions,data=grpDataBySource,
      group=type,colour=type,geom=c("point","line"),
      xlab="Year",ylab="Emissions of PM2.5 (tons)",
      main="Sources of Emissions of PM2.5 (tons) in Baltimore City")
ggsave("plot3.png")
