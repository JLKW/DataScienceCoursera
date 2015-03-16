if(!file.exists(file.path(getwd(),"Project 2"))){
  unzip(file.path(getwd(),"exdata-data-NEI_data.zip"),
        exdir=file.path(getwd(),"Project 2"))
}

# read data from the 2 RDS files in the zip file
NEI <- readRDS(file.path(getwd(),"Project 2","summarySCC_PM25.rds"))
SCC <- readRDS(file.path(getwd(),"Project 2","Source_Classification_Code.rds"))

library(dplyr)
SCC_motor <- SCC[grepl(pattern = "motor",x = SCC$Short.Name,ignore.case = TRUE),]
NEI_motor <- tbl_df(NEI[NEI$SCC %in% SCC_motor$SCC,])

countyEmissions <- filter(NEI_motor,fips=="24510"|fips=="06037") %>%
                    group_by(year,fips)%>%
                    summarize(totalEmissions=sum(Emissions))

df <- data.frame(fips=c("24510","06037"),County=c("Baltimore City","Los Angeles County"))
countyEmissions <- merge(x = countyEmissions,y = df,by.x = "fips",by.y="fips")

# plot 2 line graphs, 1 each for LA county and Baltimore City
library(ggplot2)

qplot(year,totalEmissions,data=countyEmissions,
      group=County,colour=County,geom=c("point","line"),
      xlab="Year",ylab="Emissions of PM2.5 (tons)",
      main="Comparison of PM2.5 emissions (tons) for LA and Baltimore")
ggsave("plot6.png")

