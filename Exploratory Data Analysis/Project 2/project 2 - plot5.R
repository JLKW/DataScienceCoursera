if(!file.exists(file.path(getwd(),"Project 2"))){
  unzip(file.path(getwd(),"exdata-data-NEI_data.zip"),
        exdir=file.path(getwd(),"Project 2"))
}

# read data from the 2 RDS files in the zip file
NEI <- readRDS(file.path(getwd(),"Project 2","summarySCC_PM25.rds"))
SCC <- readRDS(file.path(getwd(),"Project 2","Source_Classification_Code.rds"))


library(dplyr)
# find the list of SCC from motor sources
SCC_motorSources <- SCC[grepl(pattern = "motor", x = SCC$Short.Name, ignore.case = TRUE),]

# subset the data based on the SCCs of motor sources
NEI_motorSources <- tbl_df(NEI[NEI$SCC %in% SCC_motorSources$SCC,])

aggNEI_motorSources <- filter(NEI_motorSources,fips=="24510") %>%
                        group_by(year) %>%
                        summarize(totalEmissions=sum(Emissions))

library(ggplot2)

png("plot5.png")
g <- ggplot(aggNEI_motorSources,aes(factor(year),totalEmissions)) + 
  geom_bar(stat="identity") + labs(x="Year") + labs(y="") + 
  labs(title="PM2.5 Emissions (tons) from Motor Vehicle Sources in Baltimore City")
print(g)
dev.off()