library(dplyr)
library(reshape2)
library(ggplot2)

stormDataRaw <- read.csv(bzfile(file.path(getwd(),"Reproducible research project 2",
                                          "repdata-data-StormData.csv.bz2")),
                          stringsAsFactors=FALSE)

stormDataRaw$EVTYPE <- as.factor(stormDataRaw$EVTYPE)
stormData <- tbl_df(stormDataRaw)

# Take care of PROPDMGEXP and CROPDMGEXP unit scalers (eg. k -> 1,000)

unique(stormData$PROPDMGEXP)
stormData[stormData$PROPDMGEXP=="H"|stormData$PROPDMGEXP=="h","PROPDMGEXP"] <- 100
stormData[stormData$PROPDMGEXP=="K","PROPDMGEXP"] <- 1000
stormData[stormData$PROPDMGEXP=="M"|stormData$PROPDMGEXP=="m","PROPDMGEXP"] <- 1000000
stormData[stormData$PROPDMGEXP=="B","PROPDMGEXP"] <- 1000000000
stormData[stormData$PROPDMGEXP=="0","PROPDMGEXP"] <- 10
stormData[stormData$PROPDMGEXP=="1","PROPDMGEXP"] <- 10
stormData[stormData$PROPDMGEXP=="2","PROPDMGEXP"] <- 10
stormData[stormData$PROPDMGEXP=="3","PROPDMGEXP"] <- 10
stormData[stormData$PROPDMGEXP=="4","PROPDMGEXP"] <- 10
stormData[stormData$PROPDMGEXP=="5","PROPDMGEXP"] <- 10
stormData[stormData$PROPDMGEXP=="6","PROPDMGEXP"] <- 10
stormData[stormData$PROPDMGEXP=="7","PROPDMGEXP"] <- 10
stormData[stormData$PROPDMGEXP=="8","PROPDMGEXP"] <- 10
stormData[stormData$PROPDMGEXP=="","PROPDMGEXP"] <- 1
stormData[stormData$PROPDMGEXP=="+","PROPDMGEXP"] <- 1
stormData[stormData$PROPDMGEXP=="-","PROPDMGEXP"] <- 1
stormData[stormData$PROPDMGEXP=="?","PROPDMGEXP"] <- 1

stormData$PROPDMGEXP <- as.numeric(stormData$PROPDMGEXP)
stormData <- mutate(stormData,property_damage=PROPDMG*PROPDMGEXP)

unique(stormData$CROPDMGEXP)
stormData[stormData$CROPDMGEXP=="K"|stormData$CROPDMGEXP=="k","CROPDMGEXP"] <- 1000
stormData[stormData$CROPDMGEXP=="M"|stormData$CROPDMGEXP=="m","CROPDMGEXP"] <- 1000000
stormData[stormData$CROPDMGEXP=="B","CROPDMGEXP"] <- 1000000000
stormData[stormData$CROPDMGEXP=="0","CROPDMGEXP"] <- 10
stormData[stormData$CROPDMGEXP=="2","CROPDMGEXP"] <- 10
stormData[stormData$CROPDMGEXP=="","CROPDMGEXP"] <- 1
stormData[stormData$CROPDMGEXP=="?","CROPDMGEXP"] <- 1

stormData$CROPDMGEXP <- as.numeric(stormData$CROPDMGEXP)
stormData <- mutate(stormData,crop_damage=CROPDMG*CROPDMGEXP)

# which types of events are most harmful with respect to population health?

healthDmgTable <- group_by(stormData,EVTYPE) %>%
                  summarise(fatalities=sum(FATALITIES),injuries=sum(INJURIES))%>%
                  arrange(desc(fatalities+injuries))%>%
                  mutate(Total=fatalities+injuries)

# convert the data to long format so that we can plot the stacked bar chart
healthDmgTableLong <- melt(healthDmgTable[1:10,1:3],id.vars="EVTYPE")

healthDamage <- ggplot(data=healthDmgTableLong,
                                 aes(x=reorder(EVTYPE,-value),y=value,fill=variable,order=-value))+
              geom_bar(stat="identity",position="stack",colour="black")+
              labs(x="Weather Event",y="Fatalities/Injuries Count",
                   title="Harm to population health caused by 
                          weather events in the US between 1950 and 2011")+
              theme(axis.text.x=element_text(angle=45,hjust=1),
                    plot.title=element_text(face="bold",vjust=1))+
              scale_y_continuous(breaks=seq(0,100000,by=5000))
              

# which types of events have the greatest economic consequences?


economicDmgTable <- group_by(stormData,EVTYPE)%>%
                    summarise(TotalPropertyDamage=sum(property_damage)/10^9,
                              TotalCropDamage=sum(crop_damage)/10^9)%>%
                    arrange(desc(TotalPropertyDamage+TotalCropDamage))%>%
                    mutate(TotalDamage=TotalPropertyDamage+TotalCropDamage)

# convert the data table to long format so that we can plot the stacked bar chart
economicDmgTableLong <- melt(economicDmgTable[1:10,1:3],id.vars="EVTYPE")

# stacked bar chart of property + crop damage
economicDmg <- ggplot(data=economicDmgTableLong,
                      aes(x=reorder(EVTYPE,-value),y=value,fill=variable))+
                geom_bar(stat="identity",position="stack",colour="black")+
                labs(x="Weather Event",y="Economic Damage (US$ billions)",
                title="Economic damage caused by weather events in the US 
                        between 1950 and 2011")+
                theme(axis.text.x=element_text(angle=45,hjust=1),
                      plot.title=element_text(face="bold"))+
                scale_fill_discrete(guide=guide_legend(reverse=TRUE))+
                scale_y_continuous(breaks=seq(0,170,by=10))

