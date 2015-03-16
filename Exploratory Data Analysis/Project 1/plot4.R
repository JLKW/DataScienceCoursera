if (!file.exists(file.path(getwd(),"Project 1"))){
  unzip("exdata-data-household_power_consumption.zip", exdir="./Project 1")
}

# read the data into R
data = read.table(file.path(getwd(),"Project 1","household_power_consumption.txt"),
                  header=TRUE,stringsAsFactors=FALSE,sep=";",na.strings="?")

# create another column which contains both date and time info 
data$DateAndTime <- paste(data$Date,data$Time)

# convert date-time info from character to date and time class
data$Date <- as.Date(data$Date,format="%d/%m/%Y")
data$DateAndTime <- strptime(data$DateAndTime,format="%d/%m/%Y %H:%M:%S")


#subset the data based on dates
data <- subset(data,Date>=as.Date("01/02/2007",format="%d/%m/%Y") &
                 Date<=as.Date("02/02/2007",format="%d/%m/%Y"))

# create 4 plots
png(file="plot4.png",height=480,width=480)
par(mfrow=c(2,2))

with(data,{
  # first plot
  plot(type="l",x=DateAndTime,y=Global_active_power,xlab="",ylab="Global Active Power")
  
  # second plot
  plot(type="l",x=DateAndTime,y=Voltage,xlab="datetime",ylab="Voltage")
  
  # third plot
  plot(type="l",x=DateAndTime,y=Sub_metering_1,xlab="",ylab="Energy sub metering")
  lines(type="l",x=DateAndTime,y=Sub_metering_2,xlab="",col="red")
  lines(type="l",x=DateAndTime,y=Sub_metering_3,col="blue")
  
  # add in the legend for the third plot
  legend(x="topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
         col=c("black","red","blue"),lty=c(1,1,1),bty="n")
  
  # fourth plot
  plot(type="l",x=DateAndTime,y=Global_reactive_power,xlab="datetime",
       ylab="Global_reactive_power")
  
})

dev.off()
