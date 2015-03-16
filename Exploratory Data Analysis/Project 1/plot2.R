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

#plot the series onto a png file
png(file="plot2.png",height=480,width=480)

plot(type="l",x=data$DateAndTime,y=data$Global_active_power,
     ylab="Global Active Power (kilowatts)",xlab="")

dev.off()