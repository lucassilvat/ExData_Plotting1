library(dplyr);library(tidyr);library(textclean);library(utils); library(lubridate)
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")
## ----------------------------------------

##Downloading and Cleaning DATA

##download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",destfile="data.zip",mode="wb")

##unzip("data.zip")

##Since we know the data structure, it is possible to calculate which lines should be read by the input date

date.ini <- ymd_hms("2007-02-01 00:00:00")
date.end <- ymd_hms("2007-02-02 23:59:00")

n.lines <- as.numeric(difftime(date.end,date.ini,units = "mins"))

##Since the first measurement was made on 16/12/2006, at 17:24:00, we can calculate the probable offset so we can pass to read
##as "skip"

date.first <- dmy_hm("16/12/2006 17:24")

offset.lines <- as.numeric(difftime(date.ini,date.first,units = "mins"))

##Also, we don't know if every line is present or if there is any double measurements. So, in order to avoid reading wrong chunks
##of data, offset is reduced by 30% and the number of lines that should be read increased by 30% and by
##the offset that we've reduced

n.lines <- ceiling(n.lines*1.3) + ceiling(offset.lines*0.3)
offset.lines <- ceiling(offset.lines*0.7)

##Reading the selected chunk of data that may contain what we are looking for

dataset <- read.table("household_power_consumption.txt",sep= ";",nrows = n.lines, skip = offset.lines,stringsAsFactors = FALSE)

##Reading variable names

var.names <- read.table("household_power_consumption.txt",sep= ";",nrows=1,header=TRUE)

##Merging together and dumping unnecessary variables after

names(dataset) <- names(var.names)
rm(var.names,date.first,n.lines,offset.lines)

##Merging date and time
dataset$Date <- as.character(dataset$Date)
dataset$Time <- as.character(dataset$Time)
dataset <- mutate(dataset,dt = paste(dataset$Date,dataset$Time))
dataset$dt <- dmy_hms(dataset$dt)

dataset <- tbl_df(dataset)
data.subset <- dataset %>% select(-Date,-Time) %>% filter((dt >= date.ini) & (dt<=date.end))
data.subset[,1:7]<-lapply(data.subset[,1:7],as.numeric)

##---------------------------------------------------------

##Configuring global parameters

par(mfrow=c(2,2))

##Plot 1

with(data.subset,plot(dt,Global_active_power,xlab = "", ylab="Global Active Power (kilowatts)",type="l"))

##Plot 2

with(data.subset,plot(dt,Voltage,xlab="",ylab="Voltage",sub="datetime",type="l"))

##Plot 3

##Initializing plot
with(data.subset,plot(dt,Sub_metering_1,xlab = "", ylab="Energy sub metering",type="n"))

##Adding lines
lines(data.subset$dt,data.subset$Sub_metering_1,col="black")
lines(data.subset$dt,data.subset$Sub_metering_2,col="red")
lines(data.subset$dt,data.subset$Sub_metering_3,col="blue")

##Adding legend
legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),lty = 1)

##Plot 4

with(data.subset,plot(dt,Global_reactive_power,xlab="",ylab="Voltage",sub="datetime",type="l"))

dev.copy(png,"plot4.png")
dev.off()