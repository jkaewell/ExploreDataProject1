## Exploratory Data Analysis Project #1
## Introduction
## This assignment uses data from the UC Irvine Machine Learning Repository,
## a popular repository for machine learning datasets. In particular, we 
## will be using the “Individual household electric power consumption Data Set” 
## which I have made available on the course web site:
##  
## Dataset: Electric power consumption [20Mb]
##
## Description: Measurements of electric power consumption in one household
## with a one-minute sampling rate over a period of almost 4 years. Different 
## electrical quantities and some sub-metering values are available.
##
## The following descriptions of the 9 variables in the dataset are taken from 
## the UCI web site:
##  
## Date: Date in format dd/mm/yyyy
## Time: time in format hh:mm:ss
## Global_active_power: household global minute-averaged active power 
##     (in kilowatt)
## Global_reactive_power: household global minute-averaged reactive power 
##     (in kilowatt)
## Voltage: minute-averaged voltage (in volt)
## Global_intensity: household global minute-averaged current intensity 
##     (in ampere)
## Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). 
##      It corresponds to the kitchen, containing mainly a dishwasher, an 
##      oven and a microwave (hot plates are not electric but gas powered).
## Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy).
##      It corresponds to the laundry room, containing a washing-machine, 
##      a tumble-drier, a refrigerator and a light.
## Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). 
##      It corresponds to an electric water-heater and an air-conditioner.
## 
## We will only be using data from the dates 2007-02-01 and 2007-02-02. One 
## alternative is to read the data from just those dates rather than reading 
## in the entire dataset and subsetting to those dates.
##
## You may find it useful to convert the Date and Time variables to Date/Time 
## classes in R using the strptime() and as.Date() functions.
## Note that in this dataset missing values are coded as ?.
##
fileURL <- "./data/household_power_consumption.txt"
df <- read.csv(fileURL, sep = ";", colClasses="character")
## sub-set to just 1/2/2007 and 2/2/2007 rows
df2 <- df[(as.Date(df$Date,"%d/%m/%Y")=="2007-02-01")|(as.Date(df$Date,"%d/%m/%Y")=="2007-02-02"),]
#
##
##  date abd time equations are picky.  Watch case of the letters following
##  the % in date and time formatting.
##
df2$timestamp <- strptime(paste(df2[,1],df2[,2]),"%d/%m/%Y %H:%M:%S")

library(dplyr)

Global_active_power <- as.numeric(unlist(df2$Global_active_power))

#
timestamp <- df2$timestamp
plot2data <- data.frame(timestamp,Global_active_power)
# plot the 3 sub_metering data on one chart
##
plot3data <- data.frame(timestamp,
                        as.numeric(unlist(df2$Sub_metering_1)),
                        as.numeric(unlist(df2$Sub_metering_2)),
                        as.numeric(unlist(df2$Sub_metering_3)))
## create plot 4
## setup to plot in 4 windows
##
par(mfrow=c(2,2), mar=c(5,4,4,2)+0.1)

plot(plot2data,type="l",
     ylab="Global Active Power (kilowatts)", xlab="",cex.lab="0.75")
plot4bdata <- data.frame(timestamp,as.numeric(unlist(df2$Voltage)))
plot(plot4bdata[,1],plot4bdata[,2], type="l",
     ylab="Voltage",xlab="datetime",cex.lab="0.75")

plot(plot3data[,1],plot3data[,2],type="l",ylab="Energy sub metering",
     xlab="", cex.lab="0.75")
lines(plot3data[,1],plot3data[,3],type="l", col="red")
lines(plot3data[,1],plot3data[,4],type="l",col="blue")
legend("topright",lty=c(1,1,1),col=c("black","red","blue"),
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),cex=0.4,bty="n")

plot4ddata <- data.frame(timestamp,as.numeric(unlist(
                                df2$Global_reactive_power)))
plot(plot4ddata[,1],plot4ddata[,2],type="l",
     ylab="Global_reactive_power",xlab="datetime",cex.lab="0.75")





