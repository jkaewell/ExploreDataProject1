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
##  x <- paste(df2[1440,1],df2[1440,2])
##  strptime(x, "%d/%m/%Y %H:%M:%S)
df2$timestamp <- strptime(paste(df2[,1],df2[,2]),"%d/%m/%Y %H:%M:%S")

library(dplyr)

Global_active_power <- as.numeric(unlist(df2$Global_active_power))

# create the plot2
#
timestamp <- df2$timestamp
plot2data <- data.frame(timestamp,Global_active_power)
plot(plot2data,type="l",ylab="Global Active Power (kilowats)",xlab="")
##

