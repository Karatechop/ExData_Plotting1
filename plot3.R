# This script uses R base plotting system to construct and output a PNG file
# with a width of 480 pixels and a height of 480 pixels containing Plot 3 as specified
# in Data Science - Exploratory Data Analysis Course Project 1 assignment.
#
# As per assignment specification it is required that all plots are based on  
# measurements of electric power consumption in one household with a one-minute 
# sampling rate over a period of two days  2007-02-01 and 2007-02-02.
# 
# Complete data set is located in the "household_power_consumption.txt" file.
# Both "household_power_consumption.txt" file and the file containing this script
# have to be in the same working folder in order for this scrip to work.
#
# Assignment specification suggests that as the complete dataset contains data 
# collected over the course of 4 years of which only two days are relevant for the 
# assignment, it makes sense to read only data from those two days into R to save memory.
# Initial data exploration consisted of reading complete data set into R and 
# locating the rows that contain data dated 2007-02-01 and 2007-02-02. This was
# achieved as follows:
# 	data<- read.table("./household_power_consumption.txt",
#			header = TRUE	,
#			sep = ";", 
#			col.names = c( "Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
#			na.strings = "?", 
#			colClasses = c( "character", "character", rep("numeric", 7)),
#			skip = 1)
#
#
# 	head(which(data$Date == "1/2/2007"))
# 	tail(which(data$Date == "2/2/2007"))
#
# After this it bacame clear that 2880 rows starting with row 66637 of the original 
# data set contain all data dated 2007-02-01 and 2007-02-02.


# Load 2007-02-01 and 2007-02-02 data
	data<- read.table("./household_power_consumption.txt", 
                  header = TRUE, 
                  sep = ";", 
                  col.names = c( "Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
                  na.strings = "?", 
                  colClasses = c( "character", "character", rep("numeric", 7)), 
                  nrows =2880, 
                  skip = 66636)

# Update data with POSIXlt DateTime column and remove Date and Time character columns

	data<-data.frame(DateTime = strptime(paste(data$Date, data$Time), "%e/%m/%Y %T"), data[,3:9])

	print("data created")

# Define function plot3() that will output the PNG file containing Plot 3

	plot3<-function(dataset, plotfile) {
  
		 # Set English US locale
		  Sys.setlocale("LC_TIME", "English")
		  
		  # Open the PNG device
		  png(filename = plotfile,
		      width = 480, height = 480, units = "px",
		      bg = "white")
		  
		  print("png open")
		  
		  # Create plot
		  plot(dataset$DateTime, dataset$Global_active_power, type = "n",
		       xlab = "", ylab = "Energy sub metering", ylim=c(0,40))
		  
		  points( dataset$DateTime, dataset$Sub_metering_1, type = "l", 
		  col = "black")
		  
		  points( dataset$DateTime, dataset$Sub_metering_2, type = "l", 
		  col = "red")
		  
		  points( dataset$DateTime, dataset$Sub_metering_3, type = "l", 
		  col = "blue")

		  legend( "topright", 
		  	legend = c( "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
		  	col = c( "black", "red", "blue"), 
		  	lty = 1)
		       
		  print("plot created")
		  
		  # Close the PNG device
		  dev.off()
		  
		  print("png closed")
		  
		  # Re-set my original locale
		  Sys.setlocale("LC_TIME", "Slovenian")
		  }

# Create Plot 3

	plot3(data, "./plot3.png")

	print("Script executed successfully, plot saved to plot3.png in working directory")
