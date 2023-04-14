library(dplyr)

#Data manipulation
data <- read.table("household_power_consumption.txt", header = T, sep = ";")
data <- filter(data, data$Date == "1/2/2007" | data$Date == "2/2/2007")
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
data$Time <- strptime(data$Time, format="%H:%M:%S")
data[data$Date == "2007-02-01",]$Time <- format(data[data$Date == "2007-02-01",]$Time, 
                                                "2007-02-01 %H:%M:%S")
data[data$Date == "2007-02-02",]$Time <- format(data[data$Date == "2007-02-01",]$Time, 
                                                "2007-02-02 %H:%M:%S")

#Figure.1
hist(as.numeric(data$Global_active_power), main = "Global Active Power", 
     xlab = "Global Active Power(kilowatts)", col = "red")

#Figure.2
plot(data$Time,as.numeric(data$Global_active_power), main = "Global Active Power Vs Time", 
     xlab = "", ylab="Global Active Power (kilowatts)", type = "l")

#Figure.3
plot(data$Time,data$Sub_metering_1, main = "Energy sub-metering", 
     xlab="", ylab="Energy sub metering", type="n")
lines(data$Time,as.numeric(data$Sub_metering_1))
lines(data$Time,as.numeric(data$Sub_metering_2),col="red")
lines(data$Time,as.numeric(data$Sub_metering_3),col="blue")
legend("topright", lty=1, legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
       col=c("black","red","blue"))

#Figure.4
par(mfrow=c(2,2))

plot(data$Time,as.numeric(data$Global_active_power), xlab="", 
     ylab="Global Active Power", type="l")  

plot(data$Time,as.numeric(data$Voltage), xlab="datetime", 
     ylab="Voltage", type="l")

plot(data$Time,data$Sub_metering_1, xlab="", ylab="Energy sub metering", type="n")
lines(data$Time,as.numeric(data$Sub_metering_1))
lines(data$Time,as.numeric(data$Sub_metering_2),col="red")
lines(data$Time,as.numeric(data$Sub_metering_3),col="blue")
legend("topright", lty=1, legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
       col=c("black","red","blue"))

plot(data$Time,as.numeric(data$Global_reactive_power), xlab="datetime", 
     ylab="Global_reactive_power", type="l")


















