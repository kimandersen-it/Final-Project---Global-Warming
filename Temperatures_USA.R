Temp_Data_Cntry <- read.csv("GlobalLandTemperaturesByCountry.csv") 
Temp_Data_Cntry <- na.omit(Temp_Data_Cntry)
names(Temp_Data_Cntry)

#Data Preparation
library(lubridate)

Temp_Data_Cntry$Year <- year(Temp_Data_Cntry$dt)

US_data <- subset(Temp_Data_Cntry, Country == "United States")
summary(US_data)
str(US_data)

US_data$dt <- as.Date(US_data$dt)
str(US_data)
US_data$Year <- year(US_data$dt)
names(US_data)

Avg_temp_US <- aggregate(AverageTemperature ~ Year, FUN=mean, data = US_data)

#Data Visualizations
library(ggplot2)

#Visualization for the increase in temperature for last ~170 years
US_170 <- subset(Avg_temp_US, Year >= 1850)

ggplot(US_170, aes(Year, AverageTemperature)) + 
  geom_line(size = 2, aes(colour = AverageTemperature)) +
  scale_colour_gradient(low="light yellow", high="dark orange") +
  scale_x_continuous(breaks=seq(1850, 2015, 15)) + 
  scale_y_continuous(breaks=seq(-3.0, 12, 1)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of USA in last ~170 Years")

#Visualization for the increase in temperature for last ~100 years
US_100 <- subset(Avg_temp_US, Year >= 1915)

ggplot(US_100, aes(Year, AverageTemperature)) + 
  geom_line(size = 2, aes(colour = AverageTemperature)) + 
  scale_colour_gradient(low="light green", high="orange") +
  scale_x_continuous(breaks=seq(1915, 2015, 10)) + 
  scale_y_continuous(breaks=seq(7, 12, 0.4)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of USA in last ~100 Years")

#Visualization for the increase in temperature for last ~50 years
US_50 <- subset(Avg_temp_US, Year >= 1970)

ggplot(US_50, aes(Year, AverageTemperature)) +
  geom_line(size = 2, aes(colour = AverageTemperature)) + 
  scale_colour_gradient(low="purple", high="pink") +
  scale_x_continuous(breaks=seq(1970, 2015, 5)) + 
  scale_y_continuous(breaks=seq(8, 12, 0.2)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of USA in last ~50 Years")

#Visualization for the increase in temperature for last ~25 years
US_25 <- subset(Avg_temp_US, Year >= 1995)

ggplot(US_25, aes(Year, AverageTemperature)) +
  geom_line(size = 2, aes(colour = AverageTemperature)) + 
  scale_colour_gradient(low="light green", high="orange") +
  scale_x_continuous(breaks=seq(1995, 2015, 2)) + 
  scale_y_continuous(breaks=seq(7, 12, 0.2)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of USA in last ~25 Years")