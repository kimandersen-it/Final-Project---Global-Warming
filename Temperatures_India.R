Temp_Data_Cntry <- read.csv("GlobalLandTemperaturesByCountry.csv") 
Temp_Data_Cntry <- na.omit(Temp_Data_Cntry)
names(Temp_Data_Cntry)

library(lubridate)

Temp_Data_Cntry$Year <- year(Temp_Data_Cntry$dt)

IND_data <- subset(Temp_Data_Cntry, Country == "India")
summary(IND_data)
str(IND_data)

IND_data$dt <- as.Date(IND_data$dt)
str(IND_data)
IND_data$Year <- year(IND_data$dt)
names(IND_data)

Avg_temp_IND <- aggregate(AverageTemperature ~ Year, FUN=mean, data = IND_data)

#Data Visualizations
library(ggplot2)

#Visualization for the increase in temperature for last ~170 years
IND_170 <- subset(Avg_temp_IND, Year >= 1850)

ggplot(IND_170, aes(Year, AverageTemperature)) + 
  geom_line(size = 2, aes(colour = AverageTemperature)) +
  scale_colour_gradient(low="light yellow", high="dark orange") +
  scale_x_continuous(breaks=seq(1850, 2015, 15)) + 
  scale_y_continuous(breaks=seq(15, 30, 0.5)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of India in last ~170 Years")

#Visualization for the increase in temperature for last ~100 years
IND_100 <- subset(Avg_temp_IND, Year >= 1920)

ggplot(IND_100, aes(Year, AverageTemperature)) + 
  geom_line(size = 2, aes(colour = AverageTemperature)) + 
  scale_colour_gradient(low="light green", high="orange") +
  scale_x_continuous(breaks=seq(1920, 2015, 10)) + 
  scale_y_continuous(breaks=seq(20, 30, 0.1)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of India in last ~100 Years")

#Visualization for the increase in temperature for last ~50 years
IND_50 <- subset(Avg_temp_IND, Year >= 1970)

ggplot(IND_50, aes(Year, AverageTemperature)) +
  geom_line(size = 2, aes(colour = AverageTemperature)) + 
  scale_colour_gradient(low="purple", high="pink") +
  scale_x_continuous(breaks=seq(1970, 2015, 5)) + 
  scale_y_continuous(breaks=seq(20, 30, 0.1)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of India in last ~50 Years")

#Visualization for the increase in temperature for last ~25 years
IND_25 <- subset(Avg_temp_IND, Year >= 1995)

ggplot(IND_25, aes(Year, AverageTemperature)) +
  geom_line(size = 2, aes(colour = AverageTemperature)) + 
  scale_colour_gradient(low="light green", high="orange") +
  scale_x_continuous(breaks=seq(1995, 2015, 2)) + 
  scale_y_continuous(breaks=seq(20, 30, 0.1)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of India in last ~25 Years")

