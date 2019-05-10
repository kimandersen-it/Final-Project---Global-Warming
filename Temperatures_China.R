Temp_Data_Cntry <- read.csv("GlobalLandTemperaturesByCountry.csv") 
Temp_Data_Cntry <- na.omit(Temp_Data_Cntry)
names(Temp_Data_Cntry)

library(lubridate)

Temp_Data_Cntry$Year <- year(Temp_Data_Cntry$dt)

CHN_data <- subset(Temp_Data_Cntry, Country == "China")
summary(CHN_data)
str(CHN_data)

CHN_data$dt <- as.Date(CHN_data$dt)
str(CHN_data)
CHN_data$Year <- year(CHN_data$dt)
names(CHN_data)

Avg_temp_CHN <- aggregate(AverageTemperature ~ Year, FUN=mean, data = CHN_data)

#Data Visualizations
library(ggplot2)

#Visualization for the increase in temperature for last ~170 years
CHN_170 <- subset(Avg_temp_CHN, Year >= 1850)

ggplot(CHN_170, aes(Year, AverageTemperature)) + 
  geom_line(size = 2, aes(colour = AverageTemperature)) +
  scale_colour_gradient(low="light yellow", high="dark orange") +
  scale_x_continuous(breaks=seq(1850, 2015, 15)) + 
  scale_y_continuous(breaks=seq(5, 10, 0.2)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of China in last ~170 Years")

#Visualization for the increase in temperature for last ~100 years
CHN_100 <- subset(Avg_temp_CHN, Year >= 1920)

ggplot(CHN_100, aes(Year, AverageTemperature)) + 
  geom_line(size = 2, aes(colour = AverageTemperature)) + 
  scale_colour_gradient(low="light green", high="orange") +
  scale_x_continuous(breaks=seq(1920, 2015, 15)) + 
  scale_y_continuous(breaks=seq(5, 15, 0.2)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of China in last ~100 Years")

#Visualization for the increase in temperature for last ~50 years
CHN_50 <- subset(Avg_temp_CHN, Year >= 1970)

ggplot(CHN_50, aes(Year, AverageTemperature)) +
  geom_line(size = 2, aes(colour = AverageTemperature)) + 
  scale_colour_gradient(low="purple", high="pink") +
  scale_x_continuous(breaks=seq(1970, 2015, 5)) + 
  scale_y_continuous(breaks=seq(6, 10, 0.1)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of China in last ~50 Years")

#Visualization for the increase in temperature for last ~25 years
CHN_25 <- subset(Avg_temp_CHN, Year >= 1995)

ggplot(CHN_25, aes(Year, AverageTemperature)) +
  geom_line(size = 2, aes(colour = AverageTemperature)) + 
  scale_colour_gradient(low="light green", high="orange") +
  scale_x_continuous(breaks=seq(1995, 2015, 2)) + 
  scale_y_continuous(breaks=seq(6, 10, 0.1)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of China in last ~25 Years")
