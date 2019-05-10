Temp_Data_Cntry <- read.csv("GlobalLandTemperaturesByCountry.csv") 
Temp_Data_Cntry <- na.omit(Temp_Data_Cntry)
names(Temp_Data_Cntry)

library(lubridate)

Temp_Data_Cntry$Year <- year(Temp_Data_Cntry$dt)

BZL_data <- subset(Temp_Data_Cntry, Country == "Brazil")
summary(BZL_data)
str(BZL_data)

BZL_data$dt <- as.Date(BZL_data$dt)
str(BZL_data)
BZL_data$Year <- year(BZL_data$dt)
names(BZL_data)

Avg_temp_BZL <- aggregate(AverageTemperature ~ Year, FUN=mean, data = BZL_data)

#Data Visualizations
library(ggplot2)

#Visualization for the increase in temperature for last ~170 years
BZL_170 <- subset(Avg_temp_BZL, Year >= 1850)

ggplot(BZL_170, aes(Year, AverageTemperature)) + 
  geom_line(size = 2, aes(colour = AverageTemperature)) +
  scale_colour_gradient(low="light yellow", high="dark orange") +
  scale_x_continuous(breaks=seq(1850, 2015, 15)) + 
  scale_y_continuous(breaks=seq(22, 26, 0.5)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of Brazil in last ~170 Years")

#Visualization for the increase in temperature for last ~100 years
BZL_100 <- subset(Avg_temp_BZL, Year >= 1915)

ggplot(BZL_100, aes(Year, AverageTemperature)) + 
  geom_line(size = 2, aes(colour = AverageTemperature)) + 
  scale_colour_gradient(low="light green", high="orange") +
  scale_x_continuous(breaks=seq(1915, 2015, 10)) + 
  scale_y_continuous(breaks=seq(22, 26, 0.5)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of Brazil in last ~100 Years")

#Visualization for the increase in temperature for last ~50 years
BZL_50 <- subset(Avg_temp_BZL, Year >= 1970)

ggplot(BZL_50, aes(Year, AverageTemperature)) +
  geom_line(size = 2, aes(colour = AverageTemperature)) + 
  scale_colour_gradient(low="purple", high="pink") +
  scale_x_continuous(breaks=seq(1970, 2015, 5)) + 
  scale_y_continuous(breaks=seq(22, 26, 0.2)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of Brazil in last ~50 Years")

#Visualization for the increase in temperature for last ~25 years
BZL_25 <- subset(Avg_temp_BZL, Year >= 1995)

ggplot(BZL_25, aes(Year, AverageTemperature)) +
  geom_line(size = 2, aes(colour = AverageTemperature)) + 
  scale_colour_gradient(low="light green", high="orange") +
  scale_x_continuous(breaks=seq(1995, 2015, 2)) + 
  scale_y_continuous(breaks=seq(22, 26, 0.2)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Temperature of Brazil in last ~25 Years")

