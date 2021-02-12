

rm(list=ls())
library(ggplot2)
library(scales)   # for date_format
df.temp_pH <- read.csv(file = "data/Sample_SeaFET_data.csv")
head(df.temp_pH)
summary(df.temp_pH)

# rename the variables because they are too long
colnames(df.temp_pH) <- c("Time1", "Temp", "pH")

df.temp_pH$Date1 <- strptime(df.temp_pH$Time1,
                            format = "%m/%d/%y %H:%M")

# as.Date doesn't read hours and minutes.
df.temp_pH$Date2 <- as.Date(df.temp_pH$Time1,
                            format = "%m/%d/%y %H:%M")
str(df.temp_pH)


plot1 <- ggplot(data = df.temp_pH, 
                aes(x = Date1, y = Temp)) + 
  geom_line(color = "blue") + 
  xlab("Date") + 
  ylab("Temperature (C)") + 
  theme(axis.text = element_text(size = 12))

plot1


plot2 <- ggplot(data = df.temp_pH, 
                aes(x = Date2, y = Temp)) + 
  geom_line(color = "blue") + 
  xlab("Date") + 
  ylab("Temperature (C)") + 
  theme(axis.text = element_text(size = 12)) + 
scale_x_date(labels = date_format("%m/%d/%y"))

plot2
