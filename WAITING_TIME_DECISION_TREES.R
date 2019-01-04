library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.csv("C:/Temp/R/waiting_time_data_1.csv",header =TRUE)

data <- data[c(5,6,10,11,12,15)]
#turn arrival time into hours
data$arrival.time<- data$arrival.time/3600

hist(data$waiting.mins)

#adding in the High Variable
#we are turning waiting time into a binary response variable
#High = ifelse(data$waiting.mins<=50, "No", "Yes")
#data = data.frame(data, High)

tree.data = tree(waiting.mins ~., data=data)

plot(tree.data)
text(tree.data, pretty=0)