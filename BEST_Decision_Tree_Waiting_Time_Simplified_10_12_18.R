library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)

data <- read.csv("C:/Temp/R/waiting_time_data_2.csv",header =TRUE)

#Select the columns that I want
data <- data[c(5,6,10,11,12,15)]
#turn arrival time into hours
data$arrival.time<- data$arrival.time/3600

#define the variable that I want to evaluate in form
form <- as.formula(waiting.mins ~ .)
tree.2 <- rpart(form,data)			# A more reasonable tree
prp(tree.2)                                     # A fast plot													
fancyRpartPlot(tree.2)
text(tree.data, pretty=0)