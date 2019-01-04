library(plyr)
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

data <- read.csv("O:/Business Process Engineering/BPEAs/Automated SAS Projects/Automated Report Data/Edits Dashboard Data/ADVANCED_DISBURSEMENTS_TXN_STATS.csv",header =TRUE)
data$Processing_Time <- data_4$PROCESSING_TIME.min._Sum
data$PROCESSING_TIME.min._Sum <- NULL
data$PROCESSING_TIME.min._N <- NULL


##Decision Trees

#Decision Tree of Everything
form <- as.formula(Processing_Time ~ .)
tree.2 <- rpart(form,data)			# A more reasonable tree
prp(tree.2)                                     # A fast plot													
fancyRpartPlot(tree.2)
text(tree.data, pretty=0)

