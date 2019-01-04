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

data <- read.csv("C:/Temp/R/waiting_time_resources.csv",header =TRUE)
data$arrival.time<- data$arrival.time/3600  #turn arrival time into hours

#Select the columns that I want
data <- data[c(2,4,5,17,10,11,18)]

#subset the data into all of the relevant transactions
data_queues <- data[c(2,3,4,5,6)]
data_pw<-subset(data, TXN_TYPE == 'pos_adv_annuity_partial_withdrawal')
data_adv_sw <-subset(data, TXN_TYPE == 'pos_adv_payout_system_withdrawal')
#should combine the advanced transfers and outgoing transfers possibly or maybe just look at advanced
data_ot <-subset(data, TXN_TYPE == 'pos_annuity_outgoing_transfers')
data_adv_ot<-subset(data, TXN_TYPE == 'pos_adv_annuity_outgoing_transfers')
data_claims<-subset(data, TXN_TYPE == 'claims_annuity')
data_rmd<-subset(data, TXN_TYPE == 'pos_adv_one_time_rmd')
data_sw<-subset(data, TXN_TYPE == 'pos_payout_system_withdrawal')



#get rid of the TXN_TYPE for the decision trees themselves
data<-data[c(3,4,5,6,7)]
data_adv_sw <-data_adv_sw[c(2,3,4,5,6)]
data_ot<-data_ot[c(2,3,4,5,6)]
data_adv_ot<-data_adv_ot[c(2,3,4,5,6)]
data_claims<-data_claims[c(2,3,4,5,6)]
data_rmd<-data_rmd[c(2,3,4,5,6)]
data_sw<-data_sw[c(2,3,4,5,6)]


unique(data$TXN_TYPE)
#data$start.time<- data$start.time/3600
#data$event.time <- data$event.time/3600
#data$end.time <- data$end.time/3600


##Decision Trees

#Decision Tree of Everything
form <- as.formula(waiting.mins ~ .)
tree.2 <- rpart(form,data)			# A more reasonable tree
prp(tree.2)                                     # A fast plot													
fancyRpartPlot(tree.2)
text(tree.data, pretty=0)


#partial withdrawal decision tree
form <- as.formula(waiting.mins ~ .)
tree.3 <- rpart(form,data_pw)			# A more reasonable tree
prp(tree.3)                                     # A fast plot													
fancyRpartPlot(tree.3)
text(tree.data, pretty=0)
title("Advanced Partial Withdrawals")


#Queue Based Decision Tree
form <- as.formula(waiting.mins ~ .)
tree.4 <- rpart(form,data_queues)			# A more reasonable tree
prp(tree.4)                                     # A fast plot													
fancyRpartPlot(tree.4)
text(tree.data, pretty=0)

#Advanced Systematic Withdrawals
form <- as.formula(waiting.mins ~ .)
tree.5 <- rpart(form,data_adv_sw)			# A more reasonable tree
prp(tree.5)                                     # A fast plot													
fancyRpartPlot(tree.5)
text(tree.data, pretty=0)
title("Advanced Systematic Withdrawals")

#Systematic Withdrawals
form <- as.formula(waiting.mins ~ .)
tree.6 <- rpart(form,data_sw)			# A more reasonable tree
prp(tree.6)                                     # A fast plot													
fancyRpartPlot(tree.6)
text(tree.data, pretty=0)
title("Systematic Withdrawals")


#Outgoing Transfers
form <- as.formula(waiting.mins ~ .)
tree.7 <- rpart(form,data_ot)			# A more reasonable tree
prp(tree.7)                                     # A fast plot													
fancyRpartPlot(tree.7)
text(tree.data, pretty=0)
title("Outgoing Transfers")

#Advanced Outgoing Transfers
form <- as.formula(waiting.mins ~ .)
tree.8 <- rpart(form,data_adv_ot)			# A more reasonable tree
prp(tree.8)                                     # A fast plot													
fancyRpartPlot(tree.8)
text(tree.data, pretty=0)
title("Advanced Outgoing Transfers")

#Claims Annuity
form <- as.formula(waiting.mins ~ .)
tree.9 <- rpart(form,data_claims)			# A more reasonable tree
prp(tree.9)                                     # A fast plot													
fancyRpartPlot(tree.9)
text(tree.data, pretty=0)
title("Claims Annuity")

#One Time RMD
form <- as.formula(waiting.mins ~ .)
tree.10 <- rpart(form,data_rmd)			# A more reasonable tree
prp(tree.10)                                     # A fast plot													
fancyRpartPlot(tree.10)
text(tree.data, pretty=0)
title("One Time RMD")






#a little more explatory analysis of the variables
cor(data$waiting.mins,data$Skilled_AA_Hours)

cor(data$waiting.mins,data$arrival.time)


##Reload the full data set to do some single variable analysis
data <- read.csv("C:/Temp/R/waiting_time_resources.csv",header =TRUE)
data$arrival.time<- data$arrival.time/3600

#Select the columns that I want
data <- data[c(2,4,5,17,10,11,18)]


ddply(data,~data$lob,summarise,mean=mean(waiting.mins),sd=sd(waiting.mins))
ddply(data,~data$Tier,summarise,mean=mean(waiting.mins),sd=sd(waiting.mins))
QUEUE_WAIT_TIMES <- ddply(data,~data$DESCRIPTION_X,summarise,mean=mean(waiting.mins),sd=sd(waiting.mins))
QUEUE_WAIT_TIMES
TXN_WAIT_TIMES <- ddply(data,~data$TXN_TYPE,summarise,mean=mean(waiting.mins),sd=sd(waiting.mins))
TXN_WAIT_TIMES


