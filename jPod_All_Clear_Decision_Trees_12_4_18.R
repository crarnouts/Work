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
library(plotly)
library(ggcorrplot)

data <- read.csv("U:/WORK_PROCESSING_TIME_BY_REQUEST.csv",header =TRUE)
data <- read.csv("U:/ATTRIBUTES_BY_REQUEST.csv",header =TRUE)

data_4<- read.csv("O:/Business Process Engineering/BPEAs/Automated SAS Projects/Automated Report Data/Edits Dashboard Data/ADVANCED_DISBURSEMENTS_TXN_STATS.csv",header =TRUE)


##############################################################

#data_4 <- data_4[sample(nrow(data_4),5000),]
data_4$TXN_ID <- NULL
data_4$FIRST_EVENT_DATE1 <- NULL
data_4$PROCESSING_TIME.min._N <- NULL
data_4$PRODUCT <- NULL
data_4$Transaction.Type <- NULL
data_4$FIRST_EVENT_DATE <- NULL

data_4$REQUIREMENT <- NULL


#tree.data = tree(Processing_Time ~., data=data_4)

#Decision Tree of Everything
form <- as.formula(PROCESSING_TIME.min._Sum ~ .)

tree.2 <- rpart(form,data_4)			# A more reasonable tree
prp(tree.2)  
new.tree <-prp(tree.2,snip=TRUE)$obj
# A fast plot													
fancyRpartPlot(new.tree)
text(tree.data, pretty=0)
title("Attributes affecting Processing Time")



#####################BY POLICY ATTRIBUTE#####################
data_policy_specs <- data_4[c(2,3,4,5,6)]
form <- as.formula(PROCESSING_TIME.min._Sum ~ .)

tree.2 <- rpart(form,data_policy_specs)			# A more reasonable tree
prp(tree.2)  
new.tree <-prp(tree.2,snip=TRUE)$obj
# A fast plot													
fancyRpartPlot(new.tree)
text(tree.data, pretty=0)
title("Attributes affecting Processing Time")


##################################THROUGHPUT#################################

data_4$REQUIREMENT <- NULL
data_4$Transaction.Type <- NULL
#tree.data = tree(Processing_Time ~., data=data_4
data_4$FIRST_EVENT_DATE <- NULL
data_4$LAST_EVENT_HOUR <- NULL

data_VA <-filter(data_4,LOB_C=="VARIABLE")


#Decision Tree of Everything
form <- as.formula(Throughput_Time.hour. ~ .)

tree.2 <- rpart(form,data_VA)			# A more reasonable tree
prp(tree.2)  
new.tree <-prp(tree.2,snip=TRUE)$obj
# A fast plot													
fancyRpartPlot(new.tree)
text(tree.data, pretty=0)
title("Attributes affecting Throughput Time")

############################################################################ boxplot ##################
p<-ggplot(data_4, aes(x=REQUIREMENT, y=PROCESSING_TIME.min._Sum)) +
  geom_boxplot()
p
##################################################################
data$CASE_ID <- NULL

#define the data_trial dataset
data_trial <- data[c(2,3,4)]
(data_trial_edits_mean <- mean(data_trial$EDIT.COUNTS))
(data_trial_txns_mean <- mean(data_trial$TXNS_PER_REQUEST))

#mean of the variables
(data_trial_edits_mean <- mean(data_trial$EDIT.COUNTS))
(data_trial_txns_mean <- mean(data_trial$TXNS_PER_REQUEST))

#min of the variables
(data_trial_edits_min <- min(data_trial$EDIT.COUNTS))
(data_trial_txns_min <- min(data_trial$TXNS_PER_REQUEST))

#max of the variables
(data_trial_edits_max <- max(data_trial$EDIT.COUNTS))
(data_trial_txns_max <- max(data_trial$TXNS_PER_REQUEST))

#standard deviation of variables 
(data_trial_edits_sd <- sd(data_trial$EDIT.COUNTS))
(data_trial_txns_sd <- sd(data_trial$TXNS_PER_REQUEST))

(cor(data_trial$ELAPSED_MIN_Sum,data_trial$EDIT.COUNTS))
(cor(data_trial$ELAPSED_MIN_Sum,data_trial$TXNS_PER_REQUEST))
(cor(data_trial$TXNS_PER_REQUEST,data_trial$EDIT.COUNTS))

data_trial$EDIT.COUNTS_rescale<- (data_trial$EDIT.COUNTS-data_trial_edits_min)/(data_trial_edits_max-data_trial_edits_min)
data_trial$TXN.COUNT_rescale<- (data_trial$TXNS_PER_REQUEST-data_trial_txns_min)/(data_trial_txns_max-data_trial_txns_min)

data_trial$EDIT.COUNTS_standard <- data_trial$EDIT.COUNTS/data_trial_edits_mean
data_trial$TXN.COUNT_standard <- data_trial$TXNS_PER_REQUEST/data_trial_txns_mean

#Comparing the new metrics to see how they correlate with Processing time
data_trial$mega_cor <- data_trial$EDIT.COUNTS_standard + data_trial$TXN.COUNT_standard
data_trial$rescale_total <- data_trial$EDIT.COUNTS_rescale+data_trial$TXN.COUNT_rescale

(cor(data_trial$ELAPSED_MIN_Sum,data_trial$rescale_total))

(cor(data_trial$ELAPSED_MIN_Sum,data_trial$mega_cor))


cor1 <- cor(data$ELAPSED_MIN_Sum,data$EDIT.COUNTS)
cor2<- cor(data$EDIT.COUNTS,data$TXNS_PER_REQUEST)
cor3 <- cor(data_6$Processing_Time,data_6$Frequency.Count)

data_6 <- filter(data_6, data_6$Frequency.Count <100)
data_6 <- filter(data_6, data_6$Processing_Time > 3)
  
ggplot(data_6, aes(x=data_6$Frequency.Count, y=data_6$Processing_Time)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)


(uniquecaus <- cor1*(1-cor2))

data[is.na(data)]<-0
##Decision Trees

#Decision Tree of Everything
form <- as.formula(ELAPSED_MIN_Sum ~ .)

tree.2 <- rpart(form,data)			# A more reasonable tree
prp(tree.2)  
new.tree <-prp(tree.2,snip=TRUE)$obj
# A fast plot													
fancyRpartPlot(new.tree)
text(tree.data, pretty=0)
title("Transactons Per Request effect on Processing Time")

#########################################################################NEW Decision Trees###############################################################################
data_4<- read.csv("O:/Business Process Engineering/BPEAs/Automated SAS Projects/Automated Report Data/Edits Dashboard Data/ADVANCED_DISBURSEMENTS_TXN_STATS.csv",header =TRUE)
data_4$Processing_Time <- data_4$PROCESSING_TIME.min._Sum
data_4$PROCESSING_TIME.min._Sum <- NULL
data_4$PROCESSING_TIME.min._N <- NULL

data_4 <- data_4[complete.cases(data_4), ]

cor(data_4$Processing_Time,data_4$TRANSACTION_AMOUNT_NUM)

data_5 <- data_4[sample(nrow(data_4),1000),]


data_5$TXN_ID <- NULL
data_5$FIRST_EVENT_DATE <- NULL
data_5$LAST_EVENT_DATE <-NULL
data_5$FIRST_EVENT_DATE1<- NULL



form <- as.formula(Processing_Time ~ .)

tree.2 <- rpart(form,data_5)			# A more reasonable tree
prp(tree.2)  
new.tree <-prp(tree.2,snip=TRUE)$obj
# A fast plot													
fancyRpartPlot(new.tree)
text(tree.data, pretty=0)
title("Attributes the effect Processing Time")

#####################################################################################################################################################################



data_2<-data[c(2,3,4)]

#Unneccssary Edits affect on Transactions
form <- as.formula(ELAPSED_MIN_Sum ~ .)

tree.2 <- rpart(form,data_2)			# A more reasonable tree
prp(tree.2)  
new.tree <-prp(tree.2,snip=TRUE)$obj
# A fast plot													
fancyRpartPlot(new.tree)
text(tree.data, pretty=0)
title("Unneccssary Edits affect on Transactions")



data <- filter(data, data$ELAPSED_MIN_Sum < 180)
data <- filter(data, data$TXNS_PER_REQUEST < 10)

ggplot(data, aes(x=data$TXNS_PER_REQUEST, y=data$ELAPSED_MIN_Sum)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)


data <- filter(data, data$ELAPSED_MIN_Sum < 180)


ggplot(data, aes(x=data$EDIT.COUNTS, y=data$ELAPSED_MIN_Sum)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)

plot_ly(data, x =~data$TXNS_PER_REQUEST ,y = ~data$ELAPSED_MIN_Sum, type = "box")


unique(data$TXN_TYPE)
#data <- filter(data, data$TXN_TYPE == "pos_adv_annuity_partial_withdrawal")



cor(data$EDIT.COUNTS,data$ELAPSED_MIN_Sum)


data_cor<- data[c(3:8)]

#Correlation matrix
corr <- round(cor(data_cor), 1)
head(corr[, 1:6])

ggcorrplot(corr, method = "circle")


#Transfers Investigation
transfers_data <- data %>% filter(TXN_TYPE == "pos_adv_annuity_outgoing_transfers")

(cor1 <- cor(transfers_data$ELAPSED_MIN_Sum,transfers_data$RESTART_COUNT))



#Decision Tree of Everything
form <- as.formula(ELAPSED_MIN_Sum ~ .)

tree.2 <- rpart(form,transfers_data)			# A more reasonable tree
prp(tree.2)  
new.tree <-prp(tree.2,snip=TRUE)$obj
# A fast plot													
fancyRpartPlot(new.tree)
text(tree.data, pretty=0)
title("Transactons Per Request effect on Processing Time")

############################################################################################
data_4$Processing_Time <- data_4$PROCESSING_TIME.min._Sum
data_4$PROCESSING_TIME.min._Sum <- NULL
data_4$PROCESSING_TIME.min._N <- NULL
data_4$FIRST_EVENT_DATE1 <- NULL


#Decision Tree of Everything
form <- as.formula(Processing_Time ~ .)

tree.2 <- rpart(form,data_4)			# A more reasonable tree
prp(tree.2)  
new.tree <-prp(tree.2,snip=FALSE)$obj
# A fast plot													
fancyRpartPlot(new.tree)
text(tree.data, pretty=0)
title("Processing Time")



