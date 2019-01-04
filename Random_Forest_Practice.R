library(MASS)
library(randomForest)
data(package="MASS")
boston<-Boston
dim(boston)
names(boston)

# crim
# per capita crime rate by town.
# zn
# proportion of residential land zoned for lots over 25,000 sq.ft.
# indus
# proportion of non-retail business acres per town.
# chas
# Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# nox
# nitrogen oxides concentration (parts per 10 million).
# rm
# average number of rooms per dwelling.
# age
# proportion of owner-occupied units built prior to 1940.
# dis
# weighted mean of distances to five Boston employment centres.
# rad
# index of accessibility to radial highways.
# tax
# full-value property-tax rate per $10,000.
# ptratio
# pupil-teacher ratio by town.
# black
# 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# lstat
# lower status of the population (percent).
# medv
# median value of owner-occupied homes in $1000s.


#set seed includes a random number generator in order to take a random sample from the data 
#I believe there is a split function that takes an unbiased sample on purpose need to look into this
set.seed(101)
train = sample(1:nrow(boston), 300)

rf.boston = randomForest(medv~., data = boston, subset = train)
rf.boston
plot(rf.boston)
