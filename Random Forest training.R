library(MASS)
data(package="MASS")
boston<-Boston
dim(boston)
names(boston)
require(randomForest)

set.seed(101)
train = sample(1:nrow(boston), 300)

rf.boston = randomForest(medv~., data = boston, subset = train)
rf.boston

oob.err = double(13)
test.err = double(13)
for(mtry in 1:13){
  fit = randomForest(medv~., data = boston, subset=train, mtry=mtry, ntree = 350)
  oob.err[mtry] = fit$mse[350]
  pred = predict(fit, boston[-train,])
  test.err[mtry] = with(boston[-train,], mean( (medv-pred)^2 ))
}

matplot(1:mtry, cbind(test.err, oob.err), pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))
########################################################################################################################33


options(repos='http://cran.rstudio.org')
have.packages <- installed.packages()
cran.packages <- c('devtools','plotrix','randomForest','tree')
to.install <- setdiff(cran.packages, have.packages[,1])
if(length(to.install)>0) install.packages(to.install)

library(devtools)
if(!('reprtree' %in% installed.packages())){
  install_github('araastat/reprtree')
}
for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))


library(randomForest)
library(reprtree)

model <- randomForest(Species ~ ., data=iris, importance=TRUE, ntree=500, mtry = 2, do.trace=100)

reprtree:::plot.getTree(model)


###############################################################################################
library(party)
library(randomForest)

cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

cf <- cforest(Species ~ ., data = iris) 
pt <- party:::prettytree(cf@ensemble[[1]], names(cf@data@get("input"))) 
pt 
nt <- new("BinaryTree") 
nt@tree <- pt 
nt@data <- cf@data 
nt@responses <- cf@responses 
nt 
plot(nt) 


data_1 <-read.csv("U:/ATTRIBUTES_BY_REQUEST.csv",header =TRUE)

cf <- cforest(ELAPSED_MIN_Sum ~ ., data = data_1) 
pt <- party:::prettytree(cf@ensemble[[1]], names(cf@data@get("input"))) 
pt 
nt <- new("BinaryTree") 
nt@tree <- pt 
nt@data <- cf@data 
nt@responses <- cf@responses 
nt 
plot(nt) 



