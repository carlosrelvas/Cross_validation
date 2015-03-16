setwd("C:/Users/Cacá Relvas/Desktop/blog/")

data = read.table("spambase.data", sep=",", header=F)

data[,"V58"] = as.factor(ifelse(data[,"V58"]==1, "Spam", "NotSpam"))
id = sample(1:nrow(data), 3000)
data.train = data[id,]
data.test = data[-id,]

formula = paste0("V58 ~ ", paste0(paste0("V", 1:57), collapse="+"))

install.packages("ROCR")

diag = function(obs, prev){
  library(ROCR)
  pred = prediction(prev, obs)
  perf = performance(pred,"auc")
  ROC=attr(perf,'y.values')
  return(ROC)
}

install.packages("caret")
library(caret)

fitControl <- trainControl(## 10-fold CV
  method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

set.seed(4322)
ptm <- proc.time()
fit <- train(as.formula(formula), data = data.train,
                 method = "glm",
                 trControl = fitControl,
                 metric = "ROC")
proc.time() - ptm

#usuário   sistema decorrido 
#18.64      0.02     18.69

fit

#Generalized Linear Model #
#
#3000 samples
#57 predictor
#2 classes: 'NotSpam', 'Spam' #
#
#No pre-processing
#Resampling: Cross-Validated (10 fold) #
#
#Summary of sample sizes: 2700, 2700, 2700, 2700, 2700, 2700, ... #
#
#Resampling results
#
#ROC        Sens       Spec       ROC SD       Sens SD     Spec SD   
#0.9686324  0.9428571  0.8788136  0.008807609  0.02216042  0.02826271

diag(data.train[,"V58"], predict(fit, newdata=data.train, type="prob")[,2])
#[1] 0.9779449
diag(data.test[,"V58"], predict(fit, newdata=data.test, type="prob")[,2])
#[1] 0.97379