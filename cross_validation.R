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

ctreeGrid <-  expand.grid(.maxdepth=c(10,15,20))

set.seed(4322)
ptm <- proc.time()
fit <- train(as.formula(formula), data = data.train,
                 method = "ctree2",
                 trControl = fitControl,
                 metric = "ROC",
                 tuneGrid = ctreeGrid)
proc.time() - ptm

#usuário   sistema decorrido 
#81.47      0.04     82.42 

fit

#Conditional Inference Tree 

#3000 samples
#57 predictor
#2 classes: 'NotSpam', 'Spam' 

#No pre-processing
#Resampling: Cross-Validated (10 fold) 

#Summary of sample sizes: 2700, 2700, 2699, 2700, 2701, 2700, ... 

#Resampling results across tuning parameters:
  
#  maxdepth  ROC        Sens       Spec       ROC SD      Sens SD     Spec SD   
#10        0.9462662  0.9272356  0.8636732  0.01309512  0.01990168  0.03562211
#15        0.9508955  0.9245241  0.8662669  0.01222702  0.02296164  0.03661791
#20        0.9495368  0.9234371  0.8671289  0.01239722  0.02435236  0.03660957

#ROC was used to select the optimal model using  the largest value.
#The final value used for the model was maxdepth = 15. 

diag(data.train[,"V58"], predict(fit, newdata=data.train, type="prob")[,2])
#[1] 0.9553383
diag(data.test[,"V58"], predict(fit, newdata=data.test, type="prob")[,2])
#[1] 0.9292543