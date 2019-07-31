#Scope: Trying to predict column asp is 1 or not. 1 means naturally aspirated.

#Libraries:
install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(dplyr)

#Clean out objects and set working directory:
rm(list = ls())

getwd()

#Read the csv file and rename first column:
df <- read.csv("18cars.csv")
colnames(df)[1] = "displ"

#Converting factors into numeric data:
df$asp = as.numeric(df$asp)
df$trans = as.numeric(df$trans)
df$lockup = as.numeric(df$lockup)
df$drive = as.numeric(df$drive)
df$cyldeact = as.numeric(df$cyldeact)
df$vvt = as.numeric(df$vvt)
df$vvl = as.numeric(df$vvl)
df = mutate(df, asp = ifelse(asp>1,0,1))

#Split data into training and testing subsets:
set.seed(123)
n = nrow(df)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
train = df[trainIndex ,]
test = df[-trainIndex ,]

#Machine Learning, model training
install.packages("caretEnsemble")
library(caret)
library(caretEnsemble)
library(randomForest)
library(MLmetrics)
control = trainControl(method="cv", number=10)
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
rfModels = caretList(as.factor(asp)~., data=train, methodList=algorithmList, trControl=control)

#Results
print(rfModels)
results = resamples(rfModels)
summary(results)

#Stack the results from different models and "vote" for outcome
predictAspStacking = predict(rfModels, newdata = test)

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
pa = c()
for (i in 1:nrow(predictAspStacking))
{
  pa[i] = mode(predictAspStacking[i,])
}
accuracy_Stacking = Accuracy(y_pred = pa, y_true = test$asp) 
pa = as.numeric(pa)

#Visualize results of test subset, predicted vs labels
cbind(test,pa)
ggplot(data = test, mapping = aes(x=asp, y=pa)) + geom_point() + geom_jitter(height = 0.3, width = 0.3, alpha = 0.7)