#install.packages("caret")
#install.packages("AppliedPredictiveModeling")
#install.packages("e1071")
#install.packages("corrplot")
library(caret)
library(ggplot2)
library(tidyr)
library(dplyr)
library(AppliedPredictiveModeling)
library(e1071)

data("segmentationOriginal")
segmentationOriginal %>% str()

segData = segmentationOriginal[segmentationOriginal$Case == "Train",]
cellID = segData$Cell
class = segData$Class
case = segData$Case
segData = segData[,-(1:3)]

statusCols = grep(pattern = "Status", x = colnames(segData))
segData = segData[,-statusCols]

# Transform and normalize data
AreaCh1_trans = BoxCoxTrans(segData$AreaCh1)
AreaCh1_trans

head(segData$AreaCh1)
predict(AreaCh1_trans, head(segData$AreaCh1))

pcaObject = prcomp(x = segData, center = T, scale. = T)
summary(pcaObject)
names(pcaObject)
pctVariance = pcaObject$sdev^2/sum(pcaObject$sdev^2)*100
head(round(pctVariance,1))


trans = preProcess(x = segData, 
                   method = c("BoxCox", "center", "scale", "pca"))


transformed = predict(trans, segData)
head(transformed[1:5,1:5])


# Study correlation and remove highly correlated predictors
library(corrplot)
cor_matrix = cor(segData)
corrplot(corr = cor_matrix, order = "hclust")

high_corr = findCorrelation(x = cor_matrix, cutoff = 0.75)
high_corr
segData = segData[, - high_corr]
