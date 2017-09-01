library(caret)
library(tidyr)
library(dplyr)
library(AppliedPredictiveModeling)

data(solubility)
ls(pattern = "sol")

set.seed(2)
sample(names(solTrainX), 10)

# Ordinary Least Squares
trainigData = solTrainXtrans
trainigData$Solubility = solTrainY

lmfit_allpredictors = lm(Solubility~., data = trainigData)
summary(lmfit_allpredictors)
lmpred1 = predict(lmfit_allpredictors, newdata = solTestXtrans)
head(lmpred1)

lmvalues1 = data.frame(obs=solTestY, pred = lmpred1)
lmvalues1 %>% head()

defaultSummary(lmvalues1)

# Robust linear regression
library(MASS)
rlmfit_allpredictors = rlm(Solubility~., data = trainigData)

cntrl = trainControl(method = "cv", number = 10)

set.seed(100)
lmfit1 = train(x = solTrainXtrans, y = solTrainY, 
               method = "lm", trControl = cntrl)

lmfit1
plot(x = solTrainY, y = predict(lmfit1), ylab = "Predicted", xlab = "Actual")
plot(x = residuals(lmfit1), y = fitted.values(lmfit1), xlab = "Residuals", ylab = "Fitted Values")

# Regression without Collinearity
cor_threshold = 0.9
high_cor = findCorrelation(x = cor(solTrainXtrans), cutoff = cor_threshold)
corr_pred = names(solTrainXtrans)[high_cor]

trainXFiltered = solTrainXtrans[,-high_cor]
testXFiltered = solTestXtrans[, -high_cor]

set.seed(100)
lmFiltered = train(x = trainXFiltered, y = solTrainY, 
                   method = "lm", trControl = cntrl)

lmFiltered

# Partial Least Squares
library(pls)

plsFit = plsr(Solubility~., data = trainigData)
predict(plsFit, newdata = solTestXtrans[1:5, ], ncomp = 1:3)

set.seed(100)
plsTune = train(x = solTrainXtrans, y = solTrainY, 
                method = "pls", tuneLength = 20, 
                trControl = cntrl, 
                preProcess = c("center", "scale"))

plsTune

# Penalized Regression Models
ridge_grid = data.frame(.lambda = seq(0, 0.1, length.out = 15))
ridge_fit = train(x = solTrainXtrans, y = solTrainY, 
                  method = "ridge", 
                  tuneGrid = ridge_grid, 
                  preProcess = c("center", "scale"), 
                  trControl = cntrl)
ridge_fit
plot(ridge_fit)

enet_grid = expand.grid(.lambda = c(0,0.01, 0.1),
                        .fraction = seq(0.05, 1, length.out = 20))

enet_fit = train(x = solTrainXtrans, y = solTrainY, 
                 method = "enet",
                 tuneGrid = enet_grid, 
                 trControl = cntrl,
                 preProcess = c("center", "scale"))

enet_fit
plot(enet_fit)
