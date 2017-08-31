library(tidyr)
library(dplyr)
library(caret)
library(AppliedPredictiveModeling)

data(twoClassData)

str(predictors)
str(classes)

# Data Splitting
train_rows = createDataPartition(y = classes, p = 0.8, list = F)
head(train_rows)

train_predictors = predictors[train_rows, ]
train_classes = classes[train_rows]

test_predictors = predictors[ -train_rows, ]
test_classes = classes[-train_rows]

str(train_predictors)
str(test_predictors)

# Resampling
## Repeated Samples
set.seed(1)
repeated_splits = createDataPartition(y = classes, times = 3, p=0.80)
repeated_splits %>% str()

## 10-fold
cv_splits = createFolds(y = classes, k = 10, returnTrain = T)
str(cv_splits)
fold1 = cv_splits[[1]]

classes %>% length()
fold1 %>% length()

# Basic Model Building
train_predictors = as.matrix(train_predictors)
knn_fit = knn3(x = train_predictors, y = train_classes, k = 5)
knn_fit

test_predictons = predict(object = knn_fit, newdata = test_predictors, type = "class")
table(test_classes, test_predictons)

# Paramter Tuning
data("GermanCredit")

GermanCredit <- GermanCredit[, -nearZeroVar(GermanCredit)]
GermanCredit$CheckingAccountStatus.lt.0 <- NULL
GermanCredit$SavingsAccountBonds.lt.100 <- NULL
GermanCredit$EmploymentDuration.lt.1 <- NULL
GermanCredit$EmploymentDuration.Unemployed <- NULL
GermanCredit$Personal.Male.Married.Widowed <- NULL
GermanCredit$Property.Unknown <- NULL
GermanCredit$Housing.ForFree <- NULL

set.seed(100)
inTrain <- createDataPartition(GermanCredit$Class, p = .8)[[1]]
GermanCreditTrain <- GermanCredit[ inTrain, ]
GermanCreditTest  <- GermanCredit[-inTrain, ]

set.seed(1056)
svm_fit = train(Class~.,
                data = GermanCreditTest,
                method = "svmRadial",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "repeatedcv", repeats = 5))

svm_fit
plot(svm_fit, scales = list(x = list(log = 2)))

# Between model comparision
set.seed(1056)
logistic_reg = train(Class~.,
                     data = GermanCreditTrain,
                     method = "glm",
                     trControl = trainControl(method = "repeatedcv", repeats = 5))


resamp = resamples(list(SVM = svm_fit, Logistic = logistic_reg))
summary(resamp)
diff(resamp) %>% summary()
