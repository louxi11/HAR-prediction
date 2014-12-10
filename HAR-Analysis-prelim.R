# Preliminaries
setwd("~/github/HAR-prediction/")
library(caret)
set.seed(1337)

## Read data
raw <- read.csv('pml-training.csv', header = T)

## Clean data

# Remove NA columns. All columns do either contain no NA values at all or
# consist mostly of NA values. We restrict our analysis to the columns without
# NA values.
raw <- raw[colSums(is.na(raw)) == 0]

# We also drop the variables X (which just counts the observations), user_name,
# and three timestamp variables.
# These are the first five variables in the original data set.
raw <- raw[-(1:5)]

# From visual inspection, all variables (except classe and user_name) are of
# numeric type. However, some are presented as factor variables.
sapply(raw[-88], is.factor)

# The main reason for the conversion to factor variables is that most of the
# corresponding columns contain no data for about 98% of observations. We
# therefore ignore these columns too. Note that the first variable (new_window)
# and the last variable (classe) stay in the data frame, since they legitimately
# are factor variables.
raw <- raw[c(T, !sapply(raw[-c(1, 88)], is.factor), T)]

## Split into training and cross-validation sets.

trainID <- createDataPartition(raw$classe, p = .7, list = F)
training <- raw[trainID,]
crossval <- raw[-trainID,]

## Preprocess

## Training -- takes about one hour on my 2014 Macbook Pro.
# Use cached result if possible.
if (file.exists('fit_rf.RData')) {
    load('fit_rf.RData')
} else {
    fit.rf <- train(classe ~ ., data = training, method = 'rf')
    save(fit.rf, file = 'fit_rf.RData')
}

## Make and check predictions.
pred.cv <- predict(fit.rf, crossval)
accuracy <- function (x) { sum(x)/length(x) }
oos_error <- accuracy(pred.cv == crossval$classe)

# Accuracy on the cross-validation set (running the full script with
# the seed above) is about 99.8%.

## Read testing data
raw.test <- read.csv('pml-testing.csv', header = T)
pred.test <- predict(fit.rf, raw.test)

# From submitting to the Coursera site, accuracy on the test set is 100%.