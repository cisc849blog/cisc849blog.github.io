library(mgcv)
library(car)
library(nnet)
library(glmnet)
library(MASS)

# ######################### Data Preprocessing #####################

replaceMissingValues <- function(dataVar) {
  dataVar = ifelse(is.na(dataVar),ave(dataVar, FUN = function(x) mean(x, na.rm = 'TRUE')),dataVar)
}

timesdata <- read.csv("/Users/sri/Downloads/11. CISC849-013/Final Project/Data/timesDataFinal.csv")

timesdata$teaching <- (timesdata$teaching)
timesdata$international <- (round(replaceMissingValues(timesdata$international), 2))
timesdata$research <- (timesdata$research)
timesdata$citations <- (timesdata$citations)
timesdata$income <- (round(replaceMissingValues(timesdata$income), 2))
timesdata$num_students <- scale(round(replaceMissingValues(timesdata$num_students), 2))
timesdata$student_staff_ratio	<- (round(replaceMissingValues(timesdata$student_staff_ratio), 2))
timesdata$international_students <- (round(replaceMissingValues(timesdata$international_students), 2))
timesdata$female_male_proportion <- (round(replaceMissingValues(timesdata$female_male_proportion), 2))

timesdata$total_score	<- (round(replaceMissingValues(timesdata$total_score), 2))

# ############################### Train Test Split #########################
set.seed(123)
train_index <- sample(1:nrow(timesdata), 0.8 * nrow(timesdata))
test_index <- setdiff(1:nrow(timesdata), train_index)

X_train <- timesdata[train_index, ]
y_train <- timesdata[train_index, "total_score"]

X_test <- timesdata[test_index, ]
y_test <- timesdata[test_index, "total_score"]

timesfit <- lm(total_score ~ teaching+international+research+citations+income+num_students
          +student_staff_ratio+international_students+female_male_proportion,data=X_train)

# ####################### Stepwise regression model ##########################
timesstep <- stepAIC(fit, direction = "both", 
                      trace = FALSE)

summary(timesfit)
summary(timesstep)

plot(timesstep)


confint(timesstep) # 95% CI for the coefficients
exp(coef(timesstep)) # exponentiated coefficients
exp(confint(timesstep)) # 95% CI for exponentiated coefficients
pred_scores_train <- round(predict(timesstep, X_train),2) # predicted values
pred_scores_test <- round(predict(timesstep, X_test),2)

table(pred_scores_test, X_test$total_score)
mean(as.character(pred_scores_test) != as.character(X_test$total_score))
residuals(timesstep, type="deviance") # residuals

pred1 <- predict(timesstep, newdata = X_test)
rmse <- sqrt(sum((pred1 - y_test)^2)/length(y_test))
c(RMSE = rmse, R2=summary(timesstep)$r.squared)

par(mfrow=c(1,1))
plot(y_test, pred1)

summary(timesstep)




