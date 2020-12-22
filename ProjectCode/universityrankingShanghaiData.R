library(mgcv)
library(car)
library(nnet)
library(glmnet)
library(MASS)

# ######################### Data Preprocessing #####################

replaceMissingValues <- function(dataVar) {
  dataVar = ifelse(is.na(dataVar),ave(dataVar, FUN = function(x) mean(x, na.rm = 'TRUE')),dataVar)
}

shanghaidata <- read.csv("/Users/sri/Downloads/11. CISC849-013/Final Project/Data/shanghaiDataFinal.csv")

# ############################### Train Test Split #########################
set.seed(123)
train_index <- sample(1:nrow(shanghaidata), 0.8 * nrow(shanghaidata))
test_index <- setdiff(1:nrow(shanghaidata), train_index)

X_train <- shanghaidata[train_index, ]
y_train <- shanghaidata[train_index, "total_score"]

X_test <- shanghaidata[test_index, ]
y_test <- shanghaidata[test_index, "total_score"]

shanghaifit <- lm(total_score ~ alumni+award+hici+ns+pub+pcp,data=X_train)

# ####################### Stepwise regression model ##########################
shanghaistep <- stepAIC(shanghaifit, direction = "both", 
                     trace = FALSE)

summary(shanghaifit)
summary(shanghaistep)

plot(shanghaistep)


confint(shanghaistep) # 95% CI for the coefficients
exp(coef(shanghaistep)) # exponentiated coefficients
exp(confint(shanghaistep)) # 95% CI for exponentiated coefficients
pred_scores_train <- round(predict(shanghaistep, X_train),2) # predicted values
pred_scores_test <- round(predict(shanghaistep, X_test),2)

table(pred_scores_test, X_test$total_score)
mean(as.character(pred_scores_test) != as.character(X_test$total_score))
residuals(shanghaistep, type="deviance") # residuals

pred1 <- predict(shanghaistep, newdata = X_test)
rmse <- sqrt(sum((pred1 - y_test)^2)/length(y_test))
c(RMSE = rmse, R2=summary(shanghaistep)$r.squared)

par(mfrow=c(1,1))
plot(y_test, pred1)

summary(shanghaistep)




