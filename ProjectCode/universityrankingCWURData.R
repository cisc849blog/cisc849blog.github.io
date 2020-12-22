library(mgcv)
library(car)
library(nnet)
library(glmnet)
library(MASS)
library(moments)

install.packages("moments")
install.packages("mgcv")
# ######################### Data Preprocessing #####################

replaceMissingValues <- function(dataVar) {
  dataVar = ifelse(is.na(dataVar),ave(dataVar, FUN = function(x) mean(x, na.rm = 'TRUE')),dataVar)
}

getscore <- function(data, index, datavar) {
  datavar = index$datavar-data$datavar+1
}

cwurdata <- read.csv("/Users/sri/Downloads/11. CISC849-013/Final Project/Data/cwurDataFinal1.csv")
cwurrankminmax <- read.csv("/Users/sri/Downloads/11. CISC849-013/Final Project/Data/cwurMaxMinValues.csv")

cwurdata$national_rank	<- cwurrankminmax$national_rank - cwurdata$national_rank + 1
cwurdata$quality_of_education	<- cwurrankminmax$quality_of_education - cwurdata$quality_of_education + 1
cwurdata$alumni_employment	<- cwurrankminmax$alumni_employment - cwurdata$alumni_employment + 1
cwurdata$quality_of_faculty	<- cwurrankminmax$quality_of_faculty - cwurdata$quality_of_faculty + 1
cwurdata$publications	<- cwurrankminmax$publications - cwurdata$publications + 1
cwurdata$influence <- cwurrankminmax$influence - cwurdata$influence + 1
cwurdata$citations <- cwurrankminmax$citations - cwurdata$citations + 1
cwurdata$broad_impact	<- cwurrankminmax$broad_impact - cwurdata$broad_impact + 1
cwurdata$patents <- cwurrankminmax$patents - cwurdata$patents + 1

# ############################### Train Test Split #########################
set.seed(123)
train_index <- sample(1:nrow(cwurdata), 0.8 * nrow(cwurdata))
test_index <- setdiff(1:nrow(cwurdata), train_index)

X_train <- cwurdata[train_index, ]
y_train <- cwurdata[train_index, "score"]

X_test <- cwurdata[test_index, ]
y_test <- cwurdata[test_index, "score"]

cwurfit <- lm(score ~ national_rank+quality_of_education+alumni_employment+quality_of_faculty
              +publications+influence+citations+broad_impact+patents, data=X_train)

# ####################### Stepwise regression model ##########################
cwurstep <- stepAIC(cwurfit, direction = "both", 
                        trace = FALSE)


summary(cwurfit)
summary(cwurstep)

plot(cwurstep)


confint(cwurstep) # 95% CI for the coefficients
exp(coef(cwurstep)) # exponentiated coefficients
exp(confint(cwurstep)) # 95% CI for exponentiated coefficients
pred_scores_train <- round(predict(cwurstep, X_train),2) # predicted values
pred_scores_test <- round(predict(cwurstep, X_test),2)

table(pred_scores_test, X_test$score)
mean(as.character(pred_scores_test) != as.character(X_test$score))
residuals(cwurstep, type="deviance") # residuals

pred1 <- predict(cwurstep, newdata = X_test)
rmse <- sqrt(sum((pred1 - y_test)^2)/length(y_test))
c(RMSE = rmse, R2=summary(cwurstep)$r.squared)

par(mfrow=c(1,1))
plot(y_test, pred1)

summary(cwurstep)

plot(cwurdata$quality_of_education)
skewness(cwurdata$quality_of_education)
