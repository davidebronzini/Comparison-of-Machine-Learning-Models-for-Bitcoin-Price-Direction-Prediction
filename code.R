library(readxl)
library(tseries)
raw<-read_excel("E:/file importanti/pavia/statistical learning/exam/Dataset1_Cryptos.xlsx")
data<-as.data.frame(raw)
any(is.na(data))
conv<-ts(data = data[,-1],start = c(2016,1), frequency=365)
r<-sapply(data[,-1], function(x)diff(log(x),lag=1))
conv2<-ts(data = r, start = c(2016,1), frequency=365)
ADF<-adf.test(conv2[,1])
ADF2<-adf.test(conv2[,2])
ADF3<-adf.test(conv2[,3])
ADF4<-adf.test(conv2[,4])
ADF5<-adf.test(conv2[,5])
#stationarity of the series confirmed at 0,05 level
btcclass<-ifelse(r[,2]>0,1,0) #mantain numerical format for the neural network method because otherwise the neural network will treat as a multinomial problem with two classes.
r1<-as.data.frame(r)
r1$BTC<-btcclass
r_final<-cbind(data[2:nrow(data),1],r1) #to insert the date vector
train<-r_final[r_final$`data[2:nrow(data), 1]`<="2018-12-31",]
test<-r_final[r_final$`data[2:nrow(data), 1]`>"2018-12-31",]
library(nnet)
library(neuralnet)
set.seed(10)
neuralnetwork<-neuralnet(BTC~ETH+XRP+LTC+XLM,data = train, hidden=c(10),
  act.fct="logistic",
   err.fct='ce',
  linear.output=FALSE,
  lifesign="minimal")
plot(neuralnetwork)
testwithoutdate<-test[,-1]
prediction_nn<-compute(neuralnetwork,testwithoutdate[,-2])
pred1<-prediction_nn$net.result
predictedvalues <- ifelse(pred1 > 0.5, 1, 0) # the treshold affects very much the error
originals <- test$BTC
miss_class_err <- mean(predictedvalues != originals)
conf0<-table(Predicted=predictedvalues, Actual=originals )
conf0
library(pROC)
roc_nn <- roc(test$BTC, as.numeric(pred1))
plot(roc_nn, col = "blue", main = "ROC Curve for Neural Network")
auc_nn <- auc(roc_nn)
auc_nn
r2<-as.data.frame(r)
r2$BTC <- factor(btcclass, levels = c(0, 1)) 
r_final2<-cbind(data[2:nrow(data),1],r2) #to insert the date vector
train2<-r_final2[r_final2$`data[2:nrow(data), 1]`<="2018-12-31",]
test2<-r_final2[r_final2$`data[2:nrow(data), 1]`>"2018-12-31",]
train2<-train2[,-1]
test2<-test2[,-1]
library(randomForest)
set.seed(10)
forest.fit<-randomForest(BTC~ETH+XRP+LTC+XLM, data=train2, mtry=2,ntree=500,importance=TRUE)
forest.fit
prediction_rf<-predict(forest.fit,test2,type="class")
prediction_rf <- as.numeric(as.character(prediction_rf))
conf<-table(Predicted=prediction_rf,Actual=test2$BTC)
conf
importance(forest.fit)
varImpPlot(forest.fit)


logistic.fit<-glm(BTC~ETH+XRP+LTC+XLM, data=train2, family=binomial)
summary(logistic.fit)
prediction_ls<-predict(logistic.fit,test2)
predicted_values2 <- ifelse(prediction_ls > 0.5, 1, 0)
conf2<-table(Predicted=predicted_values2, Actual=test2$BTC)
conf2
roc_ls <- roc(test$BTC, as.numeric(predicted_values2))
plot(roc_ls, col = "blue", main = "ROC Curve for Logistic regression")
auc_ls <- auc(roc_ls)
auc_ls

logistic.fit<-glm(BTC~LTC+XLM, data=train2, family=binomial)
summary(logistic.fit)
prediction_ls<-predict(logistic.fit,test2)
predicted_values2 <- ifelse(prediction_ls > 0.5, 1, 0)
conf2<-table(Predicted=predicted_values2, Actual=test2$BTC)
conf2

par(mfrow=c(2,2))
hist(r2$ETH)
hist(r2$XRP)
hist(r2$LTC)
hist(r2$XLM)
summary(train2[, c("ETH", "XRP", "LTC", "XLM")])
variances <- sapply(train2[, c("ETH", "XRP", "LTC", "XLM")], var, na.rm = TRUE)
print(variances)
library(rstanarm)
bayesian_model <- stan_glm(BTC ~ ETH + XRP + LTC + XLM, 
                           data = train2, 
                           family = binomial(), 
                           prior = normal(0, 1),  # Normal prior for coefficients
                           prior_intercept = normal(0, 5),  # Normal prior for intercept
                           chains = 4, 
                           iter = 2000, 
                           warmup = 1000, 
                           seed = 10)

bayesian_predictions <- posterior_predict(bayesian_model, newdata = test2)
predicted_probs <- apply(bayesian_predictions, 2, mean) 
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

conf_bayesian <- table(Predicted = predicted_classes, Actual = test2$BTC)
conf_bayesian
summary(bayesian_model)
plot(bayesian_model)
pp_check(bayesian_model)
par(mfrow=c(1,1))
roc_by <- roc(test$BTC, as.numeric(predicted_classes))
plot(roc_by, col = "blue", main = "ROC Curve for Bayesian MCMC")
auc_by <- auc(roc_by)
auc_by
prior_summary(bayesian_model)


library(ggplot2)
logistic_error <- 1 - (sum(diag(conf2)) / sum(conf2))
nn_error <- 1 - (sum(diag(conf0)) / sum(conf0))
bayesian_error <- 1 - (sum(diag(conf_bayesian)) / sum(conf_bayesian))
rf_error <- 1 - (sum(diag(conf)) / sum(conf))

errors_df <- data.frame(
  Method = rep(c("Logistic", "Neural Networks", "Bayesian","Random Forest"), each = 1),
  Error = c(logistic_error, nn_error, bayesian_error,rf_error)
)


ggplot(errors_df, aes(x = Method, y = Error)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Box Plot of Test Errors by Method",
       x = "Method",
       y = "Test Error") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


