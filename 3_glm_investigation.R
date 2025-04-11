

#generic library setup for the project
####################################################################################
library(caret)
library(effects)

library(readr)
library(dplyr)
library(tidyr)
library(WriteXLS)

library(ggplot2)
library(reshape2)

library(car)
library(glmnet)
####################################################################################


#load data
####################################################################################
data_df <- read.csv("0_WS_1_TA_data.csv", stringsAsFactors = FALSE)
data_df$labels <- factor(data_df$labels)
####################################################################################

####################################################################################
set.seed(42)
#fold creation with caret
folds <- createFolds(data_df$labels, k = 5, list = TRUE)
####################################################################################

#vector for storing acuracy for each fold
####################################################################################
accuracy_logit    <- numeric()  # GLM, binomial (logit)
accuracy_probit   <- numeric()  # GLM, binomial (probit)
accuracy_cloglog  <- numeric()  # GLM, binomial (cloglog)
accuracy_quasibin <- numeric()  # GLM, quasi-binomial (logit)
accuracy_lpm      <- numeric()  # Linear Probability Model (LM)
####################################################################################



#formula
####################################################################################
formula <- labels ~ B + G + R + RE + NIR + GLI + ExG + ExR + ExGR +   RGI + CIVE + NGRDI + VARI + NDVI + EVI + NDRE +   RECI + GCI + SR + Ht
####################################################################################



#k=5 cv loop
####################################################################################
for(i in seq_along(folds)) {
  #train test data for each fold. each fold as test, rest as train
  test_idx   <- folds[[i]]
  train_data <- data_df[-test_idx, ]
  test_data  <- data_df[test_idx, ]
  
  #binomial (logit)
  model_logit <- glm(formula, data = train_data, family = binomial(link = "logit"))
  preds_logit_prob <- predict(model_logit, newdata = test_data, type = "response")
  preds_logit <- ifelse(preds_logit_prob >= 0.5, 1, 0)
  accuracy_logit[i] <- mean(preds_logit == test_data$labels)
  
  #binomial (probit)
  model_probit <- glm(formula, data = train_data, family = binomial(link = "probit"))
  preds_probit_prob <- predict(model_probit, newdata = test_data, type = "response")
  preds_probit <- ifelse(preds_probit_prob >= 0.5, 1, 0)
  accuracy_probit[i] <- mean(preds_probit == test_data$labels)
  
  #binomial (cloglog)
  model_cloglog <- glm(formula, data = train_data, family = binomial(link = "cloglog"))
  preds_cloglog_prob <- predict(model_cloglog, newdata = test_data, type = "response")
  preds_cloglog <- ifelse(preds_cloglog_prob >= 0.5, 1, 0)
  accuracy_cloglog[i] <- mean(preds_cloglog == test_data$labels)
  
  #quasi-binomial (logit) 
  model_quasibin <- glm(formula, data = train_data, family = quasibinomial(link = "logit"))
  preds_quasibin_prob <- predict(model_quasibin, newdata = test_data, type = "response")
  preds_quasibin <- ifelse(preds_quasibin_prob >= 0.5, 1, 0)
  accuracy_quasibin[i] <- mean(preds_quasibin == test_data$labels)
  
  #LM
  model_lpm <- lm(formula, data = train_data)
  preds_lpm <- predict(model_lpm, newdata = test_data)
  preds_lpm <- ifelse(preds_lpm >= 0.5, 1, 0)
  accuracy_lpm[i] <- mean(preds_lpm == test_data$labels)
}
####################################################################################

#print metrics
####################################################################################
cat("avg acc binomial (logit):", mean(accuracy_logit), "\n")
cat("avg acc binomial (probit):", mean(accuracy_probit), "\n")
cat("avg acc binomial (cloglog):", mean(accuracy_cloglog), "\n")
cat("avg acc quasi-binomial (logit):", mean(accuracy_quasibin), "\n")
cat("avg acc LM:", mean(accuracy_lpm), "\n")
####################################################################################






























