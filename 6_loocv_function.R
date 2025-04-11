
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





#performance metric computation function
####################################################################################
compute_metrics <- function(actual, predicted) {
  cm <- table(Actual = actual, Predicted = predicted)
  TP <- if ("1" %in% rownames(cm) && "1" %in% colnames(cm)) cm["1", "1"] else 0
  TN <- if ("0" %in% rownames(cm) && "0" %in% colnames(cm)) cm["0", "0"] else 0
  FP <- if ("0" %in% rownames(cm) && "1" %in% colnames(cm)) cm["0", "1"] else 0
  FN <- if ("1" %in% rownames(cm) && "0" %in% colnames(cm)) cm["1", "0"] else 0
  precision <- if ((TP + FP) == 0) 0 else TP / (TP + FP)
  recall <- if ((TP + FN) == 0) 0 else TP / (TP + FN)
  f1 <- if ((precision + recall) == 0) 0 else 2 * precision * recall / (precision + recall)
  accuracy <- mean(actual == predicted)
  list(confusion_matrix = cm, accuracy = accuracy,
       precision = precision, recall = recall, f1 = f1)
}
####################################################################################







#n-1 or loocv function
####################################################################################
loocv_metrics_local <- function(formula_local, file_path_local) {
  print("running cv (k=n-1) for..")
  print(formula_local)
  
  data_df_local <- read.csv(file_path_local, stringsAsFactors = FALSE)
  data_df_local$labels <- factor(data_df_local$labels)
  n_local <- nrow(data_df_local)
  
  all_preds_logit_local <- c()
  all_actual_logit_local <- c()
  all_aic_logit_local <- c()
  all_bic_logit_local <- c()
  all_test_ll_logit_local <- c()  
  all_train_ll_logit_local <- c()  
  
  for (i_local in 1:n_local) {
    train_data_local <- data_df_local[-i_local, ]
    test_data_local <- data_df_local[i_local, , drop = FALSE]
    
    model_logit_local <- glm(formula_local, data = train_data_local,
                             family = binomial(link = "logit"))
    preds_prob_local <- predict(model_logit_local, newdata = test_data_local, type = "response")
    pred_label_local <- ifelse(preds_prob_local >= 0.5, 1, 0)
    
    all_preds_logit_local <- c(all_preds_logit_local, pred_label_local)
    all_actual_logit_local <- c(all_actual_logit_local, as.character(test_data_local$labels))
    all_aic_logit_local <- c(all_aic_logit_local, AIC(model_logit_local))
    all_bic_logit_local <- c(all_bic_logit_local, BIC(model_logit_local))
    
    actual_local <- as.numeric(as.character(test_data_local$labels))
    test_ll_local <- if (actual_local == 1) log(preds_prob_local) else log(1 - preds_prob_local)
    all_test_ll_logit_local <- c(all_test_ll_logit_local, test_ll_local)
    
    train_ll_local <- as.numeric(logLik(model_logit_local))
    all_train_ll_logit_local <- c(all_train_ll_logit_local, train_ll_local)
  }
  
  levels_labels_local <- levels(data_df_local$labels)
  all_preds_logit_local <- factor(all_preds_logit_local, levels = levels_labels_local)
  all_actual_logit_local <- factor(all_actual_logit_local, levels = levels_labels_local)
  
  metrics_local <- compute_metrics(all_actual_logit_local, all_preds_logit_local)
  
  result_local <- list(
    Accuracy = metrics_local$accuracy,
    Precision = metrics_local$precision,
    Recall = metrics_local$recall,
    F1_Score = metrics_local$f1,
    Mean_AIC = mean(all_aic_logit_local),
    Mean_BIC = mean(all_bic_logit_local),
    Mean_Train_LL = mean(all_train_ll_logit_local),
    Mean_Test_LL = mean(all_test_ll_logit_local)
  )
  
  print("run complete!")
  print("cheers!!")
  print("*********************************************")
  
  return(result_local)
}
####################################################################################































































































































































































































































































































































































































































