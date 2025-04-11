
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




# function for running five fold cross validation test for a binomial model with logit function
# input: 
# takes formula, 
# file path (other dataset may not work, not tested), 
# number of fold is by default 5 and can be varried (not recommended to run n-1, will fail for ROC-AUC)
####################################################################################
def.run.binomial.5.fold.cv <- function(formula_local, file_path_local, k_local = 5) {
  data_df_local <- read.csv(file_path_local, stringsAsFactors = FALSE)
  data_df_local$labels <- factor(data_df_local$labels)
  folds_local <- createFolds(data_df_local$labels, k = k_local, list = TRUE)
  
  accuracy_vec_local   <- numeric(k_local)
  precision_vec_local  <- numeric(k_local)
  recall_vec_local     <- numeric(k_local)
  f1_vec_local         <- numeric(k_local)
  aic_vec_local        <- numeric(k_local)
  bic_vec_local        <- numeric(k_local)
  train_ll_vec_local   <- numeric(k_local)
  test_ll_vec_local    <- numeric(k_local)
  auc_vec_local        <- numeric(k_local)
  roc_list_local       <- list()
  all_preds_local      <- c()
  all_actual_local     <- c()
  
  for (i_local in 1:k_local) {
    test_idx_local <- folds_local[[i_local]]
    train_data_local <- data_df_local[-test_idx_local, ]
    test_data_local  <- data_df_local[test_idx_local, ]
    
    model_local <- glm(formula_local, data = train_data_local, family = binomial(link = "logit"))
    
    train_ll_local <- as.numeric(logLik(model_local))
    train_ll_vec_local[i_local] <- train_ll_local
    
    aic_vec_local[i_local] <- AIC(model_local)
    bic_vec_local[i_local] <- BIC(model_local)
    
    pred_probs_local <- predict(model_local, newdata = test_data_local, type = "response")
    pred_class_local <- ifelse(pred_probs_local >= 0.5, 1, 0)
    
    actual_numeric_local <- as.numeric(as.character(test_data_local$labels))
    fold_test_ll_local <- sum(ifelse(actual_numeric_local == 1, log(pred_probs_local), log(1 - pred_probs_local)))
    test_ll_vec_local[i_local] <- fold_test_ll_local
    
    pred_factor_local <- factor(pred_class_local, levels = levels(data_df_local$labels))
    actual_factor_local <- test_data_local$labels
    
    all_preds_local <- c(all_preds_local, as.character(pred_factor_local))
    all_actual_local <- c(all_actual_local, as.character(actual_factor_local))
    
    cm_local <- table(Actual = actual_factor_local, Predicted = pred_factor_local)
    TP_local <- if ("1" %in% rownames(cm_local) & "1" %in% colnames(cm_local)) cm_local["1", "1"] else 0
    TN_local <- if ("0" %in% rownames(cm_local) & "0" %in% colnames(cm_local)) cm_local["0", "0"] else 0
    FP_local <- if ("0" %in% rownames(cm_local) & "1" %in% colnames(cm_local)) cm_local["0", "1"] else 0
    FN_local <- if ("1" %in% rownames(cm_local) & "0" %in% colnames(cm_local)) cm_local["1", "0"] else 0
    
    fold_accuracy_local <- mean(pred_factor_local == actual_factor_local)
    accuracy_vec_local[i_local] <- fold_accuracy_local
    
    fold_precision_local <- if ((TP_local + FP_local) > 0) TP_local / (TP_local + FP_local) else NA
    fold_recall_local <- if ((TP_local + FN_local) > 0) TP_local / (TP_local + FN_local) else NA
    fold_f1_local <- if (is.na(fold_precision_local) || is.na(fold_recall_local) || (fold_precision_local + fold_recall_local) == 0) {
      NA
    } else {
      2 * fold_precision_local * fold_recall_local / (fold_precision_local + fold_recall_local)
    }
    
    precision_vec_local[i_local] <- fold_precision_local
    recall_vec_local[i_local] <- fold_recall_local
    f1_vec_local[i_local] <- fold_f1_local
    
    roc_obj_local <- roc(actual_factor_local, pred_probs_local, levels = levels(data_df_local$labels))
    auc_val_local <- auc(roc_obj_local)
    auc_vec_local[i_local] <- auc_val_local
    roc_list_local[[i_local]] <- roc_obj_local
    
    cat("Fold", i_local, ":\n")
    cat(sprintf("Accuracy: %.4f, Precision: %.4f, Recall: %.4f, F1: %.4f\n",
                fold_accuracy_local, fold_precision_local, fold_recall_local, fold_f1_local))
    cat(sprintf("AIC: %.4f, BIC: %.4f\n", aic_vec_local[i_local], bic_vec_local[i_local]))
    cat(sprintf("Train LogLik: %.4f, Test LogLik: %.4f, AUC: %.4f\n\n",
                train_ll_vec_local[i_local], test_ll_vec_local[i_local], auc_val_local))
  }
  
  cat("Average Metrics over", k_local, "folds:\n")
  cat(sprintf("Accuracy: %.4f\n", mean(accuracy_vec_local)))
  cat(sprintf("Precision: %.4f\n", mean(precision_vec_local, na.rm = TRUE)))
  cat(sprintf("Recall: %.4f\n", mean(recall_vec_local, na.rm = TRUE)))
  cat(sprintf("F1 Score: %.4f\n", mean(f1_vec_local, na.rm = TRUE)))
  cat(sprintf("Mean AIC: %.4f\n", mean(aic_vec_local)))
  cat(sprintf("Mean BIC: %.4f\n", mean(bic_vec_local)))
  cat(sprintf("Mean Train Log-Likelihood: %.4f\n", mean(train_ll_vec_local)))
  cat(sprintf("Mean Test Log-Likelihood: %.4f\n", mean(test_ll_vec_local)))
  cat(sprintf("Mean AUC: %.4f\n", mean(auc_vec_local)))
  
  all_preds_factor_local <- factor(all_preds_local, levels = levels(data_df_local$labels))
  all_actual_factor_local <- factor(all_actual_local, levels = levels(data_df_local$labels))
  
  list(all_preds_factor_local = all_preds_factor_local, 
       all_actual_factor_local = all_actual_factor_local, 
       accuracy_vec_local = accuracy_vec_local, 
       precision_vec_local = precision_vec_local, 
       recall_vec_local = recall_vec_local, 
       f1_vec_local = f1_vec_local, 
       aic_vec_local = aic_vec_local, 
       bic_vec_local = bic_vec_local, 
       train_ll_vec_local = train_ll_vec_local, 
       test_ll_vec_local = test_ll_vec_local, 
       auc_vec_local = auc_vec_local, 
       roc_list_local = roc_list_local)
}
####################################################################################




####################################################################################
plot_confusion_local <- function(all_preds_factor_local, all_actual_factor_local) {
  cm_local <- table(Actual = all_actual_factor_local, Predicted = all_preds_factor_local)
  cm_df_local <- as.data.frame(cm_local)
  
  dev.new()
  ggplot(cm_df_local, aes(x = Predicted, y = Actual, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "black", size = 5) +
    scale_fill_gradient(low = "blue", high = "red") +
    theme_minimal() +
    ggtitle("Confusion Matrix")
}
####################################################################################



#function for plotting ROC in the final figure
####################################################################################
plot_roc_auc_local <- function(roc_list_local, auc_vec_local) {
  dev.new()
  colors_local <- rainbow(length(roc_list_local))
  
  fpr_local <- 1 - roc_list_local[[1]]$specificities
  tpr_local <- roc_list_local[[1]]$sensitivities
  plot(fpr_local, tpr_local, type = "l", col = colors_local[1],
       xlab = "False Positive Rate (FPR)",
       ylab = "True Positive Rate (TPR)",
       main = "ROC Curves (TPR vs FPR)",
       xlim = c(0, 1), ylim = c(0, 1))
  
  for (i_local in 2:length(roc_list_local)) {
    fpr_local <- 1 - roc_list_local[[i_local]]$specificities
    tpr_local <- roc_list_local[[i_local]]$sensitivities
    lines(fpr_local, tpr_local, col = colors_local[i_local])
  }
  
  legend("bottomright",
         legend = paste("Fold", 1:length(auc_vec_local), sprintf("(AUC=%.3f)", auc_vec_local)),
         col = colors_local, lwd = 2)
}
####################################################################################




































