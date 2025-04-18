
source("5_loocv_function.R")




#formulas for table 7
####################################################################################
formula_1 <- labels ~ Ht*(VARI+GLI+B+NGRDI+NIR+NDRE+R+RECI+Ht+EVI+SR+GCI+RE)
formula_2 <- labels ~ Ht*(VARI + GLI + B + NGRDI + NIR)
formula_3 <- labels ~ Ht*(B + R + NIR + GLI + NGRDI + VARI + EVI + NDRE + RECI + Ht)
formula_4 <- labels ~ Ht*(NIR + NDVI)
formula_5 <- labels ~ Ht*(NDVI + NIR + RE + Ht)
####################################################################################


####################################################################################
model_names <- c("Caret top 13", "Caret top 5", 
                 "Sig. features from initial model (Sup. Table 1)",
                 "Parsimonious Model", 
                 "Manual Selection (considering R Band, NIR, RE, Height)")
####################################################################################


####################################################################################
features_descriptions <- c("Ht*(VARI+GLI+B+NGRDI+NIR+NDRE+R+RECI+Ht+EVI+SR+GCI+RE)",
                           "Ht*(VARI + GLI + B + NGRDI + NIR)",
                           "Ht*(B + R + NIR + GLI + NGRDI + VARI + EVI + NDRE + RECI + Ht)",
                           "Ht*(NIR + NDVI)",
                           "Ht*(NDVI + NIR + RE + Ht)")
####################################################################################



####################################################################################
formulas_list <- list(formula_1, formula_2, formula_3, formula_4, formula_5)
####################################################################################


####################################################################################
results_list <- lapply(formulas_list, loocv_metrics_local, file_path_local = "0_WS_1_TA_data.csv")
####################################################################################



####################################################################################
results_matrix <- do.call(rbind, lapply(results_list, function(x) {
  c(Accuracy = x$Accuracy, 
    Precision = x$Precision, 
    Recall = x$Recall, 
    F1_Score = x$F1_Score, 
    Mean_AIC = x$Mean_AIC, 
    Mean_BIC = x$Mean_BIC, 
    Mean_Train_LL = x$Mean_Train_LL, 
    Mean_Test_LL = x$Mean_Test_LL)
}))
####################################################################################



#dataframe for table 7
####################################################################################
results_table <- data.frame(Model = model_names, Features = features_descriptions, results_matrix,
                            row.names = NULL, stringsAsFactors = FALSE)
####################################################################################



#print table 7
####################################################################################
print(results_table)
####################################################################################

























































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































