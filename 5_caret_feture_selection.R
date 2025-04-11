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






#data loading - removing string col
###################################################################################
data_df <- read.csv("0_WS_1_TA_data.csv", stringsAsFactors = FALSE)
data_df$labels <- factor(data_df$labels)
data_clean <- data_df %>% dplyr::select(-Species)


#feature defining
###################################################################################
feature_cols <- c("B", "G", "R", "RE", "NIR", "GLI", "ExG", "ExR", 
                  "ExGR", "RGI", "CIVE", "NGRDI", "VARI", "NDVI", "EVI",
                  "NDRE", "RECI", "GCI", "SR", "Ht")


#setting up caret feature - label
###################################################################################
X <- data_clean[, feature_cols]
y <- data_clean$labels



#set up caret object params
###################################################################################
ctrl <- rfeControl(functions = lrFuncs,
                   method = "cv",          
                   number = 5,
                   verbose = FALSE)

sizes <- seq(1, ncol(X), by = 1)
if(max(sizes) < ncol(X)) sizes <- c(sizes, ncol(X)) 



#run caret object for feature selection
###################################################################################
set.seed(42)
rfe_results <- rfe(x = X, 
                   y = y,
                   sizes = sizes,
                   rfeControl = ctrl,
                   method = "glm", 
                   family = binomial(link = "logit"))



#plot accuracy
###################################################################################
dev.new()
plot(rfe_results, type = c("g", "o"))



# Print accuracy log
###################################################################################
print(rfe_results)



# Print ranked features
###################################################################################
final_vars <- rfe_results$optVariables
print(final_vars)


















































