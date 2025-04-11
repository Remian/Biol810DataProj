Project Title: Tree Species Classification from Height and Spectral Data
Course: BIOL810

Each script is numbered following the sequence for the analysis presented in the paper. 
1_gen_vi.R generates the feature set which is being investigated. Running this script (with the source csv file "Data for Sakib.csv"), will create the 0_WS_1_TA_data.csv, which is being investigated.

  
2_efv.R is for the exploratory feature visualization steps (+visualizing+kernel density+tree height effect). Running this script will export an excel file containing feature correlations. Feature correlation plot, three types of distribution plots, kernel density plot are visualized. Also it computes (and visualizes) tree height correlation with individual features for each class.

  
3_glm_investigation.R fits all the feature (total 20), for different variants for GLM models, and a lm model. The results are depicted in Table 2, for selecting a GLM model. It uses 5 fold cross validation

  
4_five_fold_binomial_functions.R and 4_five_fold_binomial_functions_caller.R | As a huge number of feature combinations have been tested, in order to simplify the repository the core functions for performing 5 fold cross validation and obtaining different comparison matrices are provided. the function script consist of the core functions, the caller script imports and run cv test for a specific function. Users can validatate the cv results of different formula using the caller function.

  
5_caret_feture_selection.R implements feature selection pipeline from caret package, which is a core investigation in this data analysis project.

  
6_loocv_function.R and 6_loocv_function_call_table_7.R | These scripts are for leave one out cross validation (k=n-1) tests. The function script has the core functions used for computing the matrices and running the cv test. The caller script generates the Table 7 from the project (final results), using the function script.

  
**for installing the required packages plaease run 0_package_installer_script.R
  
**5 fold results may produce slight variation due to randomness in fold creation. That's why leave one out cv is performed, leave one out cv should be consistant. 

  
**0_WS_1_TA_data.csv this file name may vary in some script, and produce error (some script uses file name from earlier version). In that regard the correct file name is: 0_WS_1_TA_data.csv. Please confirm it before running the scripts.
