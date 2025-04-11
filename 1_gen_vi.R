library(caret)
library(effects)

library(readr)
library(dplyr)
library(tidyr)
library(WriteXLS)



source_df <- read.csv("Data for Sakib.csv", stringsAsFactors = FALSE)

print(table(source_df$Species))
source_df$labels <- ifelse(source_df$Species == "WS", 0,
                           ifelse(source_df$Species == "TA", 1, NA))
print(table(source_df$labels))



source_df$GLI <- (2 * source_df$G - source_df$R - source_df$B) / (2 * source_df$G + source_df$R + source_df$B)
source_df$NDVI <- (2.7 * source_df$NIR - source_df$R) / (2.7 * source_df$NIR + source_df$R)
source_df$EVI <- 2.5 * (source_df$NIR - source_df$R) / (source_df$NIR + 6 * source_df$R - 7.5 * source_df$B + 1)
source_df$ExG <- 2 * source_df$G - source_df$R - source_df$B
source_df$ExR <- 1.4 * source_df$R - source_df$G
source_df$ExGR <- source_df$ExG - source_df$ExR
source_df$RGI <- source_df$R / source_df$G
source_df$CIVE <- 0.441 * source_df$R - 0.881 * source_df$G + 0.385 * source_df$B + 18.787
source_df$NGRDI <- (source_df$G - source_df$R) / (source_df$G + source_df$R)
source_df$VARI <- (source_df$G - source_df$R) / (source_df$G + source_df$R - source_df$B)
source_df$NDRE <- (source_df$NIR - source_df$RE) / (source_df$NIR + source_df$RE)
source_df$RECI <- (source_df$NIR / source_df$RE) - 1
source_df$GCI <- (source_df$NIR / source_df$G) - 1
source_df$SR <- source_df$NIR / source_df$R



colnames(source_df)

data_df <- source_df %>%
  select(Species, labels, B, G, R, RE, NIR, GLI, ExG, ExR, ExGR, 
         RGI, CIVE, NGRDI, VARI, NDVI, EVI, NDRE, RECI, GCI, SR, Ht)

write.csv(data_df, "0_WS_1_TA_data.csv", row.names = FALSE)






































