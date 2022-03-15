
# Calculate all of the summary statistics for tables of publication.
# Kept in the "dplyr::" since the model creation packages eliminate "select" and other functions.

library(dplyr)
library(readxl)
library(writexl)

dataCVD <- read_excel(here::here("../data/CVD_datasets.xlsx"), sheet = "All")
train_CVD <- read_excel(here::here("../data/CVD_datasets.xlsx"), sheet = "Train")
test_CVD <- read_excel(here::here("../data/CVD_datasets.xlsx"), sheet = "Test")

###############################################################################
# Descriptive Characteristics.
###############################################################################

var_int <- c("ageattest", "height_SI", "weight_SI", "BMI", "max_rer", "max_hr",
             "vo2_ml_kg_min", "silva_CRF_pred", "new_CRF_pred")

# For the CVD categories (the other is for CVD diagnoses).
# freq_int <- c("sex_numeric", "TM_pos", "CABG_pos", "PCI_pos", "MI_pos", "HF_pos")
freq_int <- c("sex_numeric", "TM_pos", "CABG", "PCI", "MI", "HeartFailure")

###############################################################################
# This creates summary comparing Train and Test groups.
summary_data <- data.frame(Variables= c(var_int,freq_int))

# Means ± SD plus t-tests.
for(i in 1:length(var_int)){
  # Create the datasets from the test sample to look at the specific CVD categories. 
  temp_all <- dataCVD
  temp_train <- train_CVD
  temp_test <- test_CVD
  
  # T-tests between train and test groups.
  summary_data[i, "train_test_ttest"] <- round(t.test(temp_train[[var_int[i]]],
                                                      temp_test[[var_int[i]]],
                                                      paired=F, var.equal = F)$p.value, 4)
  
  # Means and SD for each group.
  if(var_int[i] == "max_rer"){
    dec_pt <- "%.2f"
  } else {
    dec_pt <- "%.1f"
  }
  
  summary_data[i, "All"] <- paste(sprintf(dec_pt, round(mean(temp_all[[var_int[i]]], na.rm=T),2)), "±", 
                                  sprintf(dec_pt, round(sd(temp_all[[var_int[i]]], na.rm=T), 2)))
  summary_data[i, "Train"] <- paste(sprintf(dec_pt, round(mean(temp_train[[var_int[i]]], na.rm=T),2)), "±", 
                                    sprintf(dec_pt, round(sd(temp_train[[var_int[i]]], na.rm=T), 2)))
  sig_sym <- ifelse(summary_data[i, "train_test_ttest"] < 0.05, "*", "")
  summary_data[i, "Test"] <- str_trim(paste(sprintf(dec_pt, round(mean(temp_test[[var_int[i]]], na.rm=T),2)), 
                                            "±", sprintf(dec_pt, round(sd(temp_test[[var_int[i]]], na.rm=T), 2)),
                                            sig_sym))
  
  # Min-Max for each group.
  summary_data[i, "All range"] <- paste(sprintf(dec_pt, min(temp_all[[var_int[i]]], na.rm = T)), "-", 
                                        sprintf(dec_pt, max(temp_all[[var_int[i]]], na.rm = T)))
  summary_data[i, "Train range"] <- paste(sprintf(dec_pt, min(temp_train[[var_int[i]]], na.rm = T)), "-", 
                                          sprintf(dec_pt, max(temp_train[[var_int[i]]], na.rm = T)))
  summary_data[i, "Test range"] <- paste(sprintf(dec_pt, min(temp_test[[var_int[i]]], na.rm = T)), "-", 
                                         sprintf(dec_pt, max(temp_test[[var_int[i]]], na.rm = T)))
  
  
  # Sample size for each group.
  summary_data[i, "All n"] <- prettyNum(length(which(!is.na(temp_all[var_int[i]]))), big.mark = ",",
                                        scientific=F)
  summary_data[i, "Train n"] <- prettyNum(length(which(!is.na(temp_train[var_int[i]]))), big.mark = ",",
                                          scientific=F)
  summary_data[i, "Test n"] <- prettyNum(length(which(!is.na(temp_test[var_int[i]]))), big.mark = ",",
                                         scientific=F)
  
  rm(temp_all, temp_train, temp_test, sig_sym)
}
summary_data <- dplyr::select(summary_data, "Variables", "All":"Test", "train_test_ttest", "All range":"Test n")

# Frequencies plus chi-square tests.
for(i in 1:length(freq_int)){
  # Create the datasets from the test sample to look at the specific CVD categories. 
  temp_all <- dataCVD
  temp_train <- train_CVD
  temp_test <- test_CVD
  
  # Chi-square tests between train and test groups.
  temp_chi_df <- rbind(data.frame(temp_var = temp_train[[freq_int[i]]], group="Train"), 
                       data.frame(temp_var = temp_test[[freq_int[i]]], group="Test"))
  temp_tbl <- table(temp_chi_df$group, temp_chi_df$temp_var)
  summary_data[i+length(var_int), "train_test_chi"] <- round(chisq.test(temp_tbl)$p.value, 4)
  
  # Summary percentages of individuals.
  summary_data[i+length(var_int), "All"] <-
    paste(prettyNum(sum(temp_all[[freq_int[i]]], na.rm = T), big.mark = ",", scientific = F), " (",
          sprintf("%.0f", round(sum(temp_all[[freq_int[i]]], na.rm = T) / 
                                  sum(complete.cases(temp_all[[freq_int[i]]]))*100, digits = 0)),
          ")", sep="") 
  summary_data[i+length(var_int), "Train"] <-
    paste(prettyNum(sum(temp_train[[freq_int[i]]], na.rm = T), big.mark = ",", scientific = F), " (",
          sprintf("%.0f", round(sum(temp_train[[freq_int[i]]], na.rm = T) / 
                                  sum(complete.cases(temp_train[[freq_int[i]]]))*100, digits = 0)),
          ")", sep="") 
  
  sig_sym <- ifelse(summary_data[i+length(var_int), "train_test_chi"] < 0.05, "*", "")
  summary_data[i+length(var_int), "Test"] <-
    str_trim(paste(prettyNum(sum(temp_test[[freq_int[i]]], na.rm = T), big.mark = ",", scientific = F), " (",
                   sprintf("%.0f", round(sum(temp_test[[freq_int[i]]], na.rm = T) / 
                                           sum(complete.cases(temp_test[[freq_int[i]]]))*100, digits = 0)),
                   ") ", sig_sym, sep=""))
  
  rm(temp_all, temp_train, temp_test, temp_chi_df)
}

# Add in meaning of the significance symbols.
summary_data[nrow(summary_data) +2, "Test"] <- "* significantly different from training dataset (P<0.05)"

###############################################################################
# This creates summary comparing Men and Women.
summary_data_m_f <- data.frame(Variables= c(var_int,freq_int))
# Means ± SD plus t-tests.
for(i in 1:length(var_int)){
  # Create the datasets. 
  temp_all <- dataCVD
  temp_m <- filter(dataCVD, Gender == "Male")
  temp_f <- filter(dataCVD, Gender == "Female")
  
  # T-tests between train and test groups.
  summary_data_m_f[i, "m_f_ttest"] <- round(t.test(temp_m[[var_int[i]]],
                                                   temp_f[[var_int[i]]],
                                                   paired=F, var.equal = F)$p.value, 4)
  
  # Means and SD for each group.
  if(var_int[i] == "max_rer"){
    dec_pt <- "%.2f"
  } else {
    dec_pt <- "%.1f"
  }
  
  summary_data_m_f[i, "All"] <- paste(sprintf(dec_pt, round(mean(temp_all[[var_int[i]]], na.rm=T),2)), "±", 
                                      sprintf(dec_pt, round(sd(temp_all[[var_int[i]]], na.rm=T), 2)))
  summary_data_m_f[i, "Male"] <- paste(sprintf(dec_pt, round(mean(temp_m[[var_int[i]]], na.rm=T),2)), "±", 
                                       sprintf(dec_pt, round(sd(temp_m[[var_int[i]]], na.rm=T), 2)))
  sig_sym <- ifelse(summary_data_m_f[i, "m_f_ttest"] < 0.05, "*", "")
  summary_data_m_f[i, "Female"] <- str_trim(paste(sprintf(dec_pt, round(mean(temp_f[[var_int[i]]], na.rm=T),2)), 
                                                  "±", sprintf(dec_pt, round(sd(temp_f[[var_int[i]]], na.rm=T), 2)),
                                                  sig_sym))
  
  # Min-Max for each group.
  summary_data_m_f[i, "All range"] <- paste(sprintf(dec_pt, min(temp_all[[var_int[i]]], na.rm = T)), "-", 
                                            sprintf(dec_pt, max(temp_all[[var_int[i]]], na.rm = T)))
  summary_data_m_f[i, "Male range"] <- paste(sprintf(dec_pt, min(temp_m[[var_int[i]]], na.rm = T)), "-", 
                                             sprintf(dec_pt, max(temp_m[[var_int[i]]], na.rm = T)))
  summary_data_m_f[i, "Female range"] <- paste(sprintf(dec_pt, min(temp_f[[var_int[i]]], na.rm = T)), "-", 
                                               sprintf(dec_pt, max(temp_f[[var_int[i]]], na.rm = T)))
  
  # Sample size for each group.
  summary_data_m_f[i, "All n"] <- prettyNum(length(which(!is.na(temp_all[var_int[i]]))), big.mark = ",",
                                            scientific=F)
  summary_data_m_f[i, "Male n"] <- prettyNum(length(which(!is.na(temp_m[var_int[i]]))), big.mark = ",",
                                             scientific=F)
  summary_data_m_f[i, "Female n"] <- prettyNum(length(which(!is.na(temp_f[var_int[i]]))), big.mark = ",",
                                               scientific=F)
  
  rm(temp_all, temp_m, temp_f, sig_sym)
}
summary_data_m_f <- dplyr::select(summary_data_m_f, "Variables", "All":"Female n", "m_f_ttest")

# Frequencies plus chi-square tests.
for(i in 1:length(freq_int)){
  # Create the datasets. 
  temp_all <- dataCVD
  temp_m <- filter(dataCVD, Gender == "Male")
  temp_f <- filter(dataCVD, Gender == "Female")
  
  # Chi-square tests between train and test groups.
  temp_chi_df <- rbind(data.frame(temp_var = temp_m[[freq_int[i]]], group="Male"), 
                       data.frame(temp_var = temp_f[[freq_int[i]]], group="Female"))
  temp_tbl <- table(temp_chi_df$group, temp_chi_df$temp_var)
  summary_data_m_f[i+length(var_int), "m_f_chi"] <- round(chisq.test(temp_tbl)$p.value, 4)
  
  # Summary percentages of individuals.
  summary_data_m_f[i+length(var_int), "All"] <-
    paste(prettyNum(sum(temp_all[[freq_int[i]]], na.rm = T), big.mark = ",", scientific = F), " (",
          sprintf("%.0f", round(sum(temp_all[[freq_int[i]]], na.rm = T) / 
                                  sum(complete.cases(temp_all[[freq_int[i]]]))*100, digits = 0)),
          ")", sep="") 
  summary_data_m_f[i+length(var_int), "Male"] <-
    paste(prettyNum(sum(temp_m[[freq_int[i]]], na.rm = T), big.mark = ",", scientific = F), " (",
          sprintf("%.0f", round(sum(temp_m[[freq_int[i]]], na.rm = T) / 
                                  sum(complete.cases(temp_m[[freq_int[i]]]))*100, digits = 0)),
          ")", sep="") 
  
  sig_sym <- ifelse(summary_data_m_f[i+length(var_int), "m_f_chi"] < 0.05, "*", "")
  summary_data_m_f[i+length(var_int), "Female"] <-
    str_trim(paste(prettyNum(sum(temp_f[[freq_int[i]]], na.rm = T), big.mark = ",", scientific = F), " (",
                   sprintf("%.0f", round(sum(temp_f[[freq_int[i]]], na.rm = T) / 
                                           sum(complete.cases(temp_f[[freq_int[i]]]))*100, digits = 0)),
                   ") ", sig_sym, sep=""))
  
  rm(temp_all, temp_m, temp_f, temp_chi_df)
}

# Add in meaning of the significance symbols.
summary_data_m_f[nrow(summary_data_m_f) +2, "Female"] <- "* significantly different from males (P<0.05)"

###############################################################################
# Ethnicity data.
table(dataCVD$ethnicgroup)

print(paste("Have ethnicity: ", sprintf("%.0f", sum(!is.na(dataCVD$ethnicgroup))/nrow(dataCVD)*100), "%", sep=""))
print(paste("White ethnicity: ", sprintf("%.0f", 
                                         sum(dataCVD$ethnicgroup == "White, not of Hispanic origin", na.rm = T)/
                                           sum(!is.na(dataCVD$ethnicgroup))*100), "%", sep=""))


table(dataCVD$Gender)
print(paste("TM tests in Males:", sum(dataCVD$Mode == "TM" & dataCVD$Gender == "Male")))
print(paste("CY tests in Males:", sum(dataCVD$Mode == "CY" & dataCVD$Gender == "Male")))
print(paste("TM tests in Females:", sum(dataCVD$Mode == "TM" & dataCVD$Gender == "Female")))
print(paste("CY tests in Females:", sum(dataCVD$Mode == "CY" & dataCVD$Gender == "Female")))
mean(dataCVD$ageattest)
sd(dataCVD$ageattest)
table(dataCVD$Mode)
table(dataCVD$Facility)
min(dataCVD$testdate, na.rm = T)
max(dataCVD$testdate, na.rm = T)
print(paste("Percentage that are TM tests:", round(sum(dataCVD$Mode == "TM")/NROW(dataCVD)*100,0)))


###############################################################################
# Save the output.
###############################################################################

y <- list("Summary_m_f" = summary_data_m_f,
          "Summary_train_test" = summary_data)

write_xlsx(y, path = here::here("../data/FRIEND_CVD_Metrics_3_15_22_.xlsx"), col_names = T)



