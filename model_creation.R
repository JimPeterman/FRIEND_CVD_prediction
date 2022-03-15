
# Creating regression equations to predict CRF based on CVD status.
# Have to use "dplyr::" since the model creation packages eliminate "select" and other functions.

library(dplyr)
# library(tidyr)
# library(tibble)
library(readxl)
library(writexl)
# library(FRIENDanalysis)

# Importing as text keep things correct.
data_all <- read_excel(here::here("../data/FRIEND_dataset_with_City_2_23_22.xlsx"), col_types = "text")
# This dataset still includes MET-test data (though it gets filtered out below).

data <- data_all

###############################################################################
# Little Clean up.
###############################################################################

#First convert the n/a and na to actually missing.
cols_num <- c("ANYCVD", "COPD", "CABG", "MI", "PCI", "HeartFailure", "COPD",
              "ageattest", "height", "weight", "vo2_ml_kg_min", "vo2_l_min", 
              "max_hr", "max_rer", "max_load_watts", "BMI")
data <- data %>% mutate_at(.vars = cols_num,
                           .funs = list(~replace(.,.=="n/a"|.=="na"|
                                                   .=="Avg of 3 20 sec values",
                                                 NA)))
# make_num <- c("ageattest", "height", "weight", "vo2_ml_kg_min", "vo2_l_min",
#               "max_rer", "max_hr", "max_load_watts", "BMI")
data$max_hr[data$max_hr == "??"] <- NA
data[cols_num] <- sapply(data[cols_num], as.numeric)

# Create age groups (ignores those <20 and >90 years old).
data$age_group <- ifelse(data$ageattest>=20 & data$ageattest<30, "20s",
                         ifelse(data$ageattest>=30 & data$ageattest<40, "30s",
                                ifelse(data$ageattest>=40 & data$ageattest<50, "40s",
                                       ifelse(data$ageattest>=50 & data$ageattest<60, "50s",
                                              ifelse(data$ageattest>=60 & data$ageattest<70, "60s",
                                                     ifelse(data$ageattest>=70 & data$ageattest<80, "70s",
                                                            ifelse(data$ageattest>=80 & data$ageattest<90, "80s",NA)))))))

###############################################################################
# Remove extraneous values.
data$max_rer[data$max_rer > 3] <- NA

# Folks with height <4ft or >7ft are marked as missing.
data$height[data$height < 48] <- NA
data$height[data$height > 84] <- NA

data$weight[data$weight < 30] <- NA

#Creates columns for height and weight in SI units.
data$height_SI <- round(data$height*2.54,1)
data$weight_SI <- round(data$weight/2.2,1)

data$max_hr[data$max_hr < 30] <- NA
data$max_hr[data$max_hr > 250] <- NA

###############################################################################
# Dropping tests.
###############################################################################

# Drops those with "ANYCVD" missing.
data <- dplyr::filter(data, !is.na(ANYCVD))

# Includes only those 40-89 years old.
data <- dplyr::filter(data, ageattest>=40 & ageattest<90)

# Drops those not coded as M/F (so missing gender coding).
table(data$Gender)
data <- dplyr::filter(data, Gender=="Male" | Gender=="Female")

# Drops those with 0 for height, weight, age, or VO2.
data <- dplyr::filter(data, !(height==0))
data <- dplyr::filter(data, !(weight==0))
data <- dplyr::filter(data, !(ageattest==0))
data <- dplyr::filter(data, !(vo2_ml_kg_min==0))

# Only looks at TM and CY data.
data <- dplyr::filter(data, Mode=="TM" | Mode=="CY")

# Drops those with RER<1.0.
data <- dplyr::filter(data, max_rer>=1.0)

# Only looks at USA and Canada..
data <- dplyr::filter(data, Country=="USA" | Country=="CAN")

# Require complete cases for these key variables.
var_int <- c("vo2_ml_kg_min", "ageattest", "Gender", "height_SI", "weight_SI",
             "CABG", "MI", "PCI", "HeartFailure", "Mode")
data <- data[complete.cases(data[,var_int]),]

# Final filtering:
data <- dplyr::filter(data, HeartFailure==1 | PCI==1 | MI==1 | CABG==1)

# Check outputs to make sure there aren't mistakes.
table(data$Country)
table(data$Mode)
table(data$Gender)
table(data$Facility)

###############################################################################
# Specific to this analysis:

# Function to calculate CRF from de Souza e Silva pub (EJPC, 2018)
silva <- function(age, sex, weight_lbs, height_in, ex_mode){
  result_vec <- vector(mode = "numeric", length = length(sex))
  
  for(i in 1:length(sex)){
    if(sex[i] == "Male"){
      sex_ <- 1
    } else if(sex[i] == "Female"){
      sex_ <- 2
    } else {
      sex_ <- NA
    }
    
    if(ex_mode[i] == "TM"){
      mode_ <- 1
    } else if(ex_mode[i] == "CY"){
      mode_ <- 2
    } else {
      mode_ <- NA
    }
    
    CRF <- 45.2 - (.35*age[i]) - (10.9*sex_) - (.15*weight_lbs[i]) + (.68*height_in[i]) - (.46*mode_)
    result_vec[i] <- (round(CRF, 1))
    
  }
  return(result_vec)
}

# Calculate the "healthy" predicted CRF for cohort.
data <- mutate(data, silva_CRF_pred = 
                 silva(ageattest, Gender, weight, height, Mode))

# Create CVD category counts.
# Based on previous publication (Peterman et al. JAHA 2021).
data <- mutate(data, CABG_pos = ifelse(CABG==1 & PCI==0 & HeartFailure==0, 1, 0))
data <- mutate(data, MI_pos = ifelse(MI==1 & CABG==0 & HeartFailure==0, 1, 0))
data <- mutate(data, PCI_pos = ifelse(PCI==1 & CABG==0 & MI==0 & HeartFailure==0, 1, 0))
data <- mutate(data, HF_pos = ifelse(HeartFailure==1, 1, 0))

# Create mode category counts.
data <- mutate(data, TM_pos = ifelse(Mode == "TM", 1, 0))
data <- mutate(data, CY_pos = ifelse(Mode == "CY", 1, 0))

# Gender coding to 0/1.
data <- mutate(data, sex_numeric = ifelse(Gender == "Male", 1, 0))

###############################################################################
# Creating train/test datasets (80/20).
###############################################################################

# Set seed to reproduce.
set.seed(80303)

temp_df <- mutate(data, new_id = 1:nrow(data))
train_CVD <- temp_df %>% sample_frac(0.8)
test_CVD <- temp_df %>% anti_join(train_CVD, temp_df, by = "new_id")


###############################################################################
# Step-wise regression variable determination.
###############################################################################

library(tidyverse)
library(caret)
library(leaps)
library(MASS)

train_CVD_df <- dplyr::select(train_CVD, vo2_ml_kg_min, ageattest, Gender, height_SI, weight_SI,
                              Mode, CABG, MI, PCI, HeartFailure)

# Using the caret package, this creates multiple models and 
# finds the best variables for each model (1-variable model, 2-variable model, etc.)
# Number of variable models to create is set with nvmax.
# RMSE is used to decide on best model. 
# The bestTune says which model is best (9 variables in this case).
# The * in the summary output indicate best variables for each model.

set.seed(80303)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)

# Train the model
step_model <- train(vo2_ml_kg_min ~., data = train_CVD_df,
                    method = "leapForward", 
                    tuneGrid = data.frame(nvmax = 1:9),
                    trControl = train.control)

step_model$results
step_model$bestTune
summary(step_model$finalModel)
summary(step_model)


###############################################################################
# Create a function ("model performance) that allows for quick analysis of model variables.
###############################################################################

# Create root mean square error function for use in function below.
rmse <- function(act, pred) {
  result <- sqrt(mean((act - pred)^2, na.rm = T))
  return(round(result, 2))
}

# Create the model
# model_test <- lm(vo2_ml_kg_min ~ ageattest + Gender + height_SI + weight_SI +
#                    Mode + CABG + MI + PCI + HeartFailure +
#                    resting_hr + resting_sbp + resting_dbp, data = train_CVD)
model_test <- lm(vo2_ml_kg_min ~ ageattest + Gender + height_SI + weight_SI +
                   Mode + CABG + MI + PCI + HeartFailure, data = train_CVD)

# CVD categories to test the model on.
cvd_cat <- c("CVD", "Male", "Female", "CABG", "PCI", "MI", "HF")

# This function takes a model and prints the features and model performance.
# Then, it tests the model on the CVD categories of interest.
# Also does comparisons with the healthy prediction equation.
model_performance <- function(model_, lst_to_check){
  reg_one_results <- data.frame(matrix(ncol = (length(lst_to_check)+1), nrow = 0))
  colnames(reg_one_results)[1] <- "Model"
  colnames(reg_one_results)[2:length(reg_one_results)] <- lst_to_check
  
  reg_one_results["Features",1] <- ""
  
  for(i in 1:length(summary(model_)$coef[,1])){
    
    estimate <- summary(model_)$coef[i,1]
    p_val <- summary(model_)$coef[i,4]
    var_name <- names(summary(model_)$coef[,1])[i]
    
    sig <- ifelse(p_val < 0.05, "*", "")
    
    reg_one_results[paste(var_name),1] <- paste(sprintf("%.2f", estimate), sig)
    
    if(i == length(summary(model_)$coef[,1])){
      
      # Error of the model.
      # SEE is used for predictions within train group, RMSE is used for predictions in train.
      reg_one_results["Model SEE",1] <- paste(sprintf("%.2f", summary(model_)$sigma))
      
      reg_one_results["Model adj. R2", 1] <- sprintf("%.2f", summary(model_)$adj.r.square)
      
      # Sample Size of training data.
      reg_one_results["Model train n", 1] <- paste(nrow(model.frame(model_))) 
    }
    
  }
  
  reg_one_results["Test Performance",1] <- ""
  
  for(i in 1:length(lst_to_check)){
    # Create the datasets from the test sample to look at the specific CVD categories. 
    test_df <- test_CVD
    
    if(lst_to_check[i] == "CABG"){
      temp_test_df <- dplyr::filter(test_df, CABG_pos==1)

    } else if (lst_to_check[i] == "MI"){
      temp_test_df <- dplyr::filter(test_df, MI_pos==1)
      
    } else if (lst_to_check[i] == "PCI"){
      temp_test_df <- dplyr::filter(test_df, PCI_pos==1)

    } else if (lst_to_check[i] == "HF"){
      
      temp_test_df <- dplyr::filter(test_df, HF_pos==1)
      
    } else {
      temp_test_df <- test_df
    }
    
    
    if(lst_to_check[i] == "Male"){
      temp_test_df <- dplyr::filter(temp_test_df, Gender == "Male")
    } else if (lst_to_check[i] == "Female"){
      temp_test_df <- dplyr::filter(temp_test_df, Gender == "Female")
    }
    
    # RMSE.
    pred <- predict(model_, temp_test_df)
    healthy_pred <- temp_test_df$silva_CRF_pred
    act <- temp_test_df$vo2_ml_kg_min
    reg_one_results["RMSE Silva pred", lst_to_check[i]] <- sprintf("%.2f", rmse(act, healthy_pred))
    reg_one_results["RMSE CVD pred", lst_to_check[i]] <- sprintf("%.2f", rmse(act, pred))
    
    # Means of actual and predicted.
    temp_test <- t.test(pred,act, paired = F)$p.value
    sig_t <- ifelse(temp_test < 0.05, "^", "")
    # Get the results from using a healthy predictino equation (not the CVD-specifc).
    temp_test_healthy <- t.test(healthy_pred, act, paired = F)$p.value
    sig_t_healthy <- ifelse(temp_test_healthy < 0.05, "^", "")
    
    reg_one_results["Actual VO2 (test)", lst_to_check[i]] <- paste(sprintf("%.1f", mean(act, na.rm = T)),
                                                                   "±", sprintf("%.1f", sd(act, na.rm = T)))
    reg_one_results["Silva Predicted VO2 (test)", lst_to_check[i]] <- paste(sprintf("%.1f", mean(healthy_pred, na.rm = T)),
                                                                            "±", sprintf("%.1f", sd(healthy_pred, na.rm = T)),
                                                                            sig_t_healthy)
    reg_one_results["% Silva Predicted VO2 (test)", lst_to_check[i]] <- 
      paste(sprintf("%.0f", mean(healthy_pred, na.rm = T)/mean(act, na.rm = T)*100), "%", sep="")
    
    reg_one_results["Predicted VO2 (test)", lst_to_check[i]] <- paste(sprintf("%.1f", mean(pred, na.rm = T)),
                                                                      "±", sprintf("%.1f", sd(pred, na.rm = T)),
                                                                      sig_t)
    reg_one_results["% Predicted VO2 (test)", lst_to_check[i]] <- 
      paste(sprintf("%.0f", mean(pred, na.rm = T)/mean(act, na.rm = T)*100), "%", sep="")
    # Ranges for the actual and predicted.
    reg_one_results["Actual Range VO2 (test)", lst_to_check[i]] <- paste(sprintf("%.1f", min(act, na.rm = T)), "-",
                                                                         sprintf("%.1f", max(act, na.rm = T)))
    reg_one_results["Silva Predicted Range VO2 (test)", lst_to_check[i]] <- paste(sprintf("%.1f", min(healthy_pred, na.rm = T)), "-",
                                                                         sprintf("%.1f", max(healthy_pred, na.rm = T)))
    reg_one_results["Predicted Range VO2 (test)", lst_to_check[i]] <- paste(sprintf("%.1f", min(pred, na.rm = T)), "-",
                                                                         sprintf("%.1f", max(pred, na.rm = T)))
    
    # Correlation between actual and predicted.
    # Test to compare correlations.
    compare_cor <- cocor::cocor.dep.groups.overlap(cor(healthy_pred, act, use = "complete.obs"),
                                                   cor(pred, act, use = "complete.obs"),
                                                   cor(healthy_pred, pred, use = "complete.obs"),
                                                   n=length(pred))
    sig_dif <- ifelse(cocor::as.htest(compare_cor)$pearson1898$p.value < 0.05, "#", "")
    
    sig_cor_healthy <- ifelse(cor.test(healthy_pred, act)$p.value < 0.05, "†", "")
    reg_one_results["r (act vs Silva_pred) (test)", lst_to_check[i]] <- 
      paste(sprintf("%.2f", cor(healthy_pred, act, use = "complete.obs")),
            sig_cor_healthy, sig_dif)
    
    sig_cor <- ifelse(cor.test(pred, act)$p.value < 0.05, "†", "")
    reg_one_results["r (act vs pred) (test)", lst_to_check[i]] <- 
      paste(sprintf("%.2f", cor(pred, act, use = "complete.obs")),sig_cor)
    
    
    # ICC determinations.
    temp_icc_df_healthy <- data.frame(act, healthy_pred)
    # Oneway since the equations are not "randomly chosen from a population"
    temp_icc <- irr::icc(temp_icc_df_healthy, model = "oneway", type = "consistency", unit = "single",
                         conf.level = 0.95)
    sig_icc <- ifelse(temp_icc$p.value < 0.05, "‡", "")
    reg_one_results["Silva ICC (test)", lst_to_check[i]] <- paste(sprintf("%.2f", round(temp_icc$value, 2)), " (", 
                                                                  sprintf("%.2f", round(temp_icc$lbound, 2)), "-",
                                                                  sprintf("%.2f", round(temp_icc$ubound, 2)), ")", 
                                                                  sig_icc, sep = "")
    temp_icc_df <- data.frame(act, pred)
    # Oneway since the equations are not "randomly chosen from a population"
    temp_icc <- irr::icc(temp_icc_df, model = "oneway", type = "consistency", unit = "single",
                         conf.level = 0.95)
    sig_icc <- ifelse(temp_icc$p.value < 0.05, "‡", "")
    reg_one_results["ICC (test)", lst_to_check[i]] <- paste(sprintf("%.2f", round(temp_icc$value, 2)), " (", 
                                                            sprintf("%.2f", round(temp_icc$lbound, 2)), "-",
                                                            sprintf("%.2f", round(temp_icc$ubound, 2)), ")", 
                                                            sig_icc, sep = "")
    
    
    # Sample Sizes
    reg_one_results["test n", lst_to_check[i]] <- paste(sum(!is.na(pred)))
  }
  
  reg_one_results["", 1] <- NA
  
  reg_one_results["* significant model feature (P<0.05)", 2] <- NA
  reg_one_results["^ significantly different from actual VO2 (P<0.05)", 2] <- NA
  reg_one_results["† significant correlation (P<0.05)", 2] <- NA
  reg_one_results["# significantly diff correlation from CVD prediction", 2] <- NA
  reg_one_results["‡ significant ICC (P<0.05)", 2] <- NA
  
  # Rename columns and make index a column.
  new_cat_names <- paste(cvd_cat, "_test", sep="")
  colnames(reg_one_results)[2:length(reg_one_results)] <- new_cat_names
  reg_one_results <- tibble::rownames_to_column(reg_one_results, "Stats")
  
  return(reg_one_results)
}

# Create table summarizing model performance using the main function.
reg_result_final <- model_performance(model_test, cvd_cat)


###############################################################################
# Save datasets for descriptive stats and visualizations.
###############################################################################

write_xlsx(reg_result_final, path = here::here("../data/FRIEND_CVD_model_results_3_15_2022_.xlsx"), col_names = T)

###############################################################################
# Calculate the CVD-specific CRF for everyone.
###############################################################################

# Created a function to calculate CVD-specific predicted CRF from the selected model.
# Enter the model to use and those coefficients are used.
# Coefficients rounded to 2 decimal places since that's how it'll be presented.
new_eq <- function(model_, age, sex, weight_kg, height_cm, ex_mode, CABG, MI, PCI, HF){
  result_vec <- vector(mode = "numeric", length = length(sex))
  
  for(i in 1:length(sex)){
    if(sex[i] == "Male"){
      sex_ <- 1
    } else if(sex[i] == "Female"){
      sex_ <- 0
    } else {
      sex_ <- NA
    }
    
    if(ex_mode[i] == "TM"){
      mode_ <- 1
    } else if(ex_mode[i] == "CY"){
      mode_ <- 0
    } else {
      mode_ <- NA
    }
    
    # Only addition in the formula since negative coefficients will create subtraction.
    CRF <- round(model_$coefficients["(Intercept)"],2) + (round(model_$coefficients["ageattest"],2)*age[i]) + 
      (round(model_$coefficients["GenderMale"],2)*sex_) + (round(model_$coefficients["height_SI"],2)*height_cm[i]) + 
      (round(model_$coefficients["weight_SI"],2)*weight_kg[i]) + (round(model_$coefficients["ModeTM"],2)*mode_) +
      (round(model_$coefficients["CABG"],2)*CABG[i]) + (round(model_$coefficients["MI"],2)*MI[i]) + 
      (round(model_$coefficients["PCI"],2)*PCI[i]) + (round(model_$coefficients["HeartFailure"],2)*HF[i])
    result_vec[i] <- (round(CRF, 1))
  }
  return(result_vec)
}

data <- mutate(data, new_CRF_pred = 
                    new_eq(model_test,
                           ageattest, Gender, weight_SI, height_SI, Mode, CABG, MI, PCI, HeartFailure))
train_CVD <- mutate(train_CVD, new_CRF_pred = 
                    new_eq(model_test,
                           ageattest, Gender, weight_SI, height_SI, Mode, CABG, MI, PCI, HeartFailure))
test_CVD <- mutate(test_CVD, new_CRF_pred = 
                    new_eq(model_test,
                           ageattest, Gender, weight_SI, height_SI, Mode, CABG, MI, PCI, HeartFailure))


###############################################################################
# Save datasets for descriptive stats and visualizations.
###############################################################################

# Save the columns needed for the future but drop all the others from the datasets.
# Saves space and loads faster.
col_to_keep <- c("Gender", "ageattest", "height_SI", "weight_SI", "BMI", "max_rer", "max_hr",
             "resting_sbp", "resting_dbp", "resting_hr", "peak_rpe", "Mode",
             "Facility", "testdate", "ethnicgroup", 
             "vo2_ml_kg_min", "silva_CRF_pred", "new_CRF_pred",
             "sex_numeric", "TM_pos", "CABG", "PCI", "MI", "HeartFailure",
             "CABG_pos", "PCI_pos", "MI_pos", "HF_pos")

data <- dplyr::select(data, all_of(col_to_keep))
train_CVD <- dplyr::select(train_CVD,  all_of(col_to_keep))
test_CVD <- dplyr::select(test_CVD,  all_of(col_to_keep))

y <- list("All" = data,
          "Train" = train_CVD,
          "Test" = test_CVD)

write_xlsx(y, path = here::here("../data/CVD_datasets.xlsx"), col_names = T)
        
