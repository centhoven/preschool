library(haven)
library(psych)
library(labelled)
library(arsenal)
library(VIM)
library(car)
library(mice)
library(dplyr)

path<- "O:\\medewerkers\\227021 Enthoven, C.A\\Postdoc\\Papers\\Preschool"
setwd("O:\\medewerkers\\227021 Enthoven, C.A\\Postdoc\\Papers\\Preschool") 

data<-read_sav(paste(path,"Data_Chinouk2.sav",sep="/"))
data <- remove_labels(data, user_na_to_na = TRUE)

# Create CITO_final variable
data <- data %>%
  mutate(CITO_final = ifelse(!is.na(CITOtest), CITOtest, F0200293_cleaned_CITOscore))

# Create CITO_final_age variable
data <- data %>%
  mutate(CITO_final_age = ifelse(!is.na(Age_cito), Age_cito, AgeChildCito_GR1093),
         CITO_final_age = ifelse(CITO_final_age < 8, NA, CITO_final_age))

# fill in maternal education data from later information
data <- data %>% 
  mutate(educm = coalesce(EDUCM, EDUCM3, EDUCM5))

# Recode SES variable
data <- data %>%
  mutate(SES = case_when(
    is.na(educm) ~ NA,
    educm %in% 0:3 ~ 0,
    educm %in% 4:5 ~ 1
  ))

# Perform regression to compute residuals (discrepancy variable)
reg_model <- lm(CITO_final ~ WISC13_FSIQ, data = data, na.action = na.exclude)
data <- data %>%
  mutate(Discrepancy = resid(reg_model))

# Recode Ethnicity variable into Dutch/Non-Dutch
data$Ethnicity <- as.factor(ifelse(data$ETHNINFv2==1, 0, ifelse(is.na(data$ETHNINFv2), NA, 1)))

# Recode Income variable
data <- data %>%
  mutate(Income_cat = case_when(
    INCOME %in% 1:10 ~ 0,
    INCOME %in% 11:12 ~ 1
  ))

# Recode marital status variable
data <- data %>%
  mutate(Mar_stat = case_when(
    MAR_ST2 %in% 1:2 ~ 1,
    MAR_ST2 == 3 ~ 0
  ))

data <- data[!is.na(data$OUTCOMECHILD) & data$OUTCOMECHILD==1,] #N=9745 live births

df<-data[,c("SES", "WISC13_FSIQ", "CITO_final", "Discrepancy", 
            "AGECHILD13", "CITO_final_age", "GENDER", "Ethnicity", "Income_cat", "Mar_stat", 
            "G1100332", "G1100532", "G1100632", "F0400365", "F0400465", "F0400565",
            "GESTBIR", "PARITY", "BMI_1", "BMI_P", "AGE_M_v2", "AGE_P_v2", "APGAR5")] # add auxiliary variables

df1 <- df[!is.na(df$SES),] #N=8998
df2 <- df1[c(!is.na(df1$G1100332) | !is.na(df1$G1100532) | !is.na(df1$G1100632) | !is.na(df1$F0400365) | !is.na(df1$F0400465) | !is.na(df1$F0400565)),] #N=5447 with 1 preschool variable
df3 <- df2[!is.na(df2$WISC13_FSIQ),] #N=3516 with IQ variable
df4 <- df3[!is.na(df3$CITO_final),] #N=2294 with cito variable
df5 <- df4[!is.na(df4$G1100332) & !is.na(df4$G1100532) & !is.na(df4$G1100632) & !is.na(df4$F0400365) & !is.na(df4$F0400465) & !is.na(df4$F0400565),] # N=1091 complete cases

# pMiss <- function(x){sum(is.na(x))/length(x)*100}
# apply(df,2,pMiss)

df <- df %>%
  mutate(
    sens1 = ifelse(!is.na(SES) & !is.na(Discrepancy) & (!is.na(G1100332)| !is.na(G1100532) | !is.na(G1100632) | !is.na(F0400365) | !is.na(F0400465) | !is.na(F0400565)),1,0), #N=2215
    sens2 = ifelse(!is.na(SES) & !is.na(Discrepancy) & !is.na(G1100332) & !is.na(G1100532) & !is.na(G1100632) & !is.na(F0400365) & !is.na(F0400465) & !is.na(F0400565),1,0), #N=1062
    SES = as.factor(SES),
    GENDER = as.factor(GENDER),
    Ethnicity = as.factor(Ethnicity),
    Income_cat = as.factor(Income_cat),
    Mar_stat = as.factor(Mar_stat),
    G1100332 = as.factor(G1100332),
    G1100532 = as.factor(G1100532),
    G1100632 = as.factor(G1100632),
    F0400365 = as.factor(F0400365),
    F0400465 = as.factor(F0400465),
    F0400565 = as.factor(F0400565),
    PARITY = as.factor(PARITY)
  )

####################################################################################################################
############################################# MULTIPLE IMPUTATION ##################################################
####################################################################################################################

## Show complete cases
df_complete <-df[complete.cases(df),]
dim(df_complete) #N=755

# save(df, file="df.RData")
# load("df.RData")

# Impute data
imputed.data <- mice(df, m = 30, maxit = 100, seed = 999)
summary(imputed.data)

ds <- complete(imputed.data,1)

########### SAVE FILE ################
# saveRDS(imputed.data, "imputed.data.rds")

# ########### OPEN FILE #################
# imputed.data <- readRDS("imputed.data.rds")

long <- complete(imputed.data, action='long', include=T)

# Compute Preschool_2j in days/week (minus 1)
long$Preschool_2jsum <- (as.numeric(long$G1100332) -1) + (as.numeric(long$G1100532)-1) + (as.numeric(long$G1100632)-1)
describe(long$Preschool_2jsum)
long$Preschool_2j <- ifelse(long$Preschool_2jsum > 1, 1, ifelse(long$Preschool_2jsum < 2, 0, NA)) # 2 or more days
prop.table(table(long$Preschool_2j)) # 69.8%

# Compute Preschool_3j in days/week (minus 1)
long$Preschool_3jsum <- (as.numeric(long$F0400365)-1) + (as.numeric(long$F0400465)-1) + (as.numeric(long$F0400565)-1)
long$Preschool_3j <- ifelse(long$Preschool_3jsum > 1, 1, ifelse(long$Preschool_3jsum < 2, 0, NA)) # 2 or more days
prop.table(table(long$Preschool_3j)) #75.7%

# Compute Preschool
long$Preschool <- ifelse(long$Preschool_2j==1 & long$Preschool_3j==1, 1, 0) # 0=no, 1=yes
prop.table(table(long$Preschool)) #61.9%
imputed.data2 <- as.mids(long)

####################################################################################################################
#############################################       TABLE 1       ##################################################
####################################################################################################################
# imp1 <- complete(imputed.data2)
# imp1 <- imp1[imp1$sens1==1,]

# Table one with imputed data stratified by preschool y/n
long <- complete(imputed.data2, action = "long")
df_table1 <- long[long$sens1==1,]

table(df_table1$Preschool) # no: 18915/30=630.5  yes: 49905/30=1663.5
prop.table(table(df_table1$Preschool)) # no: 27.5%  yes:72.5%

# Set the variables and control options
my_controls <- list(test = F, total = T, numeric.stats = "meansd", cat.stats = "countpct", ordered.stats = "countpct", digits = 2)

# Generate the table using tableby analytic cohort for preschool yes/no
table_one_preschool <- tableby(Preschool ~ SES + Mar_stat + GENDER + Ethnicity + Income_cat + 
                          AGECHILD13 + WISC13_FSIQ + CITO_final_age + CITO_final + Discrepancy, 
                         data = df_table1, control = my_controls)

summary(table_one_preschool)

# Save the table to a Word document
write2word(summary(table_one_preschool, pfootnote=TRUE), "table1_preschool.docx")


# Characteristics tables of non-imputed data
df_table1 <- df
df_table1$childminder2j <- as.factor(ifelse(is.na(df_table1$G1100332), NA, ifelse(df_table1$G1100332==1,0,1)))
df_table1$creche2j <- as.factor(ifelse(is.na(df_table1$G1100532), NA, ifelse(df_table1$G1100532==1,0,1)))
df_table1$playgroup2j <- as.factor(ifelse(is.na(df_table1$G1100632), NA, ifelse(df_table1$G1100632==1,0,1)))
df_table1$childminder3j <- as.factor(ifelse(is.na(df_table1$F0400565), NA, ifelse(df_table1$F0400565==1,0,1)))
df_table1$creche3j <- as.factor(ifelse(is.na(df_table1$F0400465), NA, ifelse(df_table1$F0400465==1,0,1)))
df_table1$playgroup3j <- as.factor(ifelse(is.na(df_table1$F0400365), NA, ifelse(df_table1$F0400365==1,0,1)))

df_table1 <- df_table1 %>%
  mutate(Preschool_2jsum = rowSums(across(c(G1100332, G1100532, G1100632), ~ as.numeric(.x) - 1), na.rm = FALSE),
         Preschool_2jdays = ifelse(Preschool_2jsum > 5, 5, Preschool_2jsum),
         Preschool_3jsum = rowSums(across(c(F0400565, F0400465, F0400365), ~ as.numeric(.x) - 1), na.rm = FALSE),
         Preschool_3jdays = ifelse(Preschool_3jsum > 5, 5, Preschool_3jsum),
         Preschool = case_when(is.na(Preschool_2jdays) | is.na(Preschool_3jdays) ~ NA_real_, Preschool_2jdays > 1 & Preschool_3jdays > 1 ~ 1, TRUE ~ 0))

# Set the variables and control options
my_controls <- list(test = F, total = T, numeric.stats = c("meansd", "Nmiss"), cat.stats = c("countpct", "Nmiss"), ordered.stats = c("countpct", "Nmiss"), digits = 2)

# Generate a general characteristics table using tableby full cohort
table_full <- tableby( ~ SES + Mar_stat + GENDER + Ethnicity + Income_cat + as.factor(Preschool) +
                       AGECHILD13 + WISC13_FSIQ + CITO_final_age + CITO_final + Discrepancy, 
                       data = df_table1, control = my_controls)

# Save the table to a Word document
write2word(table_full, "table1_fullcohort.docx")

# Generate the table using tableby analytic cohort
df_table1_analytical_sample <- df_table1[df_table1$sens1==1,]

table_one <- tableby( ~ SES + Mar_stat + GENDER + Ethnicity + Income_cat + as.factor(Preschool) +
                         AGECHILD13 + WISC13_FSIQ + CITO_final_age + CITO_final + Discrepancy, 
                      data = df_table1_analytical_sample, control = my_controls)

# Save the table to a Word document
write2word(table_one, "table1_analyticcohort.docx")

# Generate the table using tableby for complete case cohort
df_table1_complete_sample <- df_table1[df_table1$sens2==1,] #N=1091

table_one <- tableby( ~ SES + Mar_stat + GENDER + Ethnicity + Income_cat + as.factor(Preschool) +
                        AGECHILD13 + WISC13_FSIQ + CITO_final_age + CITO_final + Discrepancy, 
                      data = df_table1_complete_sample, control = my_controls)

# Save the table to a Word document
write2word(table_one, "table1_complete_cases.docx")


########################################################################################################################
##                     Hypothetical intervention and interaction testing with imputed data                            ##
########################################################################################################################

library(mice)
library(dplyr)
library(purrr)

#------------------------------------------------------------
# Function to run model and extract Preschool coefficients
#------------------------------------------------------------

extract_terms <- function(data,
                          outcome,
                          covars,
                          interaction = FALSE,
                          cohort,
                          model_type) {
  
  # Build formula
  if (interaction) {
    formula_text <- paste0(
      outcome,
      " ~ Preschool * SES + ",
      paste(covars, collapse = " + ")
    )
  } else {
    formula_text <- paste0(
      outcome,
      " ~ Preschool + SES + ",
      paste(covars, collapse = " + ")
    )
  }
  
  # Fit model on all imputed datasets
  fit <- with(
    data,
    lm(as.formula(formula_text))
  )
  
  # Pool estimates across imputations
  res <- summary(
    pool(fit),
    conf.int = TRUE
  )
  
  # Keep only Preschool and Preschool:SES terms
  res %>%
    filter(grepl("^Preschool", term)) %>%
    mutate(
      Cohort = cohort,
      Outcome = outcome,
      Model = model_type
    ) %>%
    select(
      Cohort,
      Outcome,
      Model,
      Term = term,
      Beta = estimate,
      Lower95CI = `2.5 %`,
      Upper95CI = `97.5 %`,
      Pvalue = p.value
    )
}

#------------------------------------------------------------
# Outcomes and outcome-specific covariates
#------------------------------------------------------------

models <- list(
  
  WISC13_FSIQ = c(
    "AGECHILD13",
    "GENDER",
    "Ethnicity",
    "Income_cat",
    "Mar_stat"
  ),
  
  CITO_final = c(
    "CITO_final_age",
    "GENDER",
    "Ethnicity",
    "Income_cat",
    "Mar_stat"
  ),
  
  Discrepancy = c(
    "AGECHILD13",
    "CITO_final_age",
    "GENDER",
    "Ethnicity",
    "Income_cat",
    "Mar_stat"
  )
)

#------------------------------------------------------------
# Cohorts
#------------------------------------------------------------

datasets <- list(
  Analytic = filter(imputed.data2, sens1 == 1),
  Full     = imputed.data2,
  Sens2    = filter(imputed.data2, sens2 == 1)
)

#------------------------------------------------------------
# Run all models
#------------------------------------------------------------

results <- bind_rows(
  
  # Main effects
  imap_dfr(
    datasets,
    function(dat, cohort_name) {
      map_dfr(
        names(models),
        function(outcome) {
          extract_terms(
            data = dat,
            outcome = outcome,
            covars = models[[outcome]],
            interaction = FALSE,
            cohort = cohort_name,
            model_type = "Main Effect")})}),
  
  # Interaction models
  imap_dfr(
    datasets,
    function(dat, cohort_name) {
      map_dfr(
        names(models),
        function(outcome) {
          extract_terms(
            data = dat,
            outcome = outcome,
            covars = models[[outcome]],
            interaction = TRUE,
            cohort = cohort_name,
            model_type = "Interaction")})}))
          
#------------------------------------------------------------
# Create formatted CI column
#------------------------------------------------------------

results <- results %>%
  mutate(
    CI = sprintf(
      "%.2f (%.2f; %.2f)",
      Beta,
      Lower95CI,
      Upper95CI))

#------------------------------------------------------------
# Export results
#------------------------------------------------------------

write.csv(results, "Preschool_intervention.csv", row.names = FALSE)

# View results
results


################################################################################################
############# HYPOTHETICAL INTERVENTION ON ALL OUTCOMES AND AMONG ALL DATAFILES ################
################################################################################################

library(boot)
library(broom)
library(dplyr)
library(purrr)
library(tidyr)
library(mice)

#----------------------------------------------------------
# Terms to extract
#----------------------------------------------------------

term_names <- c(
  "pre_0 vs 1",
  "post_0 vs 1",
  "diff_post_0 vs 1",
  "ratio_post_0 vs 1")

#----------------------------------------------------------
# Function to average estimates across imputations
#----------------------------------------------------------

calculate_mean <- function(output_list, term, measure) {
    vals <- sapply(output_list, function(x) {
    row_match <- x$term == term
    if (any(row_match)) {as.data.frame(x)[row_match, measure]} 
    else { NA }})
  mean(unlist(vals), na.rm = TRUE) * -1}

#----------------------------------------------------------
# Define outcomes and model covariates
#----------------------------------------------------------

outcomes <- list(
  WISC13_FSIQ = list(
    covars = c(
      "AGECHILD13",
      "SES",
      "GENDER",
      "Ethnicity",
      "Income_cat",
      "Mar_stat"),
    adjustment = c(
      "AGECHILD13",
      "GENDER")),
  CITO_final = list(
    covars = c(
      "CITO_final_age",
      "SES",
      "GENDER",
      "Ethnicity",
      "Income_cat",
      "Mar_stat"),
    adjustment = c(
      "CITO_final_age",
      "GENDER")),
  Discrepancy = list(
    covars = c(
      "AGECHILD13",
      "CITO_final_age",
      "SES",
      "GENDER",
      "Ethnicity",
      "Income_cat",
      "Mar_stat"),
    adjustment = c(
      "AGECHILD13",
      "CITO_final_age",
      "GENDER")))

#----------------------------------------------------------
# Define cohorts
#----------------------------------------------------------

datasets <- list(
  Analytic = filter(imputed.data2, sens1 == 1),
  Full     = imputed.data2,
  Sens2    = filter(imputed.data2, sens2 == 1)
)

#----------------------------------------------------------
# Function running one outcome within one cohort
#----------------------------------------------------------

run_ineq_model <- function(data_mids,
                           outcome,
                           covars,
                           adjustment,
                           cohort,
                           m = 30,
                           nboot = 1000) { 
  
  output_list <- lapply(seq_len(m), function(imp) {
    
    ds <- complete(data_mids, imp)
    
    boot_out <- boot(
      data = ds,
      statistic = ineq_fun,
      R = nboot,
      outcome = outcome,
      mediator = "Preschool",
      ineq_group = "SES",
      covars = covars,
      interaction = TRUE,
      set_mediator_to = list("Preschool==1"),
      params = list(
        adjustment = adjustment,
        ineq_group = "SES",
        outcome = outcome))
    tidy(
      boot_out,
      conf.int = TRUE)})

  data.frame(
    Cohort = cohort,
    Outcome = outcome,
    Term = term_names,
    Estimate = sapply(
      term_names,
      function(x) calculate_mean(output_list, x, "statistic")),
    Lower95CI = sapply(
      term_names,
      function(x) calculate_mean(output_list, x, "conf.high")),
    Upper95CI = sapply(
      term_names,
      function(x) calculate_mean(output_list, x, "conf.low")))}


#----------------------------------------------------------
# Run all analyses
#----------------------------------------------------------

results_long <- imap_dfr(
  datasets,
  function(dat, cohort_name) {
    map_dfr(
      names(outcomes),
      function(outcome_name) {
        run_ineq_model(
          data_mids = dat,
          outcome = outcome_name,
          covars = outcomes[[outcome_name]]$covars,
          adjustment = outcomes[[outcome_name]]$adjustment,
          cohort = cohort_name)})})  

#----------------------------------------------------------
# Create formatted estimate (95% CI)
#----------------------------------------------------------

results_long <- results_long %>%
  mutate(
    Estimate_CI = sprintf(
      "%.2f (%.2f, %.2f)",
      Estimate,
      Lower95CI,
      Upper95CI
    )
  )

#----------------------------------------------------------
# Convert to publication-ready wide table
#----------------------------------------------------------

results_wide <- results_long %>%
  mutate(
    Term = recode(
      Term,
      "pre_0 vs 1" = "Pre",
      "post_0 vs 1" = "Post",
      "diff_post_0 vs 1" = "Diff",
      "ratio_post_0 vs 1" = "Ratio"
    )
  ) %>%
  select(
    Cohort,
    Outcome,
    Term,
    Estimate_CI
  ) %>%
  pivot_wider(
    names_from = Term,
    values_from = Estimate_CI
  ) %>%
  mutate(
    Cohort = factor(
      Cohort,
      levels = c(
        "Analytic",
        "Full",
        "Sens2"
      )
    ),
    Outcome = factor(
      Outcome,
      levels = c(
        "WISC13_FSIQ",
        "CITO_final",
        "Discrepancy"
      )
    )
  ) %>%
  arrange(
    Cohort,
    Outcome
  )

#----------------------------------------------------------
# View results
#----------------------------------------------------------

print(results_wide)

#----------------------------------------------------------
# Export
#----------------------------------------------------------

write.csv(
  results_wide,
  "Hypothetical_intervention_results.csv",
  row.names = FALSE
)

