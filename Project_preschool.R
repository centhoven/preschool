library(haven)
library(psych)
library(labelled)
library(arsenal)
library(VIM)
library(car)
library(mice)
library(dplyr)

path<- "O:\\medewerkers\\Andriese_CM_091061"
setwd("O:\\medewerkers\\Andriese_CM_091061\\Hypothetische interventie") 

data<-read_sav(paste(path,"Data_Chinouk2.sav",sep="/"))
data <- remove_labels(data, user_na_to_na = TRUE)

# Create CITO_final variable
data <- data %>%
  mutate(CITO_final = ifelse(!is.na(CITOtest), CITOtest, F0200293_cleaned_CITOscore))

# Create CITO_final_age variable
data <- data %>%
  mutate(CITO_final_age = ifelse(!is.na(Age_cito), Age_cito, AgeChildCito_GR1093),
         CITO_final_age = ifelse(CITO_final_age < 8, NA, CITO_final_age))

# Recode SES variable
data <- data %>%
  mutate(SES = case_when(
    is.na(EDUCM) ~ NA,
    EDUCM %in% 0:3 ~ 0,
    EDUCM %in% 4:5 ~ 1
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

data <- data[!is.na(data$OUTCOMECHILD) & data$OUTCOMECHILD==1,] #N=9745 live births

df<-data[,c("SES", "WISC13_FSIQ", "CITO_final", "Discrepancy", 
            "AGECHILD13", "CITO_final_age", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2", 
            "G1100332", "G1100532", "G1100632", "F0400365", "F0400465", "F0400565",
            "GESTBIR", "PARITY", "BMI_1", "BMI_P", "AGE_M_v2", "AGE_P_v2", "APGAR5")] # add auxiliary variables

# df1 <- df[!is.na(df$SES),] #N=8536 
# df2 <- df1[c(!is.na(df1$G1100332) | !is.na(df1$G1100532) | !is.na(df1$G1100632) | !is.na(df1$F0400365) | !is.na(df1$F0400465) | !is.na(df1$F0400565)),] #N=5232 with 1 preschool variable
# df3 <- df2[!is.na(df2$WISC13_FSIQ),] #N=3395 with IQ variable
# df4 <- df3[!is.na(df3$CITO_final),] #N=2215 with cito variable
# df5 <- df4[!is.na(df4$G1100332) & !is.na(df4$G1100532) & !is.na(df4$G1100632) & !is.na(df4$F0400365) & !is.na(df4$F0400465) & !is.na(df4$F0400565),] # N=1062 complete cases

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
    MAR_ST2 = as.factor(ifelse(MAR_ST2==999, NA, MAR_ST2)),
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
dim(df_complete) #N=751

# save(df, file="df.RData")
# load("df.RData")

# Impute data
imputed.data <- mice(df, m = 30, maxit = 100, seed = 999)
summary(imputed.data)

#ds <- complete(imputed.data,1)

########### SAVE FILE ################
# saveRDS(imputed.data, "imputed.data.rds")

# ########### OPEN FILE #################
# imputed.data <- readRDS("imputed.data.rds")

long <- complete(imputed.data, action='long', include=T)

# Compute Preschool_2j in days/week (minus 1)
long$Preschool_2jsum <- (as.numeric(long$G1100332) -1) + (as.numeric(long$G1100532)-1) + (as.numeric(long$G1100632)-1)
describe(long$Preschool_2jsum)
long$Preschool_2j <- ifelse(long$Preschool_2jsum > 1, 1, ifelse(long$Preschool_2jsum < 2, 0, NA)) # 2 or more days
prop.table(table(long$Preschool_2j)) # 69.5%

# Compute Preschool_3j in days/week (minus 1)
long$Preschool_3jsum <- (as.numeric(long$F0400365)-1) + (as.numeric(long$F0400465)-1) + (as.numeric(long$F0400565)-1)
long$Preschool_3j <- ifelse(long$Preschool_3jsum > 1, 1, ifelse(long$Preschool_3jsum < 2, 0, NA)) # 2 or more days
prop.table(table(long$Preschool_3j)) #75.7%

# Compute Preschool
long$Preschool <- ifelse(long$Preschool_2j==1 & long$Preschool_3j==1, 1, 0) # 0=no, 1=yes
prop.table(table(long$Preschool)) #61.8%
imputed.data2 <- as.mids(long)

####################################################################################################################
#############################################       TABLE 1       ##################################################
####################################################################################################################
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
my_controls <- list(test = TRUE, total = FALSE, numeric.stats = c("meansd", "N"), cat.stats = c("countpct", "N"), ordered.stats = c("countpct", "N"),
                    cat.test="chisq", ordered.test="chisq", numeric.test="kwt", digits = 2)

# Generate a table to check al variables using tableby
check <- tableby( ~ ., data= df_table1, control = my_controls)
summary(check)

# Save the table to a Word document
write2word(check, "check.docx")

# Generate a general characteristics table using tableby full cohort
table_full <- tableby( ~ SES + MAR_ST2 + GENDER + Ethnicity + Income_cat + 
                         creche2j + playgroup2j + childminder2j + creche3j + playgroup3j + childminder3j +
                         as.factor(Preschool_2jdays) + as.factor(Preschool_3jdays) + as.factor(Preschool) +
                       AGECHILD13 + WISC13_FSIQ + CITO_final_age + CITO_final + Discrepancy, 
                       data = df_table1, control = my_controls)

# Save the table to a Word document
write2word(table_full, "table1_fullcohort.docx")

dim(df_table1[!is.na(df_table1$creche2j) | !is.na(df_table1$playgroup2j) | !is.na(df_table1$childminder2j),]) #N=4178
dim(df_table1[!is.na(df_table1$creche3j) | !is.na(df_table1$playgroup3j) | !is.na(df_table1$childminder3j),]) #N=4903

# Generate the table using tableby analytic cohort
df_table1_analytical_sample <- df_table1[df_table1$sens1==1,]
write_sav(df_table1_analytical_sample, "df_table1_analytical_sample.sav")

table(df_table1_analytical_sample$Preschool_2jsum) # N=5 exceeds five days
table(df_table1_analytical_sample$Preschool_3jsum)

table_one <- tableby( ~ SES + MAR_ST2 + GENDER + Ethnicity + Income_cat + 
                        creche2j + playgroup2j + childminder2j + creche3j + playgroup3j + childminder3j +
                        as.factor(Preschool_2jdays) + as.factor(Preschool_3jdays) + as.factor(Preschool) +
                        AGECHILD13 + WISC13_FSIQ + CITO_final_age + CITO_final + Discrepancy, 
                      data = df_table1_analytical_sample, control = my_controls)

# Save the table to a Word document
write2word(summary(table_one, pfootnote=TRUE), "table1.docx")

dim(df_table1_analytical_sample[!is.na(df_table1_analytical_sample$creche2j) | !is.na(df_table1_analytical_sample$playgroup2j) | !is.na(df_table1_analytical_sample$childminder2j),]) #N=1803
dim(df_table1_analytical_sample[!is.na(df_table1_analytical_sample$creche3j) | !is.na(df_table1_analytical_sample$playgroup3j) | !is.na(df_table1_analytical_sample$childminder3j),]) #N=2056

# Generate the table using tableby analytic cohort for high vs low SES
table_one_SES <- tableby(SES ~ MAR_ST2 + GENDER + Ethnicity + Income_cat + 
                        creche2j + playgroup2j + childminder2j + creche3j + playgroup3j + childminder3j +
                          as.factor(Preschool_2jdays) + as.factor(Preschool_3jdays) + as.factor(Preschool) +
                        AGECHILD13 + WISC13_FSIQ + CITO_final_age + CITO_final + Discrepancy, 
                      data = df_table1_analytical_sample, control = my_controls)

# Save the table to a Word document
write2word(summary(table_one_SES, pfootnote=TRUE), "table1_ses.docx")

#high ses
df_table1_analytical_sample <- df_table1[df_table1$sens1==1 & df_table1$SES==1,]
dim(df_table1_analytical_sample[!is.na(df_table1_analytical_sample$creche2j) | !is.na(df_table1_analytical_sample$playgroup2j) | !is.na(df_table1_analytical_sample$childminder2j),]) #N=1271
dim(df_table1_analytical_sample[!is.na(df_table1_analytical_sample$creche3j) | !is.na(df_table1_analytical_sample$playgroup3j) | !is.na(df_table1_analytical_sample$childminder3j),]) #N=1359

#low ses
df_table1_analytical_sample <- df_table1[df_table1$sens1==1 & df_table1$SES==0,]
dim(df_table1_analytical_sample[!is.na(df_table1_analytical_sample$creche2j) | !is.na(df_table1_analytical_sample$playgroup2j) | !is.na(df_table1_analytical_sample$childminder2j),]) #N=532
dim(df_table1_analytical_sample[!is.na(df_table1_analytical_sample$creche3j) | !is.na(df_table1_analytical_sample$playgroup3j) | !is.na(df_table1_analytical_sample$childminder3j),]) #N=697


########################################################################################################################
##                     Hypothetical intervention with imputed data ANALYTIC COHORT                                    ##
########################################################################################################################

imputed.data3 <- filter(imputed.data2, sens1==1)
long <- complete(imputed.data3, action = "long") # N=2215
table(long$Preschool)/30 # n=1609
prop.table(table(long$Preschool)) #72.6%

# IQ
interventionmodel1 <- with(imputed.data3, lm(WISC13_FSIQ ~ Preschool + SES + AGECHILD13 + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_IQ <- summary(pool(interventionmodel1), conf.int = T)
sink("interventie_model_IQ.ses_adj.txt")
interventie_model_IQ
sink()

# Cito
interventionmodel2 <- with(imputed.data3, lm(CITO_final ~ Preschool + SES + CITO_final_age + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_cito <- summary(pool(interventionmodel2), conf.int = T)
sink("interventie_model_cito.ses_adj.txt")
interventie_model_cito
sink()

# IQ-Cito discrepancy
interventionmodel3 <- with(imputed.data3, lm(Discrepancy ~ Preschool + SES + AGECHILD13 + CITO_final_age + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_IQ_cito <- summary(pool(interventionmodel3), conf.int = T)
sink("interventie_model_IQ_cito.ses_adj.txt")
interventie_model_IQ_cito
sink()

########################################################################################################################
##                     Test interaction between preschool and SES  ANALYTIC COHORT                                    ##
########################################################################################################################

# IQ
interventionmodel1 <- with(imputed.data3, lm(WISC13_FSIQ ~ Preschool * SES + AGECHILD13 + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_IQ <- summary(pool(interventionmodel1), conf.int = T)
sink("interventie_model_IQ.ses_interaction_adj.txt")
interventie_model_IQ
sink()

# Cito
interventionmodel2 <- with(imputed.data3, lm(CITO_final ~ Preschool * SES + CITO_final_age + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_cito <- summary(pool(interventionmodel2), conf.int = T)
sink("interventie_model_cito.ses_interaction_adj.txt")
interventie_model_cito
sink()

# IQ-Cito discrepancy
interventionmodel3 <- with(imputed.data3, lm(Discrepancy ~ Preschool * SES + AGECHILD13 + CITO_final_age + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_IQ_cito <- summary(pool(interventionmodel3), conf.int = T)
sink("interventie_model_IQ_cito.ses_interaction_adj.txt")
interventie_model_IQ_cito
sink()


#Step 1: Run function change_inequality.R saved in a different file.
#Step 2: Run function diff_ineq.R saved in a different file.
library(boot)
library(broom)

calculate_mean <- function(output_list, term, measure) {
  term_values <- sapply(output_list, function(x) {
    term_match <- x$term == term
    if (any(term_match)) {
      as.data.frame(x)[term_match, measure]
    } else {
      NA
    }
  })
  mean(unlist(term_values), na.rm = TRUE) * -1
}


## IQ as outcome ##
output_list <- lapply(1:30, function(imp) {
  ds <- complete(imputed.data3, imp)
  
  ineq_fun(dat = ds, inds = 1:nrow(ds),outcome = "WISC13_FSIQ", mediator = "Preschool", 
           ineq_group = "SES", covars = c("AGECHILD13", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
           set_mediator_to = list("Preschool==1"), 
           params=list(adjustment=c("AGECHILD13", "GENDER"),ineq_group="SES",outcome="WISC13_FSIQ"))
  
  boot_out <- boot(data = ds, statistic = ineq_fun, outcome = "WISC13_FSIQ", mediator = "Preschool", 
                   ineq_group = "SES", covars = c("AGECHILD13", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
                   set_mediator_to = list("Preschool==1"), R=1000, #change to 1000
                   params=list(adjustment=c("AGECHILD13", "GENDER"),ineq_group="SES",outcome="WISC13_FSIQ"))
  
  tidy(boot_out, conf.int = TRUE)
})

list_name <- "HI_IQ"

term_names <- c("pre_0 vs 1",
                "post_0 vs 1",
                "diff_post_0 vs 1",
                "ratio_post_0 vs 1")

pooled_result_IQ <- data.frame(
  term = term_names,
  statistic = sapply(term_names, function(term) calculate_mean(output_list, term, "statistic")),
  conf.low = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.high")),
  conf.high = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.low"))
)

write.csv(pooled_result_IQ, file = paste0("pooled.result.", list_name, ".csv"), row.names = FALSE)


## Cito as outcome ##
output_list <- lapply(1:30, function(imp) {
  ds <- complete(imputed.data3, imp)
  
  ineq_fun(dat = ds, inds = 1:nrow(ds),outcome = "CITO_final", mediator = "Preschool", 
           ineq_group = "SES", covars = c("CITO_final_age", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
           set_mediator_to = list("Preschool==1"), 
           params=list(adjustment=c("CITO_final_age", "GENDER"),ineq_group="SES",outcome="CITO_final"))
  
  boot_out <- boot(data = ds, statistic = ineq_fun, outcome = "CITO_final", mediator = "Preschool", 
                   ineq_group = "SES", covars = c("CITO_final_age", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
                   set_mediator_to = list("Preschool==1"), R=1000, #change to 1000
                   params=list(adjustment=c("CITO_final_age", "GENDER"),ineq_group="SES",outcome="CITO_final"))
  
  tidy(boot_out, conf.int = TRUE)
})

list_name <- "HI_cito"

term_names <- c("pre_0 vs 1",
                "post_0 vs 1",
                "diff_post_0 vs 1",
                "ratio_post_0 vs 1")

pooled_result_cito <- data.frame(
  term = term_names,
  statistic = sapply(term_names, function(term) calculate_mean(output_list, term, "statistic")),
  conf.low = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.high")),
  conf.high = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.low"))
)

write.csv(pooled_result_cito, file = paste0("pooled.result.", list_name, ".csv"), row.names = FALSE)

## Discrepancy as outcome ##
output_list <- lapply(1:30, function(imp) {
  ds <- complete(imputed.data3, imp)
  
  ineq_fun(dat = ds, inds = 1:nrow(ds),outcome = "Discrepancy", mediator = "Preschool", 
           ineq_group = "SES", covars = c("AGECHILD13", "CITO_final_age", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
           set_mediator_to = list("Preschool==1"), 
           params=list(adjustment=c("AGECHILD13", "CITO_final_age", "GENDER"),ineq_group="SES",outcome="Discrepancy"))
  
  boot_out <- boot(data = ds, statistic = ineq_fun, outcome = "Discrepancy", mediator = "Preschool", 
                   ineq_group = "SES", covars = c("AGECHILD13","CITO_final_age", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
                   set_mediator_to = list("Preschool==1"), R=1000, #change to 1000
                   params=list(adjustment=c("AGECHILD13", "CITO_final_age", "GENDER"),ineq_group="SES",outcome="Discrepancy"))
  
  tidy(boot_out, conf.int = TRUE)
})

list_name <- "HI_IQ-cito"

term_names <- c("pre_0 vs 1",
                "post_0 vs 1",
                "diff_post_0 vs 1",
                "ratio_post_0 vs 1")

pooled_result_IQ_cito <- data.frame(
  term = term_names,
  statistic = sapply(term_names, function(term) calculate_mean(output_list, term, "statistic")),
  conf.low = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.high")),
  conf.high = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.low"))
)

write.csv(pooled_result_IQ_cito, file = paste0("pooled.result.", list_name, ".csv"), row.names = FALSE)


########################################################################################################################
##                         Hypothetical intervention with imputed data FULL COHORT                                    ##
########################################################################################################################

# Intervention models #
interventionmodel1 <- with(imputed.data2, lm(WISC13_FSIQ ~ Preschool + SES + AGECHILD13 + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_IQ <- summary(pool(interventionmodel1), conf.int = T)
sink("interventie_model_IQ.sens1.ses_adj.txt")
interventie_model_IQ
sink()

interventionmodel2 <- with(imputed.data2, lm(CITO_final ~ Preschool + SES + CITO_final_age + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_cito <- summary(pool(interventionmodel2), conf.int = T)
sink("interventie_model_cito.sens1.ses_adj.txt")
interventie_model_cito
sink()

interventionmodel3 <- with(imputed.data2, lm(Discrepancy ~ Preschool + SES + AGECHILD13 + CITO_final_age + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_IQ_cito <- summary(pool(interventionmodel3), conf.int = T)
sink("interventie_model_IQ_cito.sens1.ses_adj.txt")
interventie_model_IQ_cito
sink()

# Intervention models - test interaction #
interventionmodel1 <- with(imputed.data2, lm(WISC13_FSIQ ~ Preschool * SES + AGECHILD13 + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_IQ <- summary(pool(interventionmodel1), conf.int = T)
sink("interventie_model_IQ.sens1.ses_interaction_adj.txt")
interventie_model_IQ
sink()

interventionmodel2 <- with(imputed.data2, lm(CITO_final ~ Preschool * SES + CITO_final_age + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_cito <- summary(pool(interventionmodel2), conf.int = T)
sink("interventie_model_cito.sens1.ses_interaction_adj.txt")
interventie_model_cito
sink()

interventionmodel3 <- with(imputed.data2, lm(Discrepancy ~ Preschool * SES + AGECHILD13 + CITO_final_age + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_IQ_cito <- summary(pool(interventionmodel3), conf.int = T)
sink("interventie_model_IQ_cito.sens1.ses_interaction_adj.txt")
interventie_model_IQ_cito
sink()

#Step 1: Run function change_inequality.R saved in a different file.
#Step 2: Run function diff_ineq.R saved in a different file.
library(boot)
library(broom)

calculate_mean <- function(output_list, term, measure) {
  term_values <- sapply(output_list, function(x) {
    term_match <- x$term == term
    if (any(term_match)) {
      as.data.frame(x)[term_match, measure]
    } else {
      NA
    }
  })
  mean(unlist(term_values), na.rm = TRUE) * -1
}


## IQ as outcome ##
output_list <- lapply(1:30, function(imp) {
  ds <- complete(imputed.data2, imp)

  ineq_fun(dat = ds, inds = 1:nrow(ds),outcome = "WISC13_FSIQ", mediator = "Preschool", 
           ineq_group = "SES", covars = c("AGECHILD13", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
           set_mediator_to = list("Preschool==1"), 
           params=list(adjustment=c("AGECHILD13", "GENDER"),ineq_group="SES",outcome="WISC13_FSIQ"))

  boot_out <- boot(data = ds, statistic = ineq_fun, outcome = "WISC13_FSIQ", mediator = "Preschool", 
                   ineq_group = "SES", covars = c("AGECHILD13", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
                   set_mediator_to = list("Preschool==1"), R=1000, #change to 1000
                   params=list(adjustment=c("AGECHILD13", "GENDER"),ineq_group="SES",outcome="WISC13_FSIQ"))

  tidy(boot_out, conf.int = TRUE)
})

list_name <- "HI_IQ"

term_names <- c("pre_0 vs 1",
                "post_0 vs 1",
                "diff_post_0 vs 1",
                "ratio_post_0 vs 1")

pooled_result_IQ <- data.frame(
  term = term_names,
  statistic = sapply(term_names, function(term) calculate_mean(output_list, term, "statistic")),
  conf.low = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.high")),
  conf.high = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.low"))
)

write.csv(pooled_result_IQ, file = paste0("pooled.result.sens1", list_name, ".csv"), row.names = FALSE)

## Cito as outcome ##
output_list <- lapply(1:30, function(imp) {
  ds <- complete(imputed.data2, imp)
  
  ineq_fun(dat = ds, inds = 1:nrow(ds),outcome = "CITO_final", mediator = "Preschool", 
           ineq_group = "SES", covars = c("CITO_final_age", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
           set_mediator_to = list("Preschool==1"), 
           params=list(adjustment=c("CITO_final_age", "GENDER"),ineq_group="SES",outcome="CITO_final"))
  
  boot_out <- boot(data = ds, statistic = ineq_fun, outcome = "CITO_final", mediator = "Preschool", 
                   ineq_group = "SES", covars = c("CITO_final_age", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
                   set_mediator_to = list("Preschool==1"), R=1000, #change to 1000
                   params=list(adjustment=c("CITO_final_age", "GENDER"),ineq_group="SES",outcome="CITO_final"))
  
  tidy(boot_out, conf.int = TRUE)
})

list_name <- "HI_cito"

term_names <- c("pre_0 vs 1",
                "post_0 vs 1",
                "diff_post_0 vs 1",
                "ratio_post_0 vs 1")

pooled_result_cito <- data.frame(
  term = term_names,
  statistic = sapply(term_names, function(term) calculate_mean(output_list, term, "statistic")),
  conf.low = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.high")),
  conf.high = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.low"))
)

write.csv(pooled_result_cito, file = paste0("pooled.result.sens1", list_name, ".csv"), row.names = FALSE)

## Discrepancy as outcome ##
output_list <- lapply(1:30, function(imp) {
  ds <- complete(imputed.data2, imp)
  
  ineq_fun(dat = ds, inds = 1:nrow(ds),outcome = "Discrepancy", mediator = "Preschool", 
           ineq_group = "SES", covars = c("AGECHILD13", "CITO_final_age", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
           set_mediator_to = list("Preschool==1"), 
           params=list(adjustment=c("AGECHILD13", "CITO_final_age", "GENDER"),ineq_group="SES",outcome="Discrepancy"))
  
  boot_out <- boot(data = ds, statistic = ineq_fun, outcome = "Discrepancy", mediator = "Preschool", 
                   ineq_group = "SES", covars = c("AGECHILD13","CITO_final_age", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
                   set_mediator_to = list("Preschool==1"), R=1000, #change to 1000
                   params=list(adjustment=c("AGECHILD13", "CITO_final_age", "GENDER"),ineq_group="SES",outcome="Discrepancy"))
  
  tidy(boot_out, conf.int = TRUE)
})

list_name <- "HI_IQ-cito"

term_names <- c("pre_0 vs 1",
                "post_0 vs 1",
                "diff_post_0 vs 1",
                "ratio_post_0 vs 1")

pooled_result_IQ_cito <- data.frame(
  term = term_names,
  statistic = sapply(term_names, function(term) calculate_mean(output_list, term, "statistic")),
  conf.low = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.high")),
  conf.high = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.low"))
)

write.csv(pooled_result_IQ_cito, file = paste0("pooled.result.sens1", list_name, ".csv"), row.names = FALSE)

########################################################################################################################
##            Hypothetical intervention with imputed data SENSIVITIY WITH COMPLETE PRESCHOOL DATA                     ##
########################################################################################################################

imputed.data3 <- filter(imputed.data2, sens2==1)
nrow(complete(imputed.data3)) # N=1062

# Intervention models #
interventionmodel1 <- with(imputed.data3, lm(WISC13_FSIQ ~ Preschool + SES + AGECHILD13 + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_IQ <- summary(pool(interventionmodel1), conf.int = T)
sink("interventie_model_IQ.ses_adj.sens2.txt")
interventie_model_IQ
sink()

interventionmodel2 <- with(imputed.data3, lm(CITO_final ~ Preschool + SES + CITO_final_age + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_cito <- summary(pool(interventionmodel2), conf.int = T)
sink("interventie_model_cito.sens2.ses_adj.txt")
interventie_model_cito
sink()

interventionmodel3 <- with(imputed.data3, lm(Discrepancy ~ Preschool + SES + AGECHILD13 + CITO_final_age + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_IQ_cito <- summary(pool(interventionmodel3), conf.int = T)
sink("interventie_model_IQ_cito.sens2.ses_adj.txt")
interventie_model_IQ_cito
sink()

# Intervention models - test interactions #
interventionmodel1 <- with(imputed.data3, lm(WISC13_FSIQ ~ Preschool * SES + AGECHILD13 + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_IQ <- summary(pool(interventionmodel1), conf.int = T)
sink("interventie_model_IQ.ses_interaction_adj.sens2.txt")
interventie_model_IQ
sink()

interventionmodel2 <- with(imputed.data3, lm(CITO_final ~ Preschool * SES + CITO_final_age + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_cito <- summary(pool(interventionmodel2), conf.int = T)
sink("interventie_model_cito.sens2.ses_interaction_adj.txt")
interventie_model_cito
sink()

interventionmodel3 <- with(imputed.data3, lm(Discrepancy ~ Preschool * SES + AGECHILD13 + CITO_final_age + GENDER + Ethnicity + Income_cat + MAR_ST2))
interventie_model_IQ_cito <- summary(pool(interventionmodel3), conf.int = T)
sink("interventie_model_IQ_cito.sens2.ses_interaction_adj.txt")
interventie_model_IQ_cito
sink()


#Step 1: Run function change_inequality_rosa.R saved in a different file.
#Step 2: Run function diff_ineq2_rosa.R saved in a different file.
library(boot)
library(broom)

calculate_mean <- function(output_list, term, measure) {
  term_values <- sapply(output_list, function(x) {
    term_match <- x$term == term
    if (any(term_match)) {
      as.data.frame(x)[term_match, measure]
    } else {
      NA
    }
  })
  mean(unlist(term_values), na.rm = TRUE) * -1
}

## IQ as outcome ##
output_list <- lapply(1:30, function(imp) {
  ds <- complete(imputed.data3, imp)
  
  ineq_fun(dat = ds, inds = 1:nrow(ds),outcome = "WISC13_FSIQ", mediator = "Preschool", 
           ineq_group = "SES", covars = c("AGECHILD13", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
           set_mediator_to = list("Preschool==1"), 
           params=list(adjustment=c("AGECHILD13", "GENDER"),ineq_group="SES",outcome="WISC13_FSIQ"))
  
  boot_out <- boot(data = ds, statistic = ineq_fun, outcome = "WISC13_FSIQ", mediator = "Preschool", 
                   ineq_group = "SES", covars = c("AGECHILD13", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
                   set_mediator_to = list("Preschool==1"), R=1000, #change to 1000
                   params=list(adjustment=c("AGECHILD13", "GENDER"),ineq_group="SES",outcome="WISC13_FSIQ"))
  
  tidy(boot_out, conf.int = TRUE)
})

list_name <- "HI_IQ"

term_names <- c("pre_0 vs 1",
                "post_0 vs 1",
                "diff_post_0 vs 1",
                "ratio_post_0 vs 1")

pooled_result_IQ <- data.frame(
  term = term_names,
  statistic = sapply(term_names, function(term) calculate_mean(output_list, term, "statistic")),
  conf.low = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.high")),
  conf.high = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.low"))
)

write.csv(pooled_result_IQ, file = paste0("pooled.result.sens2", list_name, ".csv"), row.names = FALSE)

## Cito as outcome ##
output_list <- lapply(1:30, function(imp) {
  ds <- complete(imputed.data3, imp)
  
  ineq_fun(dat = ds, inds = 1:nrow(ds),outcome = "CITO_final", mediator = "Preschool", 
           ineq_group = "SES", covars = c("CITO_final_age", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
           set_mediator_to = list("Preschool==1"), 
           params=list(adjustment=c("CITO_final_age", "GENDER"),ineq_group="SES",outcome="CITO_final"))
  
  boot_out <- boot(data = ds, statistic = ineq_fun, outcome = "CITO_final", mediator = "Preschool", 
                   ineq_group = "SES", covars = c("CITO_final_age", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
                   set_mediator_to = list("Preschool==1"), R=1000, #change to 1000
                   params=list(adjustment=c("CITO_final_age", "GENDER"),ineq_group="SES",outcome="CITO_final"))
  
  tidy(boot_out, conf.int = TRUE)
})

list_name <- "HI_cito"

term_names <- c("pre_0 vs 1",
                "post_0 vs 1",
                "diff_post_0 vs 1",
                "ratio_post_0 vs 1")

pooled_result_cito <- data.frame(
  term = term_names,
  statistic = sapply(term_names, function(term) calculate_mean(output_list, term, "statistic")),
  conf.low = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.high")),
  conf.high = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.low"))
)

write.csv(pooled_result_cito, file = paste0("pooled.result.sens2", list_name, ".csv"), row.names = FALSE)

## Discrepancy as outcome ##
output_list <- lapply(1:30, function(imp) {
  ds <- complete(imputed.data3, imp)
  
  ineq_fun(dat = ds, inds = 1:nrow(ds),outcome = "Discrepancy", mediator = "Preschool", 
           ineq_group = "SES", covars = c("AGECHILD13", "CITO_final_age", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
           set_mediator_to = list("Preschool==1"), 
           params=list(adjustment=c("AGECHILD13", "CITO_final_age", "GENDER"),ineq_group="SES",outcome="Discrepancy"))
  
  boot_out <- boot(data = ds, statistic = ineq_fun, outcome = "Discrepancy", mediator = "Preschool", 
                   ineq_group = "SES", covars = c("AGECHILD13","CITO_final_age", "GENDER", "Ethnicity", "Income_cat", "MAR_ST2"), interaction = TRUE,
                   set_mediator_to = list("Preschool==1"), R=1000, #change to 1000
                   params=list(adjustment=c("AGECHILD13", "CITO_final_age", "GENDER"),ineq_group="SES",outcome="Discrepancy"))
  
  tidy(boot_out, conf.int = TRUE)
})

list_name <- "HI_IQ-cito"

term_names <- c("pre_0 vs 1",
                "post_0 vs 1",
                "diff_post_0 vs 1",
                "ratio_post_0 vs 1")

pooled_result_IQ_cito <- data.frame(
  term = term_names,
  statistic = sapply(term_names, function(term) calculate_mean(output_list, term, "statistic")),
  conf.low = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.high")),
  conf.high = sapply(term_names, function(term) calculate_mean(output_list, term, "conf.low"))
)

write.csv(pooled_result_IQ_cito, file = paste0("pooled.result.sens2", list_name, ".csv"), row.names = FALSE)
