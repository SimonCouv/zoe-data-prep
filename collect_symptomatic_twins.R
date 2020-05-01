
########################################################################
## setup
########################################################################


library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(readr)
library(purrr)
library(stringr)
library(tidyselect)

code_last_episode <- function(data, vars){
  # per symptom: get onset and end of most recent episode, and most recent positive report of the symptom
  
  l <- list()
  for (v in vars){
    # print(v)
    
    # subset, sort, drop NA
    x <- data[, c(v, "date_updated_at"), drop=T]
    x <- x[order(x$date_updated_at),] %>% drop_na()
    
    # split periods according to positive/negative status, and respecting a
    # maximal interval between assessments in the same period of 'max_carry_forward' days
    periods <- list()
    period_signs <- c()
    period <- 1
    
    if (nrow(x) > 1){
      for (i in 1:(nrow(x)-1)){
        # browser()
        
        t2 <- x$date_updated_at[i+1] 
        t1 <- x$date_updated_at[i]
        if (t2 - t1 < max_carry_forward & x[[v]][i+1] == x[[v]][i]){
          period <- c(period, i+1)
        } else {
          periods <- c(periods, list(period))
          period_signs <- c(period_signs, x[[v]][i])
          period <- i+1
        }
        if (i+1==nrow(x)) {periods <- c(periods, list(period)); period_signs <- c(period_signs, x[[v]][i+1])}
      }
    } else {
      periods <- c(periods, list(period))
      period_signs <- c(period_signs, x[[v]])
    }
    
    
    # start and end dates of the last period during which patient had positive status
    last_positive_onset <- last_positive_end <- as_date(NA)
    if (any(period_signs)){
      last_pos_period <- max(which(period_signs))
      last_positive_onset <- x$date_updated_at[min(periods[[last_pos_period]])]
      if (last_pos_period != length(periods)){
        last_positive_end <- x$date_updated_at[max(periods[[last_pos_period]])]
      }
    }
    
    # most recent positive status
    most_recent_positive <- if (any(x[[v]], na.rm=T)) {
      max(x$date_updated_at[ x[[v]] ], na.rm=T)
    } else as_date(NA)
    
    # collect results
    l[[v]] <- list(
      last_positive_onset = last_positive_onset,
      last_positive_end = last_positive_end,
      most_recent_positive = most_recent_positive
    ) 
  }
  
  # bind in df
  res <- rbindlist(l)
  res[, variable := vars]
}

########################################################################
## arguments and parameters
########################################################################

# parse arguments
args <- commandArgs(trailingOnly = TRUE)
timestamp <- args[1]
twins_annofile <- args[2]
mapfile <- args[3]
wdir <- args[4]
max_days_past <- as.numeric(args[5])
max_carry_forward <- as.numeric(args[6])

# load data
a <- distinct(fread(sprintf("%s/cleaned_twins_assessments_export_%s.csv", wdir, timestamp), data.table=F))
p <- distinct(fread(sprintf("%s/cleaned_twins_patients_export_geocodes_%s.csv", wdir, timestamp), data.table=F))
twins_anno <- fread(twins_annofile) %>% 
  setnames(tolower(names(.)))
id_map <- distinct(fread(mapfile) %>% setnames(c("study_no", "app_id")))


# variables of interest
p_vars_anno <- c("interacted_with_covid", "contact_health_worker", "classic_symptoms",
                 "year_of_birth", "gender", "has_diabetes","has_heart_disease", 
                 "has_lung_disease", "is_smoker", "does_chemotherapy", 
                 "has_cancer", "has_kidney_disease", "already_had_covid",
                 "interacted_patients_with_covid", "classic_symptoms_days_ago")
a_vars_filter <- c("fever", "persistent_cough", "fatigue_binary", "shortness_of_breath_binary", "delirium", "loss_of_smell")
a_vars_anno <- c("had_covid_test", "treated_patients_with_covid", "tested_covid_positive")

# Impute negative symptoms from 'healthy' health_status when symptoms are NA
for (v in a_vars_filter){
  a[a$health_status == "healthy" & is.na(a[[v]]), v] <- FALSE
}

########################################################################
## checks
########################################################################

day_id_count_vals <-  mutate(a, date_updated_at = as_date(updated_at)) %>% 
  count(date_updated_at, patient_id) %>% pull(n) %>% unique()
stopifnot(day_id_count_vals == 1)
message("checks passed")

########################################################################
## processing
########################################################################

multiple_accounts <- id_map %>% distinct() %>% group_by(study_no) %>% dplyr::filter(dplyr::n()>1) 

# retain only most recent patient info
p_summary <- p %>% 
  mutate(date_updated_at = as_date(updated_at)) %>% 
  group_by(id) %>% 
  dplyr::filter(date_updated_at == max(date_updated_at)) %>% 
  dplyr::select(id, p_vars_anno) %>% 
  left_join(id_map, by=c("id"="app_id")) %>% 
  left_join(
    dplyr::select(
      twins_anno, 
      study_no,
      year_of_birth_phenobase = year_birth,
      sex_phenobase = sex,
      actual_zygosity_phenobase = actual_zygosity
    )
  ) %>% 
  mutate(
    sex_phenobase = na_if(sex_phenobase, ""),
    sex_phenobase2 = recode(sex_phenobase, "F"=0, "M"=1),
    sex_mismatch = sex_phenobase2 != gender,  #caveat: trans/nonbinary/.. individuals
    birthyear_diff = abs(year_of_birth - year_of_birth_phenobase),
    twinSN_has_multiple_accounts = study_no %in% multiple_accounts$study_no
  ) %>% 
  dplyr::select(-sex_phenobase2) %>% 
  dplyr::select(study_no, sex_mismatch, birthyear_diff, everything())

# summarise covid info from assessment
a_summary <-  dplyr::select(a, a_vars_anno, patient_id) %>% 
  group_by(patient_id) %>% 
  summarise_all(~paste0(unique(.x), collapse = ", "))

message("calculating symptomatic periods")

# summary per symptom and per twin
candidates <- a %>%
  mutate(date_updated_at = as_date(updated_at)) %>% 
  dplyr::filter(!is.na(date_updated_at)) %>%
  group_by(patient_id) %>%
  nest() %>%
  mutate(
    last_episode = map(data, ~code_last_episode(.x, vars=a_vars_filter))  #KEY STEP
  ) %>%
  dplyr::select(patient_id, last_episode) %>%
  unnest(last_episode) %>%
  dplyr::filter(!is.na(last_positive_onset)) %>%
  dplyr::filter(last_positive_onset > as_date(substr(timestamp, 1, 8)) -  max_days_past) %>% # retain only symptomatic periods starting within last 'max_days_past' days
  arrange(desc(last_positive_onset), !(is.na(last_positive_end)), last_positive_end) %>% 
  left_join(p_summary, by=c("patient_id" = "id")) %>%
  left_join(a_summary, by="patient_id") %>%
  dplyr::select(study_no, everything())

message("summarising symptomatic periods")

# summarise further over symptoms to get one line per twin
candidates_summary <- candidates %>% 
  group_by(patient_id, study_no) %>% 
  summarise(n_symptoms = dplyr::n(), 
            symptoms = paste0(variable, collapse = ", "),
            `most recent positive report [any_symptom]` = max(most_recent_positive, na.rm = T),
            `onset last positive period [most recent over symptoms]` = max(last_positive_onset, na.rm = T),
            `onset last positive period [earliest over symptoms]` = min(last_positive_onset, na.rm = T),
            `any active symptom not reported as over` = any(is.na(last_positive_end))
  ) %>% 
  arrange(
    desc(n_symptoms),
    desc(`most recent positive report [any_symptom]`),
    desc(`onset last positive period [earliest over symptoms]`)
  ) %>%
  left_join(p_summary, by=c("patient_id" = "id", "study_no")) %>%
  left_join(a_summary, by="patient_id") %>% 
  dplyr::select(study_no, sex_mismatch, birthyear_diff, twinSN_has_multiple_accounts, everything())

write_csv(candidates, path = sprintf("%s/symptomatic_twins_PerTwinPerSymptom_%s.csv", wdir, timestamp))
write_csv(candidates_summary, path = sprintf("%s/symptomatic_twins_PerTwin_%s.csv", wdir, timestamp))

cat("\n\n---Formatting completed---\n\n")

