
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
    
    # reduce the intervals, i.e. retain only dates on which status changes: positive-> negative or vice versa
    xred <- rbind(x[1,], x[replace_na(lag(x[[v]]) != x[[v]], FALSE),])
    
    # start and end dates of the last period during which patient had positive status
    last_positive_onset <- last_positive_end <- NA
    if (any(xred[[v]], na.rm=T)){
      last_positive_onset <- max(xred$date_updated_at[ xred[[v]] ], na.rm=T)
      stopifnot(is.Date(last_positive_onset))
      
      if (any(!xred[[v]], na.rm=T)){
        m_neg <- max(xred$date_updated_at[ !xred[[v]] ], na.rm=T)
        last_positive_end <- if(m_neg > last_positive_onset) m_neg else NA
      }
    }
    
    
    # most recent positive status
    most_recent_positive <- if (any(x[[v]], na.rm=T)) {
      max(x$date_updated_at[ x[[v]] ], na.rm=T)
    } else NA
    
    # collect results
    l[[v]] <- tibble(
      last_positive_onset = last_positive_onset,
      last_positive_end = last_positive_end,
      most_recent_positive = most_recent_positive
    ) %>% mutate_all(as_date)
  }
  
  # bind in df
  bind_rows(l, .id="variable")
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
max_days_past <- args[5]

# load data
a <- distinct(fread(sprintf("%s/cleaned_twins_assessments_export_%s.csv", wdir, timestamp)))
p <- distinct(fread(sprintf("%s/cleaned_twins_patients_export_geocodes_%s.csv", wdir, timestamp)))
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


########################################################################
## checks
########################################################################

day_id_count_vals <-  mutate(a, date_updated_at = as_date(updated_at)) %>% 
  count(date_updated_at, patient_id) %>% pull(n) %>% unique()
stopifnot(day_id_count_vals == 1)

########################################################################
## processing
########################################################################

multiple_accounts <- id_map %>% distinct() %>% group_by(study_no) %>% dplyr::filter(n()>1) 

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

# summary per symptom and per twin
candidates <- a %>%
  mutate(date_updated_at = as_date(updated_at)) %>% 
  group_by(patient_id) %>%
  nest() %>%
  mutate(
    last_episode = map(data, ~code_last_episode(.x, vars=a_vars_filter))  #KEY STEP
  ) %>%
  dplyr::select(patient_id, last_episode) %>%
  unnest(last_episode) %>%
  dplyr::filter(!is.na(last_positive_onset)) %>%
  dplyr::filter(last_positive_onset > today() -  max_days_past) %>% # retain only symptomatic periods starting within last 'max_days_past' days
  arrange(desc(last_positive_onset), !(is.na(last_positive_end)), last_positive_end) %>% 
  left_join(p_summary, by=c("patient_id" = "id")) %>%
  left_join(a_summary, by="patient_id") %>%
  dplyr::select(study_no, everything())

# summarise further over symptoms to get one line per twin
candidates_summary <- candidates %>% 
  group_by(patient_id, study_no) %>% 
  summarise(n_symptoms = n(), 
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


########################################################################
## generate report
########################################################################

rmarkdown::render(
  input = "code/zoe-data-prep/twin_participation_report.Rmd",
  params = list(timestamp = timestamp, wdir=wdir),
  output_file = sprintf("twinsuk_participation_report_%s.pdf", timestamp),
  output_dir = wdir
)

