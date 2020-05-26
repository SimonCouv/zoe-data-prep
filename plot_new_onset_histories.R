library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(readr)
library(purrr)
library(stringr)
library(tidyselect)
library(ggplot2)
library(forcats)
library(ggforce)
library(R.utils)

# parse arguments
args <- commandArgs(trailingOnly = TRUE, asValues=T, adhoc=T)
args$timestamp <- as.character(args$timestamp)

attach(args)
cat("arguments provided:\n\n")
print(str(args))

a <- distinct(fread(sprintf("%s/linked_cleaned_twins_assessments_export_%s.csv", wdir, timestamp), data.table=F)) %>% 
  rename(study_no = TwinSN)
p <- distinct(fread(sprintf("%s/linked_cleaned_twins_patients_export_geocodes_%s.csv", wdir, timestamp), data.table=F)) %>% 
  rename(study_no = TwinSN)
ct <- distinct(fread(sprintf("%s/linked_twins_covid_test_export_%s.csv", wdir, timestamp), data.table=F))%>% 
  rename(study_no = TwinSN)
new_onset = read_csv(file.path(wdir, new_onsetfile))
new_pos = read_csv(file.path(wdir, new_posfile))
if (zoe_preds_file != "skipped"){
  zoe <- read_csv(file.path(wdir,zoe_preds_file))
}
zoe_symptom_map <- read_csv(symptom_map_file)

message("data loaded successfully")

timestamp_date <- as_date(substr(timestamp, 1, 8))

# variables of interest
p_vars_anno <- c("interacted_with_covid", "contact_health_worker", "classic_symptoms",
                 "year_of_birth", "gender", "has_diabetes","has_heart_disease", 
                 "has_lung_disease", "is_smoker", "does_chemotherapy", 
                 "has_cancer", "has_kidney_disease", "already_had_covid",
                 "interacted_patients_with_covid", "classic_symptoms_days_ago")
a_vars_anno <- c("had_covid_test", "treated_patients_with_covid", "tested_covid_positive")
binary_symptoms <- c('persistent_cough', 'delirium','fever', 'diarrhoea', 'abdominal_pain', 'chest_pain', 'hoarse_voice', 'skipped_meals', 'loss_of_smell', 'headache', 'sore_throat')
multicat_symptoms <- c('shortness_of_breath', 'fatigue')
collapsed_symptoms <- c('fatigue_binary', 'shortness_of_breath_binary')
a_vars_filter <- c(binary_symptoms, collapsed_symptoms)

# Impute negative symptoms from 'healthy' health_status when symptoms are NA
for (v in c(multicat_symptoms, binary_symptoms, collapsed_symptoms)){
  a[a$health_status == "healthy" & is.na(a[[v]]), v] <- FALSE
}

# parse dates, drop few individuals with specific invalid date format
# merge with
a <- dplyr::filter(a, updated_at != "-- ::") %>% mutate(date_updated_at = as_date(updated_at))
p <- dplyr::filter(p, updated_at != "-- ::") %>% mutate(date_updated_at = as_date(updated_at))

zoe_symptom_map <- bind_rows(zoe_symptom_map, tibble(abbrev="any", symptom="any_symptom"))
plot_symptoms <- c(binary_symptoms, multicat_symptoms, "any_symptom")


# format COVID test results
ct_tested <- ct$study_no[nchar(ct$result) > 0]

new_onset_keep <- dplyr::filter(new_onset, n_new_onset >= min_new_onset, n_prior <= max_prior)

plotdat_tmp <- a %>%
  dplyr::filter(patient_id %in% c(new_onset_keep$patient_id, new_pos$patient_id)) %>%  
  mutate(any_symptom = apply(.[,a_vars_filter],1, function(x)any(x, na.rm=T))) %>%
  mutate_at(vars(binary_symptoms), as.numeric) %>%
  mutate(
    shortness_of_breath = recode(na_if(shortness_of_breath, ""), 
                                 'no'=0,'mild'=1, 'significant'=2, 'severe'=3, .missing=0)/3,
    fatigue = recode(na_if(fatigue, ""), 'no'=0,'mild'=1, 'severe'=2, .missing=0)/2
  ) %>%
  dplyr::select(date_updated_at, study_no, plot_symptoms) %>% 
  gather(symptom, value, c(plot_symptoms)) %>% 
  left_join(zoe_symptom_map, by="symptom") %>% 
  mutate(tested_str = ifelse(study_no %in% ct_tested, "; TESTED", ""),
         newpos_str = ifelse(study_no %in% new_pos$study_no, "; NEWPOS", ""))

if (zoe_preds_file != "skipped"){
  plotdat <- plotdat_tmp %>%
  left_join(zoe, by="study_no") %>%
  mutate(sn_anno = sprintf("%d [%dy, p_zoe=%s%s%s]", 
                           study_no, age, 
                           round(p_predicted_covid, 2), tested_str, newpos_str)) %>%
  mutate(sn_anno = fct_reorder(sn_anno, -p_predicted_covid))
} else {
  plotdat <- plotdat_tmp %>%
  left_join(dplyr::select(p, study_no, age), by="study_no") %>%
  mutate(sn_anno = sprintf("%d [%dy%s%s]", 
                           study_no, age, tested_str, newpos_str))
}

message("pre-processing complete. Generating plots.")

# ggsave(plot = p_new_onset, file.path(wdir, sprintf("new_onset_history_%s.svg", timestamp)), width = 10, height = 15)
nr <- 8
nc <- 2

pdf(file.path(wdir, hist_plot_file), width = 10 , height = 15)
for (i in 1:ceiling(n_distinct(plotdat$study_no)/(nr*nc))){
  print(i)
  p <- plotdat %>% 
    ggplot(aes(x=date_updated_at, y=abbrev, fill=value))+
    geom_tile()+
    facet_wrap_paginate(~sn_anno, ncol=nc, nrow=nr, page=i, scales="free_y")+
    scale_fill_gradient(name="symptom severity", low = "green", high = "red")+
    theme_bw()+
    geom_vline(xintercept=as.numeric(timestamp_date-onset_window_length)-0.5, linetype=2)+
    geom_vline(xintercept=as.numeric(timestamp_date-onset_window_length-stat_window_length)-0.5, linetype=2)+
    xlab("assessment date")+
    ylab("symptom abbreviation")

  print(p)
}
dev.off()
