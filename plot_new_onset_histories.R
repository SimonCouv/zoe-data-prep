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

# parse arguments
args <- commandArgs(trailingOnly = TRUE)
timestamp <- args[1]
mapfile <- args[2]
zoe_preds_file <- args[3]
wdir <- args[4]
onset_window_length <- as.numeric(args[5])
stat_window_length <- as.numeric(args[6])

a <- distinct(fread(sprintf("%s/cleaned_twins_assessments_export_%s.csv", wdir, timestamp), data.table=F))
p <- distinct(fread(sprintf("%s/cleaned_twins_patients_export_geocodes_%s.csv", wdir, timestamp), data.table=F))
id_map <- distinct(fread(mapfile) %>% setnames(c("study_no", "app_id")))
zoe <- read_csv(file.path(wdir,zoe_preds_file))


timestamp_date <- as_date(substr(timestamp, 1, 8))

# variables of interest
p_vars_anno <- c("interacted_with_covid", "contact_health_worker", "classic_symptoms",
                 "year_of_birth", "gender", "has_diabetes","has_heart_disease", 
                 "has_lung_disease", "is_smoker", "does_chemotherapy", 
                 "has_cancer", "has_kidney_disease", "already_had_covid",
                 "interacted_patients_with_covid", "classic_symptoms_days_ago")
a_vars_filter <- c("fever", "persistent_cough", "fatigue_binary", "shortness_of_breath_binary", "delirium", "loss_of_smell")
a_vars_anno <- c("had_covid_test", "treated_patients_with_covid", "tested_covid_positive")
binary_symptoms <- c('persistent_cough', 'delirium','fever', 'diarrhoea', 'abdominal_pain', 'chest_pain', 'hoarse_voice', 'skipped_meals', 'loss_of_smell', 'headache', 'sore_throat')
multicat_symptoms <- c('shortness_of_breath', 'fatigue')
collapsed_symptoms <- c('fatigue_binary', 'shortness_of_breath_binary')

# Impute negative symptoms from 'healthy' health_status when symptoms are NA
for (v in c(multicat_symptoms, binary_symptoms, collapsed_symptoms)){
  a[a$health_status == "healthy" & is.na(a[[v]]), v] <- FALSE
}

# parse dates, drop few individuals with specific invalid date format
# merge with
a <- dplyr::filter(a, updated_at != "-- ::") %>% mutate(date_updated_at = as_date(updated_at)) %>% 
  left_join(id_map, by=c("patient_id"="app_id"))
p <- dplyr::filter(p, updated_at != "-- ::") %>% mutate(date_updated_at = as_date(updated_at)) %>% 
  left_join(id_map, by=c("id"="app_id"))

zoe_symptoms <- c(binary_symptoms, multicat_symptoms)
zoe_symptom_abbrevs <- map_chr(
  zoe_symptoms, 
  ~paste0(substr(unlist(strsplit(.x, split = "_")),1,1), collapse="")
)
zoe_symptom_map <- tibble(zoe_symptom_abbrevs, zoe_symptoms)
names(zoe_symptom_map) <- c("abbrev", "symptom")


plotdat <- zoe %>% 
  dplyr::select(study_no, matches("predicted_covid")) %>% 
  left_join(a, by="study_no") %>% 
  mutate_at(vars(binary_symptoms), as.numeric) %>%
  mutate(
    shortness_of_breath = recode(na_if(shortness_of_breath, ""), 
                                 'no'=0,'mild'=1, 'significant'=2, 'severe'=3, .missing=0)/3,
    fatigue = recode(na_if(fatigue, ""), 'no'=0,'mild'=1, 'severe'=2, .missing=0)/2
  ) %>%
  dplyr::select(date_updated_at, study_no, zoe_symptoms, matches("predicted_covid")) %>% 
  gather(symptom, value, zoe_symptoms) %>% 
  left_join(zoe_symptom_map, by="symptom") %>% 
  arrange(study_no, abbrev) %>% 
  mutate(sn_anno = sprintf("%d [p_zoe=%s]", study_no, round(p_predicted_covid, 2))) %>%
  mutate(sn_anno = fct_reorder(sn_anno, -p_predicted_covid))

# ggsave(plot = p_new_onset, file.path(wdir, sprintf("new_onset_history_%s.svg", timestamp)), width = 10, height = 15)


nr <- 8
nc <- 4

pdf(file.path(wdir, sprintf("new_onset_history_%s.pdf", timestamp)), width = 10 , height = 15)
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
