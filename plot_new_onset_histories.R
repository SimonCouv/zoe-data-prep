# parse arguments
args <- commandArgs(trailingOnly = TRUE)
timestamp <- args[1]
mapfile <- args[2]
zoe_preds_file <- args[3]
wdir <- args[4]

a <- distinct(fread(sprintf("%s/cleaned_twins_assessments_export_%s.csv", wdir, timestamp), data.table=F))
p <- distinct(fread(sprintf("%s/cleaned_twins_patients_export_geocodes_%s.csv", wdir, timestamp), data.table=F))
id_map <- distinct(fread(mapfile) %>% setnames(c("study_no", "app_id")))
zoe <- read_csv(zoe_preds_file) %>% 
  left_join(id_map, by=c("id"="app_id"))


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

zoe_plotdat <- zoe %>% 
  dplyr::select(study_no, matches("predicted_covid")) %>% 
  left_join(a, by="study_no") %>% 
  mutate_at(vars(binary_symptoms), as.numeric) %>%
  mutate(
    shortness_of_breath = recode(na_if(shortness_of_breath, ""), 
                                 'no'=0,'mild'=1, 'significant'=2, 'severe'=3, .missing=0)/3,
    fatigue = recode(na_if(fatigue, ""), 'no'=0,'mild'=1, 'severe'=2, .missing=0)/2
  ) %>%
  dplyr::select(date_updated_at, study_no, all_symptoms, matches("predicted_covid")) %>% 
  gather(symptom, value, all_symptoms) %>% 
  arrange(study_no, symptom) %>% 
  mutate(sn_anno = sprintf("%d [p_zoe=%s]", study_no, round(p_predicted_covid, 2))) %>%
  mutate(sn_anno = fct_reorder(sn_anno, -p_predicted_covid))

# top 40
p_zoe_40 <- zoe_plotdat %>% 
  ggplot(aes(x=date_updated_at, y=symptom, fill=value))+
  geom_tile()+
  facet_wrap(~sn_anno, ncol=4)+
  scale_fill_gradient(name="symptom severity", low = "green", high = "red")+
  theme_bw()+
  geom_vline(xintercept=as.numeric(timestamp_date-2)-0.5, linetype=2)+
  xlab("assessment date")

ggsave(plot = p_zoe_40, file.path(wdir, sprintf("zoe_top40_history_%s.svg", timestamp)), width = 10, height = 15)