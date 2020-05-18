
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
library(ggplot2)
library(forcats)

is_new_onset <- function(data, symptoms, day_t, onset_window_length=2, 
                         stat_window_length=12, prior_status_method = c("last", "any")){
  
  prior_status_method <- match.arg(prior_status_method)
  
  #### any positive report in <window_length> days before <day_t> -------------
  data_onset_in <- data[data$date_updated_at + onset_window_length >= day_t, symptoms]
  pos_in_window <- apply(data_onset_in, 2, function(x)any(x,na.rm = T))
  
  ### prior status ----------------------------------------------------
  # allow for reporting dates to differ between symptoms (-> drop_na per symptom in for loop)
  status_before <- rep(FALSE, length(symptoms))
  names(status_before) <- symptoms
  for (v in symptoms){
    
    # take only non-NA assessments into account
    # remark: relies on setting symptoms NA -> FALSE in healthy assessments) !!!
    x <- data[, c(v, "date_updated_at")]   # symptom and date
    x <- x[!is.na(x[[v]]),]  # drop NA
    
    # get assessments dates within status-defining window
    d <- x$date_updated_at
    d1 <- d[d > day_t - (onset_window_length + stat_window_length)]
    d2 <- d[d < day_t - onset_window_length]
    d_stat <- intersect(d1, d2)
    
    if (length(d_stat) > 0){
      if (prior_status_method == "last") {
        status_before[v] <- x[d == max(d_stat), v, drop=T]
      } else if (prior_status_method == "any"){
        status_before[v] <- any(x[d %in% d_stat, v, drop=T], na.rm = T)
      }
    }
  }
  
  ### summary tibble ----------------------------------------------------
  new_onset <- pos_in_window & !(status_before)
  tibble(symptoms, status_before, pos_in_window, new_onset)
  
  # debug
  # nr <- nrow(data)
  # if (any(unlist(data[(nr-1):nr, symptoms]), na.rm = T)) {
  #   print(data[, c(symptoms, "date_updated_at")], n=100)
  #   print(pos_in_window)
  #   print(status_before)
  #   print(tibble::enframe(new_onset, name = "symptom"))
  #   browser()
  # }
  
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
onset_window_length <- as.numeric(args[5])
stat_window_length <- as.numeric(args[6])
prior_status_method <- args[7]

# load data
a <- distinct(fread(sprintf("%s/cleaned_twins_assessments_export_%s.csv", wdir, timestamp), data.table=F))
p <- distinct(fread(sprintf("%s/cleaned_twins_patients_export_geocodes_%s.csv", wdir, timestamp), data.table=F))
twins_anno <- fread(twins_annofile) %>% 
  setnames(tolower(names(.)))
id_map <- distinct(fread(mapfile) %>% setnames(c("study_no", "app_id")))

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



########################################################################
## checks
########################################################################

# assessments are quantised
day_id_count_vals <-  count(a, date_updated_at, patient_id) %>% pull(n) %>% unique()
stopifnot(day_id_count_vals == 1)

# all app IDs linked to TwinsUK study numbers
stopifnot(sum(is.na(a$study_no)) == 0)
stopifnot(sum(is.na(p$study_no)) == 0)

message("checks passed")

########################################################################
## processing
########################################################################

multiple_accounts <- id_map %>% distinct() %>% group_by(study_no) %>% dplyr::filter(dplyr::n()>1) 

# retain only most recent patient info
p_summary <- p %>% 
  group_by(id) %>% 
  dplyr::filter(date_updated_at == max(date_updated_at)) %>% 
  dplyr::select(id, study_no, p_vars_anno) %>% 
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
  dplyr::select(id, study_no, sex_mismatch, birthyear_diff, everything())

# summarise covid info from assessment
a_summary <- a %>% 
  arrange(date_updated_at) %>%
  dplyr::select(a_vars_anno, patient_id) %>% 
  group_by(patient_id) %>% 
  summarise_all(~paste0(unique(.x[nchar(.x)>0]), collapse = "->"))


########################################################################
## new onset
########################################################################

message("calculating new onset of symptoms")

new_onset <- a %>% 
  group_by(patient_id, study_no) %>% 
  nest() %>% 
  mutate(
    new_onset = map(
      data, 
      ~is_new_onset(.x, symptoms = a_vars_filter, 
                    day_t = timestamp_date, 
                    stat_window_length = stat_window_length,
                    onset_window_length = onset_window_length,
                    prior_status_method = prior_status_method)
    )
  ) %>% 
  dplyr::select(-data) %>% 
  unnest(new_onset)

new_onset_summary <- new_onset %>% 
  summarise(
    new_onset_symptoms = paste0(symptoms[new_onset], collapse="+"),
    prior_symptoms = paste0(symptoms[status_before], collapse="+"),
    n_new_onset = sum(new_onset),
    n_prior = sum(status_before)
  ) %>% 
  arrange(desc(n_new_onset), n_prior)

write_csv(new_onset_summary, file.path(wdir, sprintf("new_onset_%s.csv", timestamp)))


########################################################################
## Plot new onset counts matrix
########################################################################

p_symptom_count <- new_onset_summary %>% 
  ungroup() %>% 
  count(n_new_onset, n_prior) %>% 
  ggplot(aes(n_new_onset, n_prior, fill=n))+
  geom_tile()+
  geom_text(aes(label=n))+
  scale_fill_continuous(low="grey", high="red", name="number of individuals")+
  xlab("number of new symptoms in last two days")+
  ylab("number of 'active' symptoms three days ago")+
  theme_bw()

ggsave(plot = p_symptom_count, file.path(wdir, sprintf("new_onset_symptom_count_%s.svg", timestamp)))
