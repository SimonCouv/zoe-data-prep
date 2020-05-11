
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
    onset_last_positive_period <- end_of_last_positive_period <- as_date(NA)
    if (any(period_signs)){
      last_pos_period <- max(which(period_signs))
      onset_last_positive_period <- x$date_updated_at[min(periods[[last_pos_period]])]
      if (last_pos_period != length(periods)){
        end_of_last_positive_period <- x$date_updated_at[max(periods[[last_pos_period]])]
      }
    }
    
    # most recent positive status
    most_recent_positive <- if (any(x[[v]], na.rm=T)) {
      max(x$date_updated_at[ x[[v]] ], na.rm=T)
    } else as_date(NA)
    
    # collect results
    l[[v]] <- list(
      onset_last_positive_period = onset_last_positive_period,
      end_of_last_positive_period = end_of_last_positive_period,
      most_recent_positive = most_recent_positive
    ) 
  }
  
  # bind in df
  res <- rbindlist(l)
  res[, variable := vars]
}

is_new_onset <- function(data, symptoms, day_t, onset_window_length=2, 
                         stat_window_length=12){
  
  # any positive report in <window_length> days before <day_t>
  data_onset_in <- data[data$date_updated_at + onset_window_length >= day_t, symptoms]
  pos_in_window <- apply(data_onset_in, 2, function(x)any(x,na.rm = T))
  
  # status before window
  # allow for reporting dates to differ between symptoms (-> drop_na per symptom in for loop)
  # status is negative, unless the last non-NA assessment within the 
  #   status-defining window is positive.
  status_before <- rep(FALSE, length(symptoms))
  names(status_before) <- symptoms
  for (v in symptoms){
    
    # take only non-NA assessments into account
    # remark: relies on setting symptoms NA -> F in healthy assessments) !!!
    x <- data[, c(v, "date_updated_at")]
    x <- x[!is.na(x[[v]]),]
    
    # get assessments dates within status-defining window
    d <- x$date_updated_at
    d1 <- d[d > day_t - (onset_window_length + stat_window_length)]
    d2 <- d[d < day_t - onset_window_length]
    d_stat <- intersect(d1, d2)
    
    # if there are assessments within the status-defining window, the last one defines status
    if (length(d_stat) > 0){
      status_before[v] <- x[d == max(d_stat), v, drop=T]
    }
  }
  
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
max_days_past <- as.numeric(args[5])
max_carry_forward <- as.numeric(args[6])

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

# Impute negative symptoms from 'healthy' health_status when symptoms are NA
for (v in a_vars_filter){
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
  dplyr::select(study_no, p_vars_anno) %>% 
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
      ~is_new_onset(.x, symptoms = a_vars_filter, day_t = timestamp_date)
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

p_new_onset_history <-new_onset_summary %>% 
  dplyr::filter(n_new_onset > 0) %>% 
  ungroup() %>% 
  distinct(study_no, n_new_onset, n_prior) %>% 
  left_join(a) %>% 
  dplyr::select(date_updated_at, study_no, a_vars_filter, n_new_onset, n_prior) %>% 
  gather(symptom, value, a_vars_filter) %>% 
  arrange(study_no, symptom) %>% 
  # dplyr::filter(study_no==971) %>%
  mutate(presence = ifelse(is.na(value), "no data",ifelse(value, "present", "absent")),
         sn_anno = sprintf("%d [%d new, %d prior]", study_no, n_new_onset, n_prior)) %>% 
  mutate(sn_anno = fct_reorder(sn_anno, -n_new_onset)) %>% 
  ggplot(aes(x=date_updated_at, y=symptom, fill=presence))+
  geom_tile()+
  facet_wrap(~sn_anno, ncol=4)+
  scale_fill_manual(values = c(present="red", absent="green", "no data" = "lightgrey"),
                    name="symptom presence")+
  theme_bw()+
  geom_vline(xintercept=as.numeric(timestamp_date-2)-0.5, linetype=2)+
  xlab("assessment date")

plot_path <- file.path(wdir, sprintf("new_onset_plots_%s.RData", timestamp))
save(p_symptom_count, p_new_onset_history, file = plot_path)

########################################################################
## symptomatic periods
########################################################################

message("calculating symptomatic periods")

# summary per symptom and per twin
candidates <- a %>%
  group_by(patient_id) %>%
  nest() %>%
  mutate(
    last_episode = map(data, ~code_last_episode(.x, vars=a_vars_filter))  #KEY STEP
  ) %>%
  dplyr::select(patient_id, last_episode) %>%
  unnest(last_episode) %>%
  dplyr::filter(!is.na(onset_last_positive_period)) %>%
  dplyr::filter(onset_last_positive_period > as_date(substr(timestamp, 1, 8)) -  max_days_past) %>% # retain only symptomatic periods starting within last 'max_days_past' days
  arrange(desc(onset_last_positive_period), !(is.na(end_of_last_positive_period)), end_of_last_positive_period) %>% 
  left_join(p_summary, by=c("patient_id" = "id")) %>%
  left_join(a_summary, by="patient_id") %>%
  dplyr::select(study_no, everything())

message("summarising symptomatic periods")

# summarise further over symptoms to get one line per twin
# ignore 'negative_health_status' when summarising, to maintain specificity
candidates_summary <- candidates %>% 
  group_by(patient_id, study_no) %>% 
  dplyr::filter(variable != "negative_health_status") %>% 
  summarise(n_symptoms = dplyr::n(), 
            symptoms = paste0(variable, collapse = ", "),
            `most recent positive report [any_symptom]` = max(most_recent_positive, na.rm = T),
            `onset of last positive period [most recent over symptoms]` = max(onset_last_positive_period, na.rm = T),
            `onset of last positive period [earliest over symptoms]` = min(onset_last_positive_period, na.rm = T),
            `any active symptom not reported as over` = any(is.na(end_of_last_positive_period))
  ) %>% 
  arrange(
    desc(n_symptoms),
    desc(`most recent positive report [any_symptom]`),
    desc(`onset of last positive period [earliest over symptoms]`)
  ) %>%
  left_join(p_summary, by=c("patient_id" = "id", "study_no")) %>%
  left_join(a_summary, by="patient_id") %>% 
  dplyr::select(study_no, sex_mismatch, birthyear_diff, twinSN_has_multiple_accounts, everything())

write_csv(candidates, path = sprintf("%s/symptomatic_twins_PerTwinPerSymptom_%s.csv", wdir, timestamp))
write_csv(candidates_summary, path = sprintf("%s/symptomatic_twins_PerTwin_%s.csv", wdir, timestamp))

cat("\n\n---Formatting completed---\n\n")


