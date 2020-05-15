import pandas as pd
import joblib
import matplotlib.pyplot as plt
import numpy as np
import sklearn
import datetime
import sys

#####################
# SETUP
#####################

a_path = sys.argv[1]
p_path = sys.argv[2]
predfile = sys.argv[3]
timestamp = sys.argv[4]
rf2_filename= sys.argv[5]
N= sys.argv[6]

timestamp_date = datetime.datetime.strptime(timestamp[0:8], '%Y%m%d')
clf, idx_optimal, idx_high_sens, idx_high_spec, thresholds, fpr, tpr, FEATURES, ALL_SYMPTOMS, PAT_FEATURES = joblib.load(rf2_filename)
idx = idx_high_sens
# ALL_SYMPTOMS.remove("fatigue")
# ALL_SYMPTOMS.remove("shortness_of_breath")
# ALL_SYMPTOMS.extend(["fatigue_binary", "shortness_of_breath_binary"])
# FEATURES = ALL_SYMPTOMS + PAT_FEATURES

#####################
# EXPORT FEATURE IMPORTANCES
#####################

# importances = pd.concat([pd.Series(FEATURES), pd.Series(clf.best_estimator_.feature_importances_)], axis=1)
# importances.columns = ['features', 'importance']
# importances = importances.sort_values('importance', ascending=False)
# importances.to_csv("/home/simon/OneDrive/KCL/Falchi/phd/COVID_radar/results/datateam/zoe_feature_importance.csv",
#                    index=False)

#####################
# LOAD DATA
#####################

a = pd.read_csv(a_path, low_memory=False)
p = pd.read_csv(p_path, low_memory=False)

#####################
# PRE-PROCESSING
#####################

# recode non-binary symptoms to numerical values
a.replace(to_replace={'shortness_of_breath': ['no', 'mild', 'significant', 'severe'],
                      'fatigue': ['no', 'mild', 'severe']},
          value={'shortness_of_breath': [0, 1, 2, 3],
                 'fatigue': [0, 1, 2]},
          inplace=True)

# subset to N days before timestamp date
# a["date_updated_at"] = datetime.datetime.strptime(a["updated_at"].values, "%Y-%m-%d %H:%M:%S")
a = a.loc[~a["updated_at"].str.contains("--")]
a["date_updated_at"] = pd.to_datetime(a["updated_at"])
a_recent = a.loc[timestamp_date-a["date_updated_at"]< pd.Timedelta(N, unit="days")]

# calculated values
p["prisma"] = (p["age"]>85).astype(int) + (p["gender"]==0).astype(int) + p["needs_help"].astype(int) + \
              p["housebound_problems"].astype(int) + p["help_available"].astype(int) + p["mobility_aid"].astype(int)
p["hcw"] = p[["have_worked_in_hospital_care_facility", "have_worked_in_hospital_clinic",
              "have_worked_in_hospital_home_health", "have_worked_in_hospital_inpatient", "have_worked_in_hospital_other",
              "have_worked_in_hospital_outpatient", "have_worked_in_hospital_school_clinic",
              "contact_health_worker"]].any(axis=1, skipna=True)

# summaries per individual
af = ALL_SYMPTOMS + ['patient_id']
asum = a_recent[af].groupby('patient_id').agg(lambda x: x.sum()/N)

pf = PAT_FEATURES + ['id']
psum = p[pf].drop_duplicates()

# model input
X_test = psum.merge(asum, left_on="id", right_on="patient_id", how="inner")

#####################
# RUN MODEL
#####################

X_test.loc[:, 'p_predicted_covid'] = clf.predict_proba(X_test[FEATURES])[:,1]
X_test.loc[:, 'predicted_covid_spec'] = X_test['p_predicted_covid']>thresholds[idx_high_spec]
X_test.loc[:, 'predicted_covid_opt'] = X_test['p_predicted_covid']>thresholds[idx_optimal]
X_test.loc[:, 'predicted_covid_sens'] = X_test['p_predicted_covid']>thresholds[idx_high_sens]
X_test = X_test.sort_values("p_predicted_covid", ascending=False)

X_test.to_csv(predfile, index=False)

