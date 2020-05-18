library(data.table) # for efficient reading and merging of large files
library(dplyr)

args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 5)
{
        print("Usage:")
        print("Rscript timestamp datadir workdir mapfile twins_annofile")
        print("")
        print("Note: the results for the day before day2process should be available")
        print("in the datadir folder")
        print("")
        print("Params")
        print("timestamp:   timestamp of the files to analyse")
        print("input datadir:     directory where the file to analyse are located")
        print("work and output datadir:     directory where to save output and intermediate data")
        print("mapfile:     csv file with header; twin IDs in first and app IDs in second column")
        print("twins_annofile: TwinDetails file, see :W/SharedData")
        print("")
}


timestamp <- args[1]
ddir <- args[2]
wdir <- args[3]
mapfile <- args[4]
twins_annofile <- args[5]


setwd(wdir)

print("Loading data")

twins_anno <- fread(twins_annofile) %>% setnames(tolower(names(.)))

id_map <- fread(mapfile)
names(id_map) <- c("TwinSN", "app_id")
setkey(id_map, app_id)

patfile <- paste0("patients_export_geocodes_", timestamp, ".csv")
assessfile <- paste0("assessments_export_", timestamp, ".csv")
testfile <- paste0("covid_test_export_", timestamp, ".csv")
twins_patfile <- paste0("twins_", patfile)
twins_assessfile <- paste0("twins_", assessfile)
twins_testfile <- paste0("twins_", testfile)

print("Subset to TwinsUK participants only")

# patient file
if (file.exists(twins_patfile)){
  cat("There is an existing subsetted patient file with this timestamp", timestamp, ". Existing file preserved, no new output files generated.\n")
} else {
  cat("subsetting patient file\n")
  patient_full <- fread(file.path(ddir,patfile), data.table=T, colClasses="character")
  setkey(patient_full, id)
  patient <- patient_full[id_map]
  rm(patient_full)
  patient[,TwinSN:=NULL]
  fwrite(patient, file = twins_patfile, quote = "auto")
}

# assessment file
if (file.exists(twins_assessfile)){
  cat("There is an existing subsetted assessment file with this timestamp", timestamp, ". Existing file preserved, no new output files generated.\n")
} else {
  cat("subsetting assessment file\n")

  assessment_full <- fread(file.path(ddir,assessfile),data.table=T, colClasses="character")
  setkey(assessment_full, patient_id)
  assessment <- assessment_full[id_map]
  rm(assessment_full)
  assessment[, TwinSN:=NULL]  
  fwrite(assessment, file = twins_assessfile, quote = "auto")
}

# test result file
if (file.exists(twins_testfile)){
  cat("There is an existing subsetted test result file with this timestamp", timestamp, ". Existing file preserved, no new output files generated.\n")
} else {
  cat("subsetting test result file\n")

  test_full <- fread(file.path(ddir,testfile),data.table=T, colClasses="character")
  setkey(test_full, patient_id)
  test <- test_full[id_map]
  rm(test_full)
  test[, TwinSN:=NULL]
  fwrite(test, file = twins_testfile, quote = "auto")
}

