timestamp=$1
ddir=$2
wdir=$3
mapfile=$4
twins_annofile=$5
max_days_past=$6
max_carry_forward=$7

sdir=$(pwd)

printf "\n\n\n\n\n"
echo "-------------------------------------------------------------------------"
echo "subset_twins.R"
echo "-------------------------------------------------------------------------"
printf "\n\n\n\n\n"

Rscript subset_twins.R $timestamp $ddir $wdir $mapfile $twins_annofile

printf "\n\n\n\n\n"
echo "-------------------------------------------------------------------------"
echo "pipeline.py"
echo "-------------------------------------------------------------------------"
printf "\n\n\n\n\n"


tpat="twins_patients_export_geocodes_${timestamp}.csv"
tass="twins_assessments_export_${timestamp}.csv"
tpatc="cleaned_$tpat"
tassc="cleaned_$tass"

cd $wdir


python3 $sdir/pipeline.py -t GB \
  -p $tpat -a $tass \
  -po $tpatc -ao $tassc

printf "\n\n\n\n\n"
echo "-------------------------------------------------------------------------"
echo "Zoe predictions"
echo "-------------------------------------------------------------------------"
printf "\n\n\n\n\n"

zoe_preds="Zoe_RF2_predictions_$timestamp.csv"
rf2_joblib="$wdir/rf_joblibs/Grouped_RF_2_12_05.joblib"
python3 $sdir/rf_preds.py $tassc $tpatc $zoe_preds $timestamp $rf2_joblib 2

printf "\n\n\n\n\n"
echo "-------------------------------------------------------------------------"
echo "collect_symptomatic_twins.R"
echo "-------------------------------------------------------------------------"
printf "\n\n\n\n\n"

cd $sdir

Rscript collect_symptomatic_twins.R $timestamp $twins_annofile $mapfile $zoe_preds $wdir $max_days_past $max_carry_forward
