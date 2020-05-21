timestamp=$1
ddir=$2
wdir=$3
mapfile=$4
twins_annofile=$5
onset_window_length=$6
stat_window_length=$7
onset_status_method=$8

# clean version of status method
smc=$(echo $onset_status_method | sed -e 's/%//')
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
echo "link twin ID"
echo "-------------------------------------------------------------------------"
printf "\n\n\n\n\n"

tpatc_linked="linked_$tpatc"
tassc_linked="linked_$tassc"
ttest="twins_covid_test_export_$timestamp.csv"
ttest_linked="linked_$ttest"

python3 $sdir/link_twins_anno.py $tpatc $tassc $tpatc_linked $tassc_linked \
$ttest $ttest_linked $mapfile
cp $tpatc_linked $tassc_linked $ttest_linked $ddir/Twin_Extract
echo "twin ID linking complete; files copied to $ddir/Twin_Extract."

printf "\n\n\n\n\n"
echo "-------------------------------------------------------------------------"
echo "collect_symptomatic_twins.R"
echo "-------------------------------------------------------------------------"
printf "\n\n\n\n\n"

cd $sdir

Rscript collect_symptomatic_twins.R $timestamp $twins_annofile $mapfile \
$wdir $onset_window_length $stat_window_length 'any' $onset_status_method

new_onsetfile="new_onset_onset${onset_window_length}_stat${stat_window_length}_${smc}_$timestamp.csv"

printf "\n\n\n\n\n"
echo "-------------------------------------------------------------------------"
echo "Zoe predictions"
echo "-------------------------------------------------------------------------"
printf "\n\n\n\n\n"

cd $wdir

if [ $onset_window_length -lt 4 ]
then
  zoe_preds="Zoe_RF2_predictions_onset${onset_window_length}_stat${stat_window_length}_${smc}_$timestamp.csv"
  rf_joblib="rf_joblibs/Grouped_RF_${onset_window_length}_12_05.joblib"

  echo $tassc $tpatc $zoe_preds $timestamp $rf_joblib
    
  python3 $sdir/rf_preds.py $tassc $tpatc $ttest $zoe_preds $new_onsetfile $mapfile \
  $timestamp $rf_joblib $onset_window_length 0 1 5
else
  echo 'RF model not applicable for onset window length above 3 days. Predictions skipped.'
  zoe_preds="skipped"
fi

printf "\n\n\n\n\n"
echo "-------------------------------------------------------------------------"
echo "plot results"
echo "-------------------------------------------------------------------------"
printf "\n\n\n\n\n"

cd $sdir
hist_plot_file="new_onset_history_onset${onset_window_length}_stat${stat_window_length}_${smc}_$timestamp.pdf"

Rscript plot_new_onset_histories.R $timestamp $zoe_preds $new_onsetfile $wdir \
$onset_window_length $stat_window_length $hist_plot_file 0 1

echo 'Wrapper script completed.'
