timestamp=$1
ddir=$2
wdir=$3
mapfile=$4
twins_annofile=$5
onset_window_length=$6
stat_window_length=$7


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
$wdir $onset_window_length $stat_window_length 'any'

printf "\n\n\n\n\n"
echo "-------------------------------------------------------------------------"
echo "Zoe predictions"
echo "-------------------------------------------------------------------------"
printf "\n\n\n\n\n"

cd $wdir

new_onsetfile="new_onset_$timestamp.csv"
zoe_preds="Zoe_RF2_predictions_$timestamp.csv"
rf2_joblib="rf_joblibs/Grouped_RF_2_12_05.joblib"

echo $tassc $tpatc $zoe_preds $timestamp $rf2_joblib

python3 $sdir/rf_preds.py $tassc $tpatc $zoe_preds $new_onsetfile $mapfile \
$timestamp $rf2_joblib $onset_window_length 0 1

printf "\n\n\n\n\n"
echo "-------------------------------------------------------------------------"
echo "plot results"
echo "-------------------------------------------------------------------------"
printf "\n\n\n\n\n"

cd $sdir
Rscript plot_new_onset_histories.R $timestamp $mapfile $zoe_preds $wdir \
$onset_window_length $stat_window_length

echo 'Wrapper script completed.'
