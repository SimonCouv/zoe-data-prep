timestamp=$1
ddir=$2
wdir=$3
mapfile=$4
twins_annofile=$5

sdir=$(pwd)

source ~/local/venv/3.6/bin/activate
# python3 -m pip install numpy

echo "---------------------------------------------"
echo "subset_twins.R"
echo "---------------------------------------------"

Rscript subset_twins.R $timestamp $ddir $wdir $mapfile $twins_annofile


echo "---------------------------------------------"
echo "pipeline.py"
echo "---------------------------------------------"

tpat="twins_patients_export_geocodes_${timestamp}.csv"
tass="twins_assessments_export_${timestamp}.csv"
tpatc="cleaned_$tpat"
tassc="cleaned_$tass"

cd $wdir


python3 $sdir/pipeline.py -t GB \
  -p $tpat -a $tass \
  -po $tpatc -ao $tassc


echo "---------------------------------------------"
echo "collect_symptomatic_twins.R"
echo "---------------------------------------------"

cd $sdir

Rscript collect_symptomatic_twins.R $timestamp $twins_annofile $mapfile $wdir
