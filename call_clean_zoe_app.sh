#!/bin/bash

#SBATCH -n 1
#SBATCH -N 1
#SBATCH --mem-per-cpu 180G
#SBATCH -p brc

#SBATCH --time=00-03:00:00    ## "days-hours:minutes:seconds"

#SBATCH -o /scratch/users/k1893262/twinsuk/COVID_radar/slurm_logs/slurm-%j-%x.out-%N

timestamp=$1
if [ -z $timestamp ]; then
	echo 'No timestamp provided'
	exit
fi



module load apps/R/3.6.0
module load devtools/python/3.7.3
source ~/local/venv/3.7/bin/activate

wdir='/scratch/users/k1893262/twinsuk/COVID_radar/data'
sdir='/scratch/users/k1893262/twinsuk/COVID_radar/zoe-data-prep'
ddir='/mnt/lustre/groups/dtr/covid/'




pat="patients_export_geocodes_${timestamp}.csv"
ass="assessments_export_${timestamp}.csv"
patc="cleaned_$pat"
assc="cleaned_$ass"

cd $wdir

python3 $sdir/pipeline.py -t GB \
  -p "$ddir/$pat" -a "$ddir/$ass" \
	-po "$wdir/$patc" -ao "$wdir/$assc"
	-t 'GB'
	-ps 1


