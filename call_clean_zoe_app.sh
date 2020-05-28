#!/bin/bash -l

#SBATCH -n 1
#SBATCH -N 1
#SBATCH --mem-per-cpu 350G
#SBATCH -p brc

#SBATCH --time=01-00:00:00    ## "days-hours:minutes:seconds"

#SBATCH -o /scratch/users/k1893262/twinsuk/COVID_radar/slurm_logs/slurm-%j-%x.out-%N

if [ -z $timestamp ]; then
	echo 'No timestamp provided' 1>&2
	exit 1
else
  echo "timestamp: $timestamp"
fi

wdir='/scratch/users/k1893262/twinsuk/COVID_radar/data'
sdir='/scratch/users/k1893262/twinsuk/COVID_radar/zoe-data-prep'
ddir='/mnt/lustre/groups/dtr/covid/'



# check working directory is clean
if [ -z "$(git status --porcelain)" ]; then
  echo "git on branch: $(git rev-parse --abbrev-ref HEAD)"
  echo "git commit hash: $(git rev-parse HEAD)"
else
  echo 'working directory not clean' 1>&2
  git status
  exit 1
fi

module load devtools/python/3.7.3
source ~/local/venv/3.7/bin/activate



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


