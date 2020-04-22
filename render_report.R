########################################################################
## generate report
########################################################################

args <- commandArgs(trailingOnly = TRUE)
timestamp <- args[1]
wdir <- args[2]
rdir <- args[3]

rmarkdown::render(
  input = "twin_participation_report.Rmd",
  params = list(timestamp = timestamp, wdir=wdir, rdir=rdir),
  output_file = sprintf("twinsuk_participation_report_%s.pdf", timestamp),
  output_dir = rdir
)
