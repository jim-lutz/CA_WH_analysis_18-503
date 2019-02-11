# load_RECS.R
# script to read RECS 2009 data from ../2009 RECS/recs2009_public.csv
# and ../2009 RECS/recs2009_public_repweights.csv
# see ../2009 RECS/public_layout.csv for variable names
# and  ../2009 RECS/recs2009_public_codebook.xlsx for values of variables
# started by Jim Lutz "Fri Feb  1 07:30:01 2019"

# set packages & etc
source("setup.R")

# set up paths to working directories
wd_RECS <- "2009 RECS/"

# read the recs2009_public.csv
DT_RECS <-
  fread(file = paste0(wd_RECS,"recs2009_public.csv"))

# see what's there
length(names(DT_RECS))
# [1] 940
nrow(DT_RECS)
# [1] 12083

# read the recs2009_public_repweights.csv
DT_RECS_repweights <-
  fread(file = paste0(wd_RECS,"recs2009_public_repweights.csv"))

# see what's there
length(names(DT_RECS_repweights))
# [1] 246
nrow(DT_RECS_repweights)
# [1] 12083

# merge on DOEID
DT_RECS <-
  merge(DT_RECS, DT_RECS_repweights, by='DOEID')

# see what's there
length(names(DT_RECS))
# [1] 1185
nrow(DT_RECS)
# [1] 12083

# keep only California data
DT_RECS_CA <-
  DT_RECS[ REPORTABLE_DOMAIN==26,]

# see what's there
length(names(DT_RECS_CA))
# [1] 1185
nrow(DT_RECS_CA)
# [1] 1606

# save as an .Rdata file
save(DT_RECS_CA, file = "data/DT_RECS_CA.Rdata")

