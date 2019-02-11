# load_RASS.R
# script to read RASS data from 
# started by Jim Lutz "Thu Nov 22 14:14:35 2018"

# set packages & etc
source("setup.R")

# set up paths to working directories
wd_RASS <- "2009 RASS/"

# read the Survdata.csv
DT_RASS <-
  fread(file = paste0(wd_RASS,"Survdata.csv"))

# see what's there
length(names(DT_RASS))
# [1] 564
nrow(DT_RASS)
# [1] 25721

# save as an .Rdata file
save(DT_RASS, file = "data/DT_RASS.Rdata")

# look for anything like weight
grep("^W",names(DT_RASS), value = TRUE)
grep("^w",names(DT_RASS), value = TRUE)

# 'wt' is in there.
summary(DT_RASS$wt)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.3   119.7   201.9   448.0   248.4 14703.3 
# looks about right

sum(DT_RASS$wt)
# [1] 11523719
# looks about right

# look at distribution of wt
ggplot(data = DT_RASS, aes(x=1:25721, y=sort(wt))) +
  geom_step() + scale_y_log10() 

# look for pwhfuel3, Cleaned primary water heater fuel
DT_RASS[,list(weight = sum(wt)), by=pwhfuel3 ][order(-weight)]

# does it match Table 4-11: Water Heating Fuel Data Cleaning
# in Volume 1: Methodology
DT_RASS[,list(count = length(wt),
              percent = length(wt)/nrow(DT_RASS)),
        by=pwhfuel3 ][order(pwhfuel3)]
#    pwhfuel3 count      percent
# 1:        1 20047 0.7794020450
# 2:        2  2036 0.0791571090
# 3:        3  1180 0.0458769099
# 4:        4     8 0.0003110299
# 5:        5    20 0.0007775747
# 6:       97   717 0.0278760546
# 7:       99  1713 0.0665992769
# looks close enough to use. 

# look for SEASOCC
grep("SEASOCC",names(DT_RASS), value = TRUE)
# it's there
grep("SEAS",names(DT_RASS), value = TRUE)

# examine SEASOCC responses
DT_RASS[,list(count = length(wt)), by=SEASOCC]
              
