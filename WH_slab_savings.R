# WH_slab_savings.R
# script to estimate potential numbers and savings for improvements to
# under-slab plumbing.
# show under-slab and under-slab w/ recirc on 
# plot of distribution of annual WH energy for single family slab on-grade house 
# from CA RECS 2009 data
# started by Jim Lutz "Fri Feb  8 17:32:25 2019"

# set packages & etc
source("setup.R")

# setup  working directories
# use this for scripts 
wd <- getwd()
wd_data    <- paste(wd,"/data/",sep="")      # use this for interim data files
wd_charts  <-paste(wd,"/charts/",sep="")     # use this for charts, ggsave puts in /

# get date to include in file name
d <- format(Sys.time(), "%F")

# load data
load(file = paste0('data/', "DT_RECS_CA.Rdata"))

# see data/2009 RECS/recs2009_public_codebook.xlsx, data/2009 RECS/public_layout.csv and 
# data/2009 RECS/using-microdata-022613.pdf for information about data

# factors for TYPEHUQ	Type of housing unit	
DT_RECS_CA[ ,F_TYPEHUQ:= factor(x=TYPEHUQ,
                                levels = c(2,3,4,5,1), # Single-Family first
                                labels = c('Single-Family Detached', 
                                           'Single-Family Attached', 
                                           'Apartment in Building with 2 - 4 Units', 
                                           'Apartment in Building with 5+ Units',
                                           'Mobile Home')
)
]

# factors for YEARMADERANGE	Year range when housing unit was built
DT_RECS_CA[ , 
            F_YEARMADERANGE:= factor(x=YEARMADERANGE,
                                     levels = c('1', '2', '3', '4', '5', '6', '7', '8'),
                                     labels = c('Before 1950', '1950 to 1959', '1960 to 1969',
                                                '1970 to 1979', '1980 to 1989', '1990 to 1999',
                                                '2000 to 2004', '2005 to 2009')
            )
            ]

# factors for CONCRETE	Housing unit over a concrete slab
DT_RECS_CA[ , 
            F_CONCRETE:= factor(x=CONCRETE,
                                     levels = c('0', '1', '-2'),
                                     labels = c('No', 'Yes', 'Not Applicable')
                                )]

# factors for FUELH2O	Fuel used by main water heater	
DT_RECS_CA[ , 
            F_FUELH2O:= factor(x=FUELH2O,
                                levels = c('1', '2', '3', '4', '5', '7', '8', '21', '-2'),
                                labels = c("Natural Gas", 
                                           "Propane/LPG",
                                           "Fuel Oil", 
                                           "Kerosene", "Electricity",
                                           "Wood", 
                                           "Solar", 
                                           "Other Fuel", 
                                           "Not Applicable")
                                )]

# other possible fields
# PGASHTWA	Who pays for natural gas for water heating
# CUFEETNGWTH	Natural Gas usage for water heating, in hundred cubic feet, 2009
# BTUNGWTH	Natural Gas usage for water heating, in thousand BTU, 2009

# make sure right factors are in data.table
grep("F_",names(DT_RECS_CA), value = TRUE)


# make a simple data.table to plot
DT_SLAB_WHNG <-
  DT_RECS_CA[F_TYPEHUQ    == "Single-Family Detached" & 
               F_CONCRETE == "Yes" & 
               F_FUELH2O    == "Natural Gas",
             
             list(DOEID, #	Unique identifier for each respondent
                  F_TYPEHUQ,
                  F_CONCRETE,
                  F_FUELH2O,
                  NWEIGHT.y = round(NWEIGHT.y), # number of housing units
                  WHtherms = BTUNGWTH/100) # 1000s BTU for WH fuel / 100
             ]           
             
# check out DT_SLAB_WHNG
sum(DT_SLAB_WHNG$NWEIGHT.y)
# [1] 4150964
summary(DT_SLAB_WHNG$WHtherms)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   21.92  130.54  177.29  201.09  239.58 1560.23 

# sort by WHtherms
setkey(DT_SLAB_WHNG,WHtherms)

# cumulative number of houses
DT_SLAB_WHNG[ , cumWEIGHT := cumsum(NWEIGHT.y)]

# look at the extreme tail
tail(DT_SLAB_WHNG$WHtherms)
# [1]  638.5800  678.5500  686.9734  723.0842  777.9800 1560.2337

DT_SLAB_WHNG[WHtherms %in% tail(DT_SLAB_WHNG$WHtherms),
             list(DOEID, NWEIGHT.y, WHtherms, cumWEIGHT)]
#    DOEID NWEIGHT.y  WHtherms cumWEIGHT
# 1:  7478      6799  638.5800   4116538
# 2: 12073      6767  678.5500   4123305
# 3:  8478      7571  686.9734   4130876
# 4:  4141      6821  723.0842   4137697
# 5: 11157      6799  777.9800   4144496
# 6:  1958      6398 1560.2337   4150894
# highest WHtherms house uses more than 2X next highest house. 
# remove it.
DT_SLAB_WHNG <- DT_SLAB_WHNG[DOEID!='1958']

# assume under-slab pipe is everything 300-500 therms
WHtherm.300.500 <- NULL

# weighted average annual therms
WHtherm.300.500$ave.WHtherms <- 
  DT_SLAB_WHNG[300 <= WHtherms & WHtherms <= 500, 
               list(average=weighted.mean(WHtherms, NWEIGHT.y))]$average

# min WHtherms and min cumWEIGHT above 300 WHtherms
WHtherm.300.500 <-
  c(WHtherm.300.500,
    DT_SLAB_WHNG[WHtherms > 300, list(min.WHtherms = min(WHtherms), 
                                  min.cumWEIGHT = min(cumWEIGHT))]
    )

# max WHtherms  and max cumWEIGHT below 500
WHtherm.300.500 <-
  c(WHtherm.300.500,
    DT_SLAB_WHNG[WHtherms < 500, list(max.WHtherms = max(WHtherms), 
                                  max.cumWEIGHT = max(cumWEIGHT))]
  )

# number houses in under.slab
WHtherm.300.500$num.cumWEIGHT <-
  WHtherm.300.500$max.cumWEIGHT - WHtherm.300.500$min.cumWEIGHT

WHtherm.300.500
# $ave.WHtherms
# [1] 359.1263
# 
# $min.WHtherms
# [1] 300.5467
# 
# $min.cumWEIGHT
# [1] 3642375
# 
# $max.WHtherms
# [1] 488.8534
# 
# $max.cumWEIGHT
# [1] 4046315
# 
# $num.cumWEIGHT
# [1] 403940

# under-slab box, counterclockwise from lower left corner
DT_300.550.box <-
  with(WHtherm.300.500,
      data.table(x=c(min.cumWEIGHT,max.cumWEIGHT,max.cumWEIGHT,min.cumWEIGHT,min.cumWEIGHT),
                 y=c(300,300,500,500,300))
    )

DT_300.550.box
#          x   y
# 1: 3642375 300
# 2: 4046315 300
# 3: 4046315 500
# 4: 3642375 500
# 5: 3642375 300

# assume under-slab pipe w/recirc is everything above 500 therms
# weighted average annual therms
DT_SLAB_WHNG[500 < WHtherms & WHtherms <= 1000, 
             list(average=weighted.mean(WHtherms, NWEIGHT.y))]
#     average
# 1: 600.1498

# min WHtherms above 500
DT_SLAB_WHNG[WHtherms > 500, list(WHtherms = min(WHtherms), 
                                  cumWEIGHT = min(cumWEIGHT))]
#    WHtherms cumWEIGHT
# 1: 503.5757   4052972

# max WHtherms below 1000
DT_SLAB_WHNG[WHtherms < 1000, list(WHtherms = max(WHtherms), 
                                  cumWEIGHT = max(cumWEIGHT))]
# WHtherms cumWEIGHT
# 1:   777.98   4144565

# weight under.slab
under.slab.recirc.nhouses <- 4144565 - 4052972

# under-slab recirc box, counterclockwise from lower left corner
DT_under.slab.recirc <- 
  data.table(x=c(4052972,4.2e6,4.2e6,4052972,4052972),
             y=c(500,500,1000,1000,500))

# cumulative plot
ggplot(data = DT_SLAB_WHNG ) +
  geom_line(aes(x=cumsum(NWEIGHT.y),y=WHtherms)) + 
  geom_path(data = DT_under.slab,  # under.slab box
            aes(x,y),
            color="red") +
  geom_text(x=3500000, y=400, hjust=1,
            label=paste0("under-slab: ", 
                        under.slab.weight, " houses, ",
                        "average = 359 therms"),
            size=5, family="serif", fontface="italic") +
  geom_path(data = DT_under.slab.recirc,  # under.slab w/ recirc box
            aes(x,y),
            color="green") +
  geom_text(x=3750000, y=750, hjust=1,
            label=paste0("under-slab w/recirc: ", 
                         under.slab.recirc.nhouses, " houses, ",
                         "average = 600 therms"),
            size=5, family="serif", fontface="italic") +
  labs(x = "number of houses (M)", y = "therms/year",
       title = "California Water Heater Energy Use",
       subtitle = "Single-Family Detached, Concrete Foundation, Natural Gas",
       caption = "from 2009 RECS") +
  scale_x_continuous(breaks = c(0,1e6,2e6,3e6,4e6),
                     labels = c("0","1.0","2.0","3.0","4.0"))

# save chart
ggsave(filename = paste0("SLAB_WHNG_therms","_",d,".png"), 
       path=wd_charts, scale = 1.5) 

# save data
fwrite(DT_SLAB_WHNG, file = paste0(wd_data,"SLAB_WHNG_therms","_",d,".csv") )


  


  



