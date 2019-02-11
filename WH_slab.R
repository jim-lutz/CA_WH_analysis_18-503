# WH_slab.R
# script to plot distribution of WH energy for single family slab on-grade house 
# from CA RECS 2009 data
# started by Jim Lutz "Wed Feb  6 08:02:49 2019"

# set packages & etc
source("setup.R")

# setup  working directories
# use this for scripts 
wd <- getwd()
wd_data    <- paste(wd,"/data/",sep="")      # use this for interim data files
wd_charts  <-paste(wd,"/charts/",sep="")     # use this for charts, ggsave puts in /

# load data
load(file = paste0('data/', "DT_RECS_CA.Rdata"))

# see ../2009 RECS/recs2009_public_codebook.xlsx, ../2009 RECS/public_layout.csv and 
# ../2009 RECS/using-microdata-022613.pdf for information about data

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

# possible fields
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
                  NWEIGHT.y, # number of housing units
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

# cumulative plot
ggplot(data = DT_SLAB_WHNG ) +
  geom_line(aes(x=cumsum(NWEIGHT.y),y=WHtherms)) +
  labs(x = "number of houses (M)", y = "therms/year",
       title = "California Water Heater Energy Use",
       subtitle = "Single-Family Detached, Concrete Foundation, Natural Gas",
       caption = "from 2009 RECS") +
  scale_x_continuous(breaks = c(0,1e6,2e6,3e6,4e6),
                     labels = c("0","1.0","2.0","3.0","4.0"))

# get date to include in file name
d <- format(Sys.time(), "%F")

# save chart
ggsave(filename = paste0("SLAB_WHNG","_",d,".png"), 
       path=wd_charts, scale = 1.5) 

# save data
fwrite(DT_SLAB_WHNG, file = paste0(wd_data,"SLAB_WHNG","_",d,".csv") )


  


  



