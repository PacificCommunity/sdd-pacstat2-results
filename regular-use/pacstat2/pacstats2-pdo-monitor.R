# This script checks the PDH for the number of statistics available relevant to
# the PacStat 2 Project Development Objective (PDO) #1 indicator on quality of
# statisitcs, which relates to the coverage of certain core statistics for
# IDA-eligible Pacific countries
#
# Most of the in-scope statistics are the SDGs and their composite breakdowns,
# but there are a bunch of more upstream statistics that we also want to count
# (e.g. number of people, number of peole with staitsitcs, number of births,
# etc)
#
# This script adds extra rows to the file:
#    data/pacstat2-pdo.csv
#
# which should grow over time and be committed to Git
#
# this script should be run at least once a year but can be done as frequently
# as we want.
#
# Peter Ellis August 2026

# Set up R packages and other functionality
source("setup.R")

# Coverage of the relevant SDGs:
source("regular-use/pacstat2/sdg-coverage.R")

# Coverage of the other statistics:
source("regular-use/pacstat2/pacstat-non-sdgs-coverage.R")

# Make a summary table of the number of observations as now:
latest_update <- tribble(
  ~type                    , ~number_obs                       ,
  "In-scope SDGs"          , nrow(sdgs)                        ,
  "People count"           , nrow(census_pop)                  ,
  "Disabilities"           , nrow(disability)                  ,
  "Labour statistics"      , nrow(labour_stats)                ,
  "Births and deaths"      , nrow(vital_stats) + nrow(dhs_cbr) ,
  "Migration"              , nrow(migrants)                    ,
  "Visitors"               , nrow(visitors)                    ,
  "Remittances"            , nrow(remittances)                 ,
  "Employment by industry" , nrow(employment)                  ,
  "CPI"                    , nrow(cpi)
) |>
  mutate(monitoring_date = format(Sys.Date(), "%Y-%m-%d"))

# Append this to the end of our data file:
datafile <- "data/pacstat2-pdo.csv"

if (file.exists(datafile)) {
  append <- TRUE
} else {
  append <- FALSE
}
write_csv(latest_update, file = datafile, append = append)
