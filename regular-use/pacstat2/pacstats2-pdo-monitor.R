# This script checks the PDH for the number of statistics available relevant to
# the PacStat 2 Project Development Objective (PDO) #1 indicator on quality of
# statisitcs, which relates to the coverage of certain core statistics for
# IDA-eligible Pacific countries
#
# Most of the in-scope statistics are the SDGs and their composite breakdowns,
# but there are a bunch of more upstream statistics that we also want to count
# (e.g. number of people, number of people with disabilities, number of births,
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
  # if data exists, in the usual state will just want to append our new rows:
  append <- TRUE

  # But we have to check. What if we've already run this today - we don't want
  # to keep adding multiple observations for today:
  current_data <- read_csv(datafile)
  if (Sys.Date() %in% current_data$monitoring_date) {
    warning("There is already an observation for today and it will be removed")

    latest_update <- current_data |>
      filter(monitoring_date != Sys.Date()) |>
      rbind(latest_update)

    # set append to FALSE as now we are over-writing the whole file with our corrected version
    append <- FALSE
  }
} else {
  append <- FALSE
}
write_csv(latest_update, file = datafile, append = append)

#----------------save a snapshot of the actual data for use in exploratory analysis-------------

# This is a snapshot of the dimensions of the data (so not actual observation
# values, but all the unique)combinations of date, country, sex, age, indicator,
# etc) which we will want to use for analysis such as what dates are covered,
# which indicators appear most often, etc.
pacstat_pdo1_snapshot <- bind_rows(
  mutate(sdgs, dataset = "sdgs"),
  mutate(census_pop, dataset = "census_pop"),
  mutate(disability, dataset = "disability"),
  mutate(labour_stats, dataset = "labour_stats"),
  mutate(vital_stats, dataset = "vital_stats"),
  mutate(dhs_cbr, dataset = "dhs_cbr"),
  mutate(migrants, dataset = "migrants"),
  mutate(visitors, dataset = "visitors"),
  mutate(remittances, dataset = "remittances"),
  mutate(employment, dataset = "employment"),
  mutate(cpi, dataset = "cpi")
) |>
  # Caution - obs_time is sometimes a four character year, and sometimes a 7
  # character year-month eg "2026-10" (that is for monthly CPI) and sometimes
  # "2025-Q4". But the first four characters are always year (so far...)
  mutate(year = as.numeric(substring(obs_time, 1, 4))) |>
  glimpse()

# This data is largish and not of enough interest to compare at different points
# in time so we are going to save it as a binary object and have Git ignore it.
save(pacstat_pdo1_snapshot, file = "data/pacstat_pdo1_snapshot.rda")
