source("setup.R")
source("regular-use/sdg-coverage.R")
source("regular-use/pacstat-non-sdgs-coverage.R")

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
  mutate(monitoring_date = Sys.Date())

datafile <- "output/latest_update.csv"
if (file.exists(datafile)) {
  append <- TRUE
} else {
  append <- FALSE
}
write_csv(latest_update, file = datafile, append = append)
