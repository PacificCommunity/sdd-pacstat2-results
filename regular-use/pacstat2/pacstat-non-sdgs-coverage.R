# This script is for estimating coverage of statistics other than SDGs that are
# relevant to the PacStat 2 PDO

# These are:
# 2. number of people usually resident (by sex and by rural/urban);
#
# 3. number of people with disabilities (by sex and by rural/urban);
#
# 4. size of participating labour force, formally employed, informally employed and
# unemployed (by sex, by broad age group);
#
#  5. births (by sex and by age group of mother) and deaths (by sex and by age
#  group);
#
# 6. long term migrant arrivals and departures (by sex);
#
# 7. tourist or visitor arrivals and departures (by sex);
#
# 8. remittances;
#
# 9. employment (by broad industry); and
#
# 10. consumer price index (by division).

# counting starts at #2 so #1 can be the SDGs (separate script)

#adding source just in case anyone wants to start with this script
source("setup.R")

#------------2. Number people resident------------
#
# We have a number for this of course in the poulation projections, but the
# intent behind this indicator is to count the actual numbers reported by the
# NSO, not modelled by the UN. In the absence of anything of this sort in
# PDH.Stat that I can find we report nothing on this (hopefully will fix this by
# creating standard census values)

census_pop <- tibble()

#------------3. People with disabilities------------
disability <- readSDMX(
  providerId = "PDH",
  resource = "data",
  flowRef = "DF_DISABILITY"
) |>
  as_tibble() |>
  clean_names() |>
  filter(geo_pict %in% ida_picts) |>
  # the data from PDH is large (90,000 observations) because so many different
  # cut-offs, etc. we only care about the number of combinations of country, time,
  # sex, age and urbanisation; not that there is a vector of values for each some
  # combination
  distinct(geo_pict, freq, sex, age, urbanization, obs_time)

#-----------4. participating labour force, employed, etc------
#
# this is very weak at the moment. dataflow below has unemployment and
# participation rate and seems to be the only one in PDH (even though could get
# these from HIES too). Latest observation is 2021.
labour_stats <- readSDMX(
  providerId = "PDH",
  resource = "data",
  flowRef = "DF_EMPRATES"
) |>
  as_tibble() |>
  clean_names() |>
  filter(geo_pict %in% ida_picts) |>
  distinct(freq, geo_pict, indicator, sex, age, urbanization, obs_time)

#-----------5. births and deaths----------------- We have a couple of sources of
# these
# -  'health indicators from the DHS'
# - 'Vital statistics'
#
# Spirit of PacStat2 we would count them both, because the whole point is to be
# agnostic about where the data come from but choose the best, most frequent
# source (so set up incentive to replace expensive surveys with cheaper admin
# data)

# data quality is very poor in this series, but that's another matter doesn't
# actually give number of births and deaths but does have crude birth rate and
# crude death rate, which we will treat as good enough for our purposes
vital_stats <- readSDMX(
  providerId = "PDH",
  resource = "data",
  flowRef = "DF_VITAL"
) |>
  as_tibble() |>
  clean_names() |>
  filter(indicator %in% c("CBR", "CDR"), geo_pict %in% ida_picts) |>
  distinct(freq, geo_pict, indicator, sex, obs_time)


# only one indicator relevant in the DHS set so we specifically download that one only
dhs_cbr <- readSDMX(
  "https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_HEALTH,1.0/A...FER_05..?dimensionAtObservation=AllDimensions"
) |>
  as_tibble() |>
  clean_names() |>
  filter(geo_pict %in% ida_picts) |>
  distinct(geo_pict, indicator, sex, age, time_period)

#--------------6. migrant arrivals and departures-----------
# No data available on this in PDH.Stat

migrants <- tibble()

#-------------7. tourists---------------
#
# PDH has "Tourism arrivals" and "Overseas visitors arrivals". The latter has
# more data and is current to 2025 (still not good enough! countries have
# published 2026 versions)

visitors <- readSDMX(
  providerId = "PDH",
  resource = "data",
  flowRef = "DF_OVERSEAS_VISITORS"
) |>
  as_tibble() |>
  clean_names() |>
  filter(geo_pict %in% ida_picts) |>
  # only counting total numbers, don't want to give extra points for extra granularity
  distinct(freq, geo_pict, obs_time)


#-----------------8. remittances----------------
#
# These are SDG 17.3.2 but were not caught up in the general SDGs approach
# earlier because generally SDG 17 "partnership for the goals" not obviously
# relevant.

remittances <- readSDMX(
  providerId = "PDH",
  resource = "data",
  flowRef = "DF_SDG_17"
) |>
  as_tibble() |>
  clean_names() |>
  filter(series == "BX_TRF_PWKR", ref_area %in% ida_picts) |>
  # we don't care about occupation, custom breakdown and so on
  distinct(freq, ref_area, sex, age, obs_time)

#------------------9.employment by industry-------------

employment <- readSDMX(
  providerId = "PDH",
  resource = "data",
  flowRef = "DF_EMPLOYED"
) |>
  as_tibble() |>
  clean_names() |>
  filter(geo_pict %in% ida_picts) |>
  # there are lots of extra breakdowns we don't want to count eg disability,
  # education, occupation, so we just want the following unique combos:
  distinct(geo_pict, freq, economic_sector, obs_time)

#-------------------10. CPI by division-------------
#
# Note that the CPI includes both monthly and quarterly data, and including
# monthly in the count increases it substantially. So if future countries move
# to country this indicator will go up, and CPI is a substantial proportion of
# the total indicator count. This is intended behaviour as a stated aim of the
# project is to include frequency and timeliness of statistics

cpi <- readSDMX(
  providerId = "PDH",
  resource = "data",
  flowRef = "DF_CPI"
) |>
  as_tibble() |>
  clean_names() |>
  filter(geo_pict %in% ida_picts) |>
  # excluding PERCENT because it is not really an additional observation
  filter(unit_measure == "INDEX")
