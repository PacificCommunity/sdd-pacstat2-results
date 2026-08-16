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

#------------2. Number people resident------------

#------------3. People with disabilities------------

#-----------4. participating labour force------

#-----------5. births-----------------

#--------------6. migrant arrivals and departures-----------
# No data available on this in PDH.Stat

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
  # only our series of interest and countries of interest. Note that ida_picts
  # is defined in a script in the /R/ folder and is the ISO2 codes for PICTs
  # that are members of IDA, the World bank soft loan arm:
  filter(geo_pict %in% ida_picts)

# 304 observations in August 2026. An easy way to increase this would be to
# include monthly data, as indeed we should.
nrow(visitors)


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
  # only our series of interest and countries of interest. Note that ida_picts
  # is defined in a script in the /R/ folder and is the ISO2 codes for PICTs
  # that are members of IDA, the World bank soft loan arm:
  filter(series == "BX_TRF_PWKR", ref_area %in% ida_picts)

# note this counts twice, once for in units and once as % of GDP. this is ok as
# these are indeed two separate things and knowing one doesn't automatically
# give you the other (some difficult conversion decisions needed)

# 214 observations August 2026
nrow(remittances)

#------------------9.employment by industry-------------

#-------------------10. CPI by division-------------
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

# 3566 observations August 2026
nrow(cpi)
