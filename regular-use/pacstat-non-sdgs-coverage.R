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

#-------------7. tourists---------------

#-----------------8. remittances----------------

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

# 3566 observations
nrow(cpi)
