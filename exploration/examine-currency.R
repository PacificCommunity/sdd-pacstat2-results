source("setup.R")

# This script is to estimate how "current" the data in PDH.Stat that meets
# PacStat 2 "core statistics" definition. Where currency is defined as within 5
# years, except for CPI which is within 3 months, of today's date.

# These next two files are created when you run the
# regular-use/pacstat2/pacstats2-pdo-monitor.R script. Git ignores them, so you
# need to re-run that in order to have them.
load("data/pacstat_pdo1_snapshot.rda")
load("data/sdg_series_codelist.rda")


latest <- pacstat_pdo1_snapshot |>
  # decided remittances are out of scope
  filter(dataset != "remittances") |>
  mutate(
    combined_country = coalesce(geo_pict, ref_area),
    combined_series = coalesce(series, dataset, indicator)
  ) |>
  group_by(
    combined_series,
    combined_country,
    sex,
    age,
    urbanisation,
    urbanization,
    disability_status,
    economic_sector,
    commodity
  ) |>
  summarise(latest_obs = max(year), .groups = "drop")

# we want all the combinations of sex, age, etc and series that actuall exist
valid_combos <- latest |>
  distinct(
    combined_series,
    sex,
    age,
    urbanisation,
    urbanization,
    disability_status,
    economic_sector,
    commodity
  )

# and we want all the countries that exist:
countries <- latest |>
  distinct(combined_country)

# we want every combination of series, sex and age that exists, for each
# country. And where there was no observation for that series/sex/age combo for
# that country, it gets a latest observation year of 0.
all_combos <- latest |>
  right_join(
    crossing(
      countries,
      valid_combos
    ),
    by = c(
      "combined_country",
      "combined_series",
      "sex",
      "age",
      "urbanisation",
      "urbanization",
      "disability_status",
      "economic_sector",
      "commodity"
    )
  ) |>
  mutate(
    latest_obs = replace_na(latest_obs, 0),
    target_year = ifelse(
      combined_series == "cpi",
      year(Sys.Date()),
      year(Sys.Date()) - 5
    )
  )

all_combos |>
  group_by(combined_country) |>
  summarise(
    number_possible = n(),
    number_present = sum(latest_obs > 0),
    number_current = sum(latest_obs >= target_year),
    prop_current = mean(latest_obs >= target_year)
  ) |>
  arrange(desc(prop_current))


all_combos |>
  summarise(
    number_possible = n(),
    number_present = sum(latest_obs > 0),
    number_current = sum(latest_obs >= target_year),
    prop_current = mean(latest_obs >= target_year)
  )


View(valid_combos)

count(all_combos, target_year)

filter(all_combos, target_year == 2026)

count(pacstat_pdo1_snapshot, indicator, sort = TRUE)
glimpse(pacstat_pdo1_snapshot)

unique(latest$combined_series)
