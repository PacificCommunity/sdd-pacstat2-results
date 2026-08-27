source("setup.R")

load("data/pacstat_pdo1_snapshot.rda")
load("data/sdg_series_codelist.rda")

#------------Some experiments with baseline growth-------------
total_by_year <- pacstat_pdo1_snapshot |>
  count(year) |>
  arrange(year) |>
  mutate(cumulative = cumsum(n))

# what is the current actual number of observations (28201 as at 26 August 2026)
current_max <- tail(total_by_year, 1)$cumulative

# the four biggest years in terms of adding observations:
total_by_year |>
  arrange(desc(n)) |>
  slice(1:4)
# Note that 2019 was an unusual year with several hundred "national"  SDG
# estimates for that year added in the "SDGs refactoring" project in 2026.
# Putting that unusual year aside, the highest year (as at August 2026) for adding
# observations was 2024 with 1970 then 2023 with 1959. 2025 and 2026 tail off, because
# there is a lag between data being collected and being published

# So let's use the average of recent years since that exceptional year
recent_avg <- total_by_year |>
  filter(year %in% 2020:2024) |>
  summarise(n = mean(n)) |>
  pull(n)

total_by_year |>
  # turn the years 2025 and 2026 into 2031 and 2032 - these will be our taper
  # out years ie if looking at this in 2032, we see low values for these two
  # years as the data has not been full processed and analysed
  mutate(year = ifelse(year <= 2024, year, year + 6)) |>
  # add in years 2025 to 2030 as baseline years
  bind_rows(tibble(year = 2025:2030, n = recent_avg)) |>
  arrange(year) |>
  mutate(
    cumulative = cumsum(n),
    type = ifelse(year <= 2024, "observation", "baseline forecast")
  ) |>
  tail(10)

# This would mean a grwoth of 40% over those six years, suggesting 25% is a bit unambitious:
38244 / current_max


#--------------------annual targets for the 25% growth-----------
#
# one way to do this is to just make them so they are an even growth rate over
# 5.5 years (August 2026 to February 2032) ie:
growth_rate <- 1.25^(1 / 5.5)

tibble(year = 2026:2032, cumulative = current_max * growth_rate^(0:6))
# note that this is a bit more than 25% in total, but it is very close to 25%
# growth from Feb 2027 to Feb 2032

# note that this growth as a percentage doesn't really make sense normally -
# it's not like the existing observations generate more, in fact we'd expect to
# reach a steady state where they can't expand any faster. So what we've got here
# is an approximat way to increase things faster over time

# here is a more ambitious way - I would say too ambitious
growth_rate <- recent_avg / current_max + 1
tibble(year = 2026:2032, cumulative = current_max * growth_rate^(0:6))

#-----------------ways of showing the current indicator set------

glimpse(pacstat_pdo1_snapshot)

pacstat_pdo1_snapshot |>
  count(series, dataset, sort = TRUE) |>
  rename(`Obs in PDH.Stat` = n) |>
  left_join(sdg_series_codelist, by = c("series" = "series_id")) |>
  write_csv("output/pdo1_obs_count.csv")
