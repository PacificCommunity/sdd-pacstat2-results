source("setup.R")

# This script is to estimate how "current" the data in PDH.Stat that meets
# PacStat 2 "core statistics" definition. Where currency is defined as within 5
# years, except for CPI which is within 3 months, of today's date.

# These next two files are created when you run the
# regular-use/pacstat2/pacstats2-pdo-monitor.R script. Git ignores them, so you
# need to re-run that in order to have them.

# TODO - the intent is to count CPI as not current if it is 3 months out of
# date. But the approach below is much cruder, it does this if the most recent
# observation is not in the current year. If it's March, this works, but
# otherwise it will get a different result. So the TODO is to fix this, which
# will involve mucking around with the horrible types of dates in the original
# CPI data (eg 2025Q4 and similar)

load("data/pacstat_pdo1_snapshot.rda")
load("data/sdg_series_codelist.rda")


latest <- pacstat_pdo1_snapshot |>
  # decided remittances are out of scope
  filter(dataset != "remittances") |>
  mutate(
    combined_country = coalesce(geo_pict, ref_area),
    # if series exists, then indicator doesn't and vice versa. series exists for
    # SDGs. indicator exists when a non-SDG dataset has more than one indicator.
    # Between them, the next line of code gives us a unique idnetifer of the
    # series-indicator-dataset combination:
    combined_series = paste(coalesce(series, indicator), dataset)
  ) |>
  group_by(
    combined_series,
    combined_country,
    sex,
    age,
    urbanisation,
    disability_status,
    economic_sector,
    commodity
  ) |>
  summarise(
    latest_date = max(obs_date),
    .groups = "drop"
  )

# we want all the combinations of sex, age, other composite breakdowns, and
# series that actually exist::
valid_combos <- latest |>
  distinct(
    combined_series,
    sex,
    age,
    urbanisation,
    disability_status,
    economic_sector,
    commodity
  ) |>
  bind_rows(
    expand.grid(
      combined_series = "population from census",
      sex = c("_T", "M", "F"),
      age = LETTERS[1:8]
    ),
    expand.grid(combined_series = "migration", sex = c("_T", "M", "F"))
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
      "disability_status",
      "economic_sector",
      "commodity"
    )
  ) |>
  mutate(
    # if there's no data, we say the latest observation was in year zero:
    latest_date = replace_na(latest_date, as.Date("1000-01-01")),
    target_date = if_else(
      combined_series == "IDX cpi",
      ymd(Sys.Date() - 120),
      ymd(paste0(year(Sys.Date()) - 5, "-01-01"))
    )
  )

#-------------------------what are the results - how many indicators are current and how many not?------------------
glimpse(all_combos)
arrange(all_combos, latest_date)


# In total:
currency_total <- all_combos |>
  summarise(
    number_possible = n(),
    number_present = sum(latest_date > as.Date("1500-01-01")),
    number_current = sum(latest_date >= target_date),
    prop_current = mean(latest_date >= target_date)
  )

# By country:
currency_country <- all_combos |>
  group_by(combined_country) |>
  summarise(
    number_possible = n(),
    number_present = sum(latest_date > "1500-01-01"),
    number_current = sum(latest_date >= target_date),
    prop_current = mean(latest_date >= target_date)
  ) |>
  arrange(desc(prop_current))

currency_target <- 0.38
mi <- max(currency_country$number_possible)

p1 <- currency_country |>
  ggplot(aes(x = number_present, y = prop_current)) +
  # average line
  geom_hline(yintercept = currency_total$prop_current, colour = "darkred") +
  geom_hline(yintercept = currency_target, colour = "darkred") +
  geom_point(colour = spc_cols(2)) +
  geom_text_repel(
    aes(label = combined_country),
    colour = spc_cols(1),
    seed = 42
  ) +
  scale_y_continuous(label = percent) +
  expand_limits(y = 0:1, x = mi) +
  labs(
    x = "Number of actual indicators available in PDH.Stat",
    y = "Proportion of all indicators that are 'current'",
    title = "A new target for IDA countries - 38% of indicators 'current' by 2032",
    subtitle = "Current usually means 5 years old or less"
  )

svg("output/currency-by-country-proportion.svg", width = 8, height = 6)
print(p1)
dev.off()


p2 <- currency_country |>
  ggplot(aes(x = number_present, y = number_current)) +
  geom_hline(yintercept = currency_target * mi, colour = "darkred") +
  geom_point(colour = spc_cols(2), size = 4) +
  geom_text_repel(
    aes(label = combined_country),
    colour = spc_cols(1),
    seed = 42,
    size = 4
  ) +
  annotate(
    "text",
    x = 200,
    y = currency_target * mi + 20,
    label = "2032 target for recent data to be available",
    colour = "darkred"
  ) +
  expand_limits(y = c(0, mi), x = c(0, mi)) +
  labs(
    x = "Number of actual indicators available in PDH.Stat\n(out of 803 potential indicators as at late 2026)",
    y = "Number that are 'current'",
    title = "A new target for IDA countries - 38% of indicators 'current' by 2032",
    subtitle = "'Current' usually 5 years old or less for most indicators; 3 months for CPI.",
    caption = "Source: SPC analysis using PDH.Stat"
  ) +
  coord_equal()

svg("output/currency-by-country.svg", width = 6, height = 6.5)
print(p2)
dev.off()


#--------------------------by indicator type--------------
all_combos |>
  mutate(series_type = gsub(".*\\s", "", combined_series)) |>
  group_by(series_type) |>
  summarise(
    number_possible = n(),
    number_present = sum(latest_date > "1500-01-01"),
    number_current = sum(latest_date >= target_date),
    prop_current = mean(latest_date >= target_date)
  ) |>
  arrange(desc(number_possible))
