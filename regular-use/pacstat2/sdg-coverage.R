# Script to download and count coverage of key SDG indicators for the Pacific

# The indicator is "Number of observations against core statistics and their
# composite breakdowns, for IDA countries, that are available in a regional
# dissemination platform"

# The relevant core statistics are "SDGs 1 to 10, 16 & 17.19.2 and which are
# reported by country"
source("setup.R")

options(timeout = 300) # 5 minutes

#------------Get the SDG series metadata---------------
# This seems really clunky but was the best way I could come up with

# Get all the metadata from one of the SDGs in PDH, doesn't matter which SDG:
metadata <- readSDMX(
  "https://stats-sdmx-disseminate.pacificdata.org/rest/dataflow/SPC/DF_SDG_02/4.4?references=all"
)

# Get the codelists slot from the codelists slot from this very complex XML object...
cl_series <- as.data.frame(
  slot(metadata, "codelists"),
  codelistId = "CL_SERIES"
) |>
  rename(label = label.en.label, series_id = id) |>
  select(series_id, label)

series_lookup <- cl_series |>
  # get the SDG indicator number eg 1.2.1 from out of the square brackets into its own column
  mutate(
    indicator_codes = stringr::str_remove_all(
      # Since some [] contain information other than the SDG indicator code - I have added the goal number in the code
      stringr::str_extract(label, "\\[(1[0-7]|[1-9])[^]]*\\]"),
      "\\[|\\]"
    )
  ) |>
  # some SERIES have multiple indicators e.g. 4.7.1, 12.8.1, 13.3.1
  mutate(indicator_codes = stringr::str_remove_all(indicator_codes, " ")) |>
  separate(
    indicator_codes,
    sep = ",",
    into = c("indicator_code_a", "indicator_code_b", "indicator_code_c"),
    fill = "right",
    remove = FALSE
  ) |>
  # indicate whether or not this is one of the PICT priority indicators. Note the
  # vector pict_sdg_priorities is defined in a script in the /R/ folder.
  mutate(
    pict_priority = indicator_code_a %in%
      pict_sdg_priorities |
      indicator_code_b %in% pict_sdg_priorities |
      indicator_code_c %in% pict_sdg_priorities
  )


# a many-to-many lookup table, use with caution:
series_lookup_l <- series_lookup |>
  select(series_id, label, indicator_code_a:indicator_code_c) |>
  gather(sequence, indicator_code, -series_id, -label) |>
  select(-sequence) |>
  mutate(sdg = str_extract(indicator_code, "^[0-9]*")) |>
  mutate(pict_priority = indicator_code %in% pict_sdg_priorities) |>
  mutate(
    pacstat_priority = pict_priority &
      (sdg %in% c(1:10, 16) | indicator_code == "17.19.2")
  )

# there will be lots of interest in this so save it as an output in its own
# right
write_csv(series_lookup_l, "output/sdg_series_pacstat_lookup.csv")


# Which ones in our list of PICT priorities are missing from this codelist?
# number rows below should be zero:
stopifnot(
  pict_sdg_priorities[
    !pict_sdg_priorities %in%
      unique(series_lookup_l$indicator_code)
  ] |>
    nrow() ==
    0
)


#---------------------------Download data--------
#
# Only a certain list of the SDGs are in scope for PacStat, defined in the
# Project Paper:
df_of_interest <- paste0("DF_SDG_", sprintf("%02d", c(1:10, 16, 17)))

# Vector of the SERIES codes of just those of interest to PacStat
# because they are in SDGs 1-10, 16 or a particular indicator in 17.19.2
series_of_interest <- series_lookup_l |>
  filter(pacstat_priority) |>
  pull(series_id) |>
  unique()

# an empty list in which we are going to store the downloaded data from PDH.Stat
sdgs_list <- list()

# Download each dataflow one at a time and store the indicators and country
# values that are relevant for us
for (i in 1:length(df_of_interest)) {
  sdgs_list[[i]] <- readSDMX(
    providerId = "PDH",
    resource = "data",
    flowRef = df_of_interest[i]
  ) |>
    as_tibble() |>
    clean_names() |>
    # only our series of interest and countries of interest. Note that ida_picts
    # is defined in a script in the /R/ folder and is the ISO2 codes for PICTs
    # that are members of IDA, the World bank soft loan arm:
    filter(series %in% series_of_interest, ref_area %in% ida_picts)
}

# Combine into a single tibble:
sdgs <- bind_rows(sdgs_list) |>
  # convert ISO2 codes for PICTs into their names for use later in graphics
  # etc:
  mutate(
    country = countrycode(
      ref_area,
      origin = "iso2c",
      destination = "country.name.en"
    )
  ) |>
  # We could filter by the reporting type G ("global" i.e. from the UN reporting
  # database) as the refactoring to add national indicators is not done yet
  # (2026) and muddies the water. But the intent is to measure growing numbers
  # of data, so 'national' estimates still count, if there isn't a global
  # estimate available. So there's no need to filter at all here.
  #
  # we only use certain composite breakdowns - so exclude income, education,
  # occupation, custom, activity, product. Note that reporting_type is
  # deliberately excluded in this next statement - so if there is both an N and
  # a G observation for a series in one year, only one counts.
  distinct(
    series,
    ref_area,
    country,
    sex,
    age,
    urbanisation,
    disability_status,
    obs_time
  )

# we expect no NAs, so let's just check:
stopifnot(
  nrow(filter(bind_rows(sdgs_list), is.na(obs_value))) == 0
)
#================Analysis and presentation==============
# Various ways we might use this; for now these are more illustrative.

# Key number - how many observations?
nrow(sdgs)

p1 <- sdgs |>
  group_by(obs_time) |>
  summarise(obs = n()) |>
  ggplot(aes(x = obs_time, y = obs)) +
  geom_col(fill = spc_cols(2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Reference period",
    y = "Number of observations",
    title = "PacStat2-relevant SDG observations for IDA countries in PDH.Stat",
    subtitle = "SDGs 1-10, 16 and 17.19.2, where country-values are possible"
  )

p2 <- sdgs |>
  filter(obs_time >= 2000) |>
  group_by(obs_time, country) |>
  summarise(obs = n()) |>
  ggplot(aes(x = obs_time, y = obs)) +
  facet_wrap(~country, ncol = 5) +
  geom_col(fill = spc_cols(2)) +
  scale_x_discrete(breaks = c(2002, 2014, 2026)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Reference period",
    y = "Number of observations",
    title = "PacStat2-relevant SDG observations for IDA countries in PDH.Stat",
    subtitle = "SDGs 1-10, 16 and 17.19.2, where country-values are possible"
  )

sdgs_cumul <- sdgs |>
  mutate(obs_time = as.numeric(obs_time)) |>
  group_by(obs_time) |>
  summarise(obs = n()) |>
  arrange(obs_time) |>
  mutate(cumul_obs = cumsum(obs)) |>
  mutate(
    increase = cumul_obs - lag(cumul_obs),
    growth = increase / lag(cumul_obs)
  )

tail(sdgs_cumul, 10)

p3 <- sdgs_cumul |>
  ggplot(aes(x = obs_time, y = cumul_obs)) +
  geom_line(colour = spc_cols(2)) +
  scale_y_continuous(label = comma) +
  labs(
    x = "Reference period",
    y = "Cumulative number of observations",
    title = "PacStat2-relevant SDG observations for IDA countries in PDH.Stat",
    subtitle = "SDGs 1-10, 16 and 17.19.2, where country-values are possible"
  )

p4 <- sdgs_cumul |>
  # massive growth in 2000 so only show after that point
  filter(obs_time > 2000) |>
  ggplot(aes(x = obs_time, y = growth)) +
  geom_line(colour = spc_cols(2)) +
  scale_y_continuous(label = percent) +
  labs(
    x = "Reference period",
    y = "Growth in cumulative number of observations",
    title = "PacStat2-relevant SDG observations for IDA countries in PDH.Stat",
    subtitle = "SDGs 1-10, 16 and 17.19.2, where country-values are possible"
  )


svglite("output/sdg-count-total.svg", width = 10, height = 5)
print(p1)
dev.off()

svglite("output/sdg-count-by-country.svg", width = 10, height = 5)
print(p2)
dev.off()

svglite("output/sdg-count-total-cumulative.svg", width = 10, height = 5)
print(p3)
dev.off()

svglite("output/sdg-count-cumulative-growth.svg", width = 10, height = 5)
print(p4)
dev.off()

#----------------choosing a target for 2032-----------
tail(sdgs_cumul, 10)
# 2026 value of cumul_obs is 31904

# but what is the baseline? Obviously 2024, 2025 and 2026 are incomplete and
# still increasing Say they increase to mean(c(1651, 2058, 1732, 1960)) = 1850,
# the average for 2020-2023
sdgs_cumul |>
  select(obs_time, obs, current_cumul_obs = cumul_obs) |>
  filter(obs_time <= 2023) |>
  bind_rows(tibble(
    obs_time = 2024:2032,
    obs = c(rep(1850, 6), 1000, 500, 100)
  )) |>
  mutate(
    new_cumul_obs = cumsum(obs),
    growth_since_2026 = new_cumul_obs / nrow(sdgs) - 1
  ) |>
  tail(10)

# this suggests that the way we are currently going, if we can get the average
# number of observations that was achieved for 2020:2023 for 2024 to 2029 then
# 1000, 500, 100 for 2030, 2031 and 2032 we get a total of 33% increase from the
# current number.

# This suggests a 40% increase rather than 33% would be pretty ambitious. That
# would mean an average increase of about 2250 per year rather than 1850 per
# year. In the past, only 2019 has this many observations

# My gut feel is 35% would be pretty good as there's absolutely no guarantee we
# will keep up with the 2020:2023 average.

# maximum obs is 2507 in 2019 - seven year lag for today. Say by 2032 we wanted
# that many observations for everything up to 2029 (three year lag) and 1000 for
# 2030, 500 for 2031. this seems to ambitious but gives a good maximum ambition

# ******
#Comment AS: the peak in 2019 is related to the SDG Refactoring project - it
# reflects the upload of national data points from Batch 1 (goal 7,8,12,13) +
# goal 5 if we exclude national data points, the total is 2,063. A large portion
# of the national data points is disaggregated in the goal 8 (unemployment rate
# ou neet based on HIES) In the non-sdgs-coverage, the calculation is counting
# only total numbers,  and don't give extra points for extra granularity, which
# is not the case for the SDGs
