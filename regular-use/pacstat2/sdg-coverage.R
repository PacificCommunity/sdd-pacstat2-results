# Script to download and count coverage of key SDG indicators for the Pacific

# The indicator is "Number of observations against core statistics and their
# composite breakdowns, for IDA countries, that are available in a regional
# dissemination platform"

# The relevant core statistics are "SDGs 1 to 10, 16 & 17.19.2 and which are
# reported by country"
source("setup.R")

options(timeout = 300) # 5 minutes

#-------------Out of scope SDG indicators-----------
#
# Some of the SDGs captured are not really statistics, e.g. they are whether or
# not a country has a legal framework for X; or they are not really the
# country's responsibility e.g. aid flows

out_of_scope <- read_csv(
  "raw-data/out-of-scope-sdg-series.csv",
  col_types = "clc"
) |>
  filter(not_national_statistic) |>
  pull(series)

# Should be about 13 of these, but we may add or subtract some over time so we
# have a slightly vague test
stopifnot(length(out_of_scope) > 0 & length(out_of_scope) < 30)

#------------Get the SDG series metadata---------------
# This seems really clunky but was the best way I could come up with

# Get all the metadata from one of the SDGs in PDH, doesn't matter which SDG:
metadata <- readSDMX(
  "https://stats-sdmx-disseminate.pacificdata.org/rest/dataflow/SPC/DF_SDG_02/latest?references=all"
)

# Get the codelists slot from the codelists slot from this very complex XML object...
cl_series <- as.data.frame(
  slot(metadata, "codelists"),
  codelistId = "CL_SERIES"
) |>
  rename(label = label.en.label, series_id = id) |>
  select(series_id, label)

# we want this later so are going to save it
sdg_series_codelist <- cl_series
save(sdg_series_codelist, file = "data/sdg_series_codelist.rda")

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
  )


# a many-to-many lookup table, use with caution:
series_lookup_l <- series_lookup |>
  select(series_id, label, indicator_code_a:indicator_code_c) |>
  gather(sequence, indicator_code, -series_id, -label) |>
  select(-sequence) |>
  mutate(sdg = str_extract(indicator_code, "^[0-9]*")) |>
  mutate(pict_priority = indicator_code %in% pict_sdg_priorities) |>
  # indicate whether or not this is one of the PICT priority indicators. Note the
  # vector pict_sdg_priorities is defined in a script in the /R/ folder.
  mutate(
    pacstat_priority = pict_priority &
      (sdg %in% c(1:10, 16) | indicator_code == "17.19.2")
  ) |>
  filter(!is.na(indicator_code))

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
    length() ==
    # Ideally this should equal zero. But there is one PICT priority SDG
    # indicator, 17.6.2, that for some reason does not get matched to a
    # dataflow. This needs to be fixed. The indicator is about broadband, and
    # there has been renumbering.
    1
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
  #
  # The following 'distinct' operation reduces the number of SDGs substantially,
  # about 30% as at August 2026. We think this is justified to reduce counting
  # just extra granularity of additional composite breakdowns that are not core
  # to what we are measuring, and in some cases to remove when we have two
  # observations of a value from two methods (global and national)
  #
  distinct(
    series,
    ref_area,
    country,
    sex,
    age,
    urbanisation,
    disability_status,
    obs_time
  ) |>
  # filter out the out of scope SDGs
  filter(!series %in% out_of_scope)

# we expect no NAs, so let's just check:
stopifnot(
  nrow(filter(bind_rows(sdgs_list), is.na(obs_value))) == 0
)
