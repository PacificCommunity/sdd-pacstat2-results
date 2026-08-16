# Script to download and count coverage of key SDG indicators for the Pacific

# The indicator is "Number of observations against core statistics and their
# composite breakdowns, for IDA countries, that are available in a regional
# dissemination platform"

# The relevant core statistics are "SDGs 1 to 10, 16 & 17.19.2 and which are
# reported by country"

options(timeout = 300) # 5 minutes

#------------Get the SDG series metadata---------------
# This seems really clunky but was the best way I could come up with

# Get all the metadata from one of hte SDGs in PDH, doesn't matter which SDG:
metadata <- readSDMX(
  "https://stats-sdmx-disseminate.pacificdata.org/rest/dataflow/SPC/DF_SDG_02/4.4?references=all"
)

# Get the codelists slot from the codelists slot from this very complex XML object...
codelists <- metadata@codelists@codelists

# find the codelist for CL_SERIES in particular (turns out to be '10'):
this_codelist <- codelists[[match(
  "CL_SERIES",
  sapply(codelists, function(x) {
    x@id
  })
)]]

# The @Code slot in this_codelist is a list with 800+ elements, one for each
# SERIES code, we can extract the name and label from these that we need:
# the @Code slot in each
series_lookup <- lapply(this_codelist@Code, function(this_item) {
  tibble(series_id = this_item@id, label = this_item@label$en)
}) |>
  # collapse all of these into a single tibble with 850 or so rows:
  bind_rows() |>
  # get the SDG indicator number eg 1.2.1 from out of the square brackets into its own
  mutate(
    indicator_codes = stringr::str_remove_all(
      stringr::str_extract(label, "\\[[^]]+\\]"),
      "\\[|\\]"
    )
  ) |>
  # some SERIES have multiple indicators e.g. 4.7.1, 12.8.1, 13.3.1
  mutate(indicator_codes = str_remove_all(indicator_codes, " ")) |>
  separate(
    indicator_codes,
    sep = ",",
    into = c("indicator_code_a", "indicator_code_b", "indicator_code_c"),
    fill = "right",
    remove = FALSE
  ) |>
  #indicate whether or not this is one of the PICT priority indicators. Note the
  #vector pict_sdg_priorities is defined in a script in the /R/ folder.
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
# number rows below should be zero
stopifnot(
  pict_sdg_priorities[
    !pict_sdg_priorities %in%
      unique(series_lookup_l$indicator_code)
  ] |>
    nrow() ==
    0
)

#---------------Define PICTs of interest-----------

# 10 PICTs that are members of IDA
ida_picts <- c(
  "FM", # Federated States of Micronesia
  "FJ", # Fiji
  "KI", # Kiribati
  "MH", # Marshall Islands
  "PG", # Papua New Guinea
  "WS", # Samoa
  "SB", # Solomon Islands
  "TO", # Tonga
  "TV", # Tuvalu
  "VU" # Vanuatu
)
stopifnot(length(ida_picts) == 10)

#---------------------------Download data--------
df_of_interest <- paste0("DF_SDG_", sprintf("%02d", c(1:10, 16, 17)))

# Vector of the SERIES codes of just those of interest to PacStat
# because they are in SDGs 1-10, 16 or a particular indicator in 17.19.2
series_of_interest <- series_lookup_l |>
  filter(pacstat_priority) |>
  pull(series_id) |>
  unique()

sdgs_list <- list()

for (i in 1:length(df_of_interest)) {
  sdgs_list[[i]] <- readSDMX(
    providerId = "PDH",
    resource = "data",
    flowRef = df_of_interest[i]
  ) |>
    as_tibble() |>
    clean_names() |>
    # only our series of interest and countries of interest:
    filter(series %in% series_of_interest, ref_area %in% ida_picts)
}

sdgs <- bind_rows(sdgs_list)

#================Analysis and presentation==============
