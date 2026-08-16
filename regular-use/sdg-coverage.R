# Script to download and count coverage of key SDG indicators for the Pacific

# The indicator is "Number of observations against core statistics and their
# composite breakdowns, for IDA countries, that are available in a regional
# dissemination platform"

options(timeout = 300) # 5 minutes

#------------Get the SDG series metadata---------------
# This seems really clunky but was the best I could come up with

# Get all the metadata from one of hte SDGs in PDH, doesn't matter which SDG:
metadata <- readSDMX(
  "https://stats-sdmx-disseminate.pacificdata.org/rest/dataflow/SPC/DF_SDG_02/4.4?references=all"
)

# Get the codelists slot from the codelists slot from this very complex XML object...
codelists <- metadata@codelists@codelists

# find the codelist for CL_SERIES in particular:
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
  tibble(id = this_item@id, label = this_item@label$en)
}) |>
  bind_rows() |>
  # get the SDG indicator number eg 1.2.1 from out of the square brackets into its own
  mutate(
    indicator_code = stringr::str_extract(
      label,
      "(?<=\\[)\\d+(?:\\.\\d+)+(?=\\])"
    )
  ) |>
  #indicate whether or not this is one of the PICT priority indicators
  mutate(pict_priority = indicator_code %in% pict_sdg_priorities)


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

download_sdg <- function(url) {
  this_sdg <- readSDMX(url) |>
    as_tibble() |>
    clean_names() |>
    filter(ref_area %in% ida_picts)

  missing_picts <- ida_picts[!ida_picts %in% unique(this_sdg$ref_area)]
  if (length(missing_picts) > 0) {
    warning(glue("{paste(missing_picts, collapse = ' ')} missing from data"))
  }

  # there aren't meant to be any NAs in the observations
  stopifnot(sum(is.na(this_sdg$obs_value)) == 0)
  return(this_sdg)
}


# note - this downloads some unnecessary countries and then kicks them out straight away
sdg1 <- download_sdg(
  "https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_SDG_01,4.4/A.G+N.SI_POV_EMP1+SI_COV_SOCAST+SI_COV_SOCINS+SI_POV_DAY1+SI_POV_NAHC+SD_MDP_ANDI+SD_MDP_MUHC+SI_COV_BENFTS+SI_COV_CHLD+SI_COV_DISAB+SI_COV_PENSN+SI_COV_POOR+SI_COV_UEMP+SI_COV_VULN+SI_COV_WKINJRY+SP_ACS_BSRVSAN+SP_ACS_BSRVH2O.MEL+MIC+POL+AS+CK+FJ+PF+GU+KI+MH+FM+NR+NC+NU+MP+PW+PG+WS+SB+TK+TO+TV+VU+WF._T+M+F._T+Y0T14+Y_GE15+Y_GE65._T+U+R._T.......?startPeriod=1993&dimensionAtObservation=AllDimensions"
)

sdg2 <- download_sdg(
  "https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_SDG_02,4.4/A.G+N.SN_ITK_DEFCN+SH_STA_STNTN+SH_STA_WASTN+SN_ITK_DEFC+SH_STA_STNT+SH_STA_WAST+SN_STA_OVWGT+SI_AGR_SSFP+SI_AGR_LSFP+AG_LND_SUST+AG_LND_FOVH+AG_LND_NFI+AG_LND_RMM+AG_LND_SDGRD+AG_LND_H2OAVAIL+AG_LND_FERTMG+AG_LND_AGRBIO+AG_LND_AGRWAG+AG_LND_FIES+AG_LND_LNDSTR+AG_LND_SUST_PRXTS+AG_LND_SUST_PRXCSS+ER_GRF_ANIMKPT+ER_GRF_ANIMRCNTN+ER_GRF_PLNTSTOR+ER_GRF_ANIMRCNTN_TRB+ER_GRF_ANIMKPT_TRB+AG_PRD_ORTIND+AG_XPD_AGSGB+AG_PRD_AGVAS.CK+FJ+PF+GU+KI+MH+MEL+MIC+FM+NR+NC+NU+MP+PW+PG+POL+WS+SB+TO+TV+VU._T+M+F._T+Y0T4._T._T......._T?startPeriod=1990&dimensionAtObservation=AllDimensions"
)

nrow(sdg2)

readSDMX()

sdg2a <- readSDMX(
  providerId = "PDH",
  resource = "data",
  flowRef = "DF_SDG_02"
) |>
  as_tibble() |>
  clean_names()
