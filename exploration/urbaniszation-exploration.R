# This was a one-off just to identify what dataflows have z or s in the column
# for urban / rural split

source("setup.R")

load("data/pacstat_pdo1_snapshot.rda")

pacstat_pdo1_snapshot |>
  mutate(
    has_urbanis = !is.na(urbanisation),
    has_urbaniz = !is.na(urbanization)
  ) |>
  filter(has_urbanis | has_urbaniz) |>
  distinct(series, dataset, has_urbanis, has_urbaniz) |>
  View()
