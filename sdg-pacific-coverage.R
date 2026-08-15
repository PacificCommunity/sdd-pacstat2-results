# Draws several charts showing progress in getting data against the SDGs. Purpose
# is to illustrate that there is progress but also that there are many SDGs
# without much data yet for the Pacific.
#
# There is a parameter in the code that lets you choose to either see the actual
# country names or to replace them with letters so it is less of a league table.
#
# Peter Ellis Feb - May 2023

source("setup.R")

#-----------------------Get the SDG data and metadata---------------

dataflow <- "DF_SDG"

# metadata ie code lists
d_code_list <- get_pdh_codelists(dataflow = dataflow, version = "3.0")

# data
if (!exists("d_raw")) {
  d_raw <- readSDMX(
    providerId = "PDH",
    resource = "data",
    flowRef = dataflow
  ) |>
    as_tibble()
}

d0 <- d_raw |>
  left_join(
    filter(d_code_list, category_en == "Codelist for SDG indicators"),
    by = c("INDICATOR" = "id")
  ) |>
  select(-category_en, -category_fr) |>
  rename(indicator_full = name) |>
  clean_names() |>
  mutate(
    indicator_number = str_squish(str_extract(indicator_full, "[0-9a-z\\.]+ "))
  )

# some indicators need to appear against two indicator numbers, but above
# procedure ony picks them up once so we add them specifically
extra_rows <- d0 |>
  filter(grepl("1.5.1 and 11.5.1", indicator_full)) |>
  mutate(indicator_number = "11.5.1")

d <- rbind(d0, extra_rows)

length(unique(d$indicator_number))

ind_sum <- d |>
  group_by(indicator_number, geo_pict) |>
  summarise(n_obs = length(unique(obs_time))) |>
  ungroup() |>
  complete(indicator_number, geo_pict, fill = list(n_obs = 0)) |>
  left_join(
    select(
      filter(
        d_code_list,
        category_en == "Common hierarchical codelist for PICTs"
      ),
      geo_pict = id,
      pict = name
    ),
    by = "geo_pict"
  ) |>
  mutate(sdg_number = as.numeric(str_extract(indicator_number, "^[0-9]*"))) |>
  left_join(sdg_names, by = "sdg_number")

# This next chunk of code replaces the country names with random letters A-Z.
# set anonymise_countries to FALSE if you wan tthe actual names
anonymise_countries <- FALSE
if (anonymise_countries) {
  ind_sum <- anon_col(ind_sum, "pict")
}

ind_sum <- ind_sum |>
  group_by(geo_pict) |>
  mutate(
    n_inds = length(unique(indicator_number[n_obs > 0])),
    pict_lab = glue("{pict} ({n_inds})")
  ) |>
  ungroup() |>
  mutate(
    pict = fct_reorder(pict, -n_obs, .fun = mean),
    pict_lab = fct_reorder(pict_lab, -n_obs, .fun = mean),
    indicator_number = fct_reorder(indicator_number, -n_obs, .fun = mean)
  ) |>
  mutate(
    n_obs_na = ifelse(n_obs == 0, NA, n_obs),
    n_obs_c = case_when(
      n_obs == 0 ~ "None",
      n_obs == 1 ~ "One",
      n_obs >= 2 ~ "Two or more"
    )
  )

# how many have some data?
sum_tab <- ind_sum |>
  count(n_obs_c) |>
  mutate(prop = percent(n / sum(n), accuracy = 1))

p1 <- ind_sum |>
  ggplot(aes(x = indicator_number, y = pict_lab, fill = n_obs_na)) +
  geom_tile() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
    legend.position = "bottom"
  ) +
  scale_fill_viridis_c() +
  labs(x = "", y = "", fill = "Number of\ntime points")


cat_cols <- c("grey90", spc_cols(c(1, 2)))

p2 <- ind_sum |>
  ggplot(aes(x = indicator_number, y = pict_lab, fill = n_obs_c)) +
  geom_tile() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
    legend.position = "bottom"
  ) +
  labs(x = "", y = "", fill = "Number of\ntime points") +
  scale_fill_manual(values = cat_cols)

p3 <- ind_sum |>
  ggplot(aes(x = indicator_number, y = pict, fill = n_obs_c)) +
  facet_wrap(~sdgw, scales = "free_x", ncol = 6) +
  geom_tile(colour = "white") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = c(0.92, 0.12)
  ) +
  labs(
    x = "",
    y = "",
    fill = "Number of\ntime points",
    title = glue(
      "Around {sum_tab[1, 'prop']} of SDG - country combinations have no data."
    ),
    subtitle = glue(
      "A further {sum_tab[2, 'prop']} have only a single observation"
    )
  ) +
  scale_fill_manual(values = cat_cols)

png("output/indicator-summary.png", 7000, 3000, res = 600, type = "cairo-png")
print(p1)
dev.off()

png(
  "output/indicator-summary_cat.png",
  7000,
  3000,
  res = 600,
  type = "cairo-png"
)
print(p2)
dev.off()

png(
  "output/indicator-summary_faceted.png",
  8000,
  5000,
  res = 600,
  type = "cairo-png"
)
print(p3)
dev.off()


#------------top 10 and bottom 10 indicators-----------
best_and_worst <- ind_sum |>
  group_by(indicator_number, sdg_number) |>
  summarise(
    obs_mn = mean(n_obs),
    countries1 = length(unique(pict[n_obs > 0])),
    countries2 = length(unique(pict[n_obs > 1]))
  ) |>
  ungroup() |>
  left_join(
    distinct(d, indicator_number, indicator_full),
    by = "indicator_number",
    multiple = "all"
  ) |>
  group_by(indicator_number, sdg_number, obs_mn, countries1, countries2) |>
  summarise(indicators = paste(unique(indicator_full), collapse = "; ")) |>
  ungroup()

best_and_worst |>
  arrange(countries2, countries1) |>
  slice(1:20) |>
  mutate(indicators = str_trunc(indicators, 80)) |>
  distinct(indicators, countries1)


best_and_worst |>
  arrange(-countries2, -countries1) |>
  slice(1:20) |>
  mutate(indicators = str_trunc(indicators, 80)) |>
  distinct(indicators, countries2)

#-----------------alternative presentation by Thematic Area 50-------------------
tas <- read_excel(
  "raw-data/Draft SDG indicator set for 2050 Themes.xlsx",
  sheet = "Sheet1"
)

count(tas, `SDG Label`, sort = TRUE)

p4 <- d |>
  group_by(indicator, geo_pict) |>
  summarise(n_obs = length(unique(obs_time))) |>
  ungroup() |>
  complete(indicator, geo_pict, fill = list(n_obs = 0)) |>
  left_join(
    select(
      filter(
        d_code_list,
        category_en == "Common hierarchical codelist for PICTs"
      ),
      geo_pict = id,
      pict = name
    ),
    by = "geo_pict"
  ) |>
  group_by(geo_pict) |>
  mutate(
    n_inds = length(unique(indicator[n_obs > 0])),
    pict_lab = glue("{pict} ({n_inds})")
  ) |>
  ungroup() |>
  mutate(
    pict = fct_reorder(pict, -n_obs, .fun = mean),
    pict_lab = fct_reorder(pict_lab, -n_obs, .fun = mean),
    indicator = fct_reorder(indicator, -n_obs, .fun = mean)
  ) |>
  mutate(
    n_obs_na = ifelse(n_obs == 0, NA, n_obs),
    n_obs_c = case_when(
      n_obs == 0 ~ "None",
      n_obs == 1 ~ "One",
      n_obs >= 2 ~ "Two or more"
    )
  ) |>
  left_join(
    distinct(tas, indicator = `PDH Code`, ta = `Thematic Area 2050`),
    by = "indicator"
  ) |>
  filter(!is.na(ta)) |>
  mutate(indicator = fct_reorder(indicator, -n_obs, .fun = mean)) |>
  ggplot(aes(x = indicator, y = pict, fill = n_obs_c)) +
  facet_wrap(~ta, scales = "free_x", ncol = 2) +
  geom_tile(colour = "white") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = c(0.92, 0.12)
  ) +
  labs(x = "", y = "", fill = "Number of\ntime points") +
  scale_fill_manual(values = cat_cols)

png(
  "output/indicator-themes_faceted.png",
  8000,
  7000,
  res = 600,
  type = "cairo-png"
)
print(p4)
dev.off()


p5 <- ind_sum |>
  left_join(
    distinct(tas, indicator_number = `SDG Label`, ta = `Thematic Area 2050`),
    by = "indicator_number"
  ) |>
  filter(!is.na(ta)) |>
  mutate(
    indicator_number = fct_reorder(indicator_number, -n_obs, .fun = mean)
  ) |>
  ggplot(aes(x = indicator_number, y = pict, fill = n_obs_c)) +
  facet_wrap(~ta, scales = "free_x", ncol = 2) +
  geom_tile(colour = "white") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = c(0.92, 0.12)
  ) +
  labs(x = "", y = "", fill = "Number of\ntime points") +
  scale_fill_manual(values = cat_cols)

png(
  "output/indicator-sdg-themes_faceted.png",
  8000,
  7000,
  res = 600,
  type = "cairo-png"
)
print(p5)
dev.off()


#----------------------how is it changing over time----------------

p6 <- count(d0, obs_time) |>
  arrange(obs_time) |>
  mutate(cumul_count = cumsum(n), obs_time = as.numeric(obs_time)) |>
  filter(obs_time >= 2010 & obs_time <= 2023) |>
  ggplot(aes(x = obs_time, y = cumul_count)) +
  geom_line(colour = spc_cols(1)) +
  scale_y_continuous(label = comma) +
  expand_limits(y = 0) +
  labs(
    x = "Year that estimate applies to",
    y = "Cumulative count of observations",
    title = "Increasing availability of SDG data over time",
    caption = "Source: PDH.Stat"
  )

png(
  "output/cumulative indicators.png",
  4000,
  2000,
  res = 600,
  type = "cairo-png"
)
print(p6)


# variant width version
# a couple of new packages we need for positioning plots
library(ggforce) # for facet_wrap
library(patchwork) # for using + to add plots togeterh and plot_layout

# We are going to define two plots, one for each row, and then add them together
# First, make the data that will be common to both plots:
d6 <- ind_sum |>
  left_join(
    distinct(tas, indicator_number = `SDG Label`, ta = `Thematic Area 2050`),
    by = "indicator_number"
  ) |>
  filter(!is.na(ta)) |>
  # version of Thematic Area with line breaks in it
  mutate(
    ta = case_when(
      grepl("Resources", ta) | grepl("People", ta) | grepl("Ocean", ta) ~ ta,
      grepl("Technology", ta) ~ str_wrap(ta, 10),
      TRUE ~ str_wrap(ta, 15)
    )
  ) |>
  mutate(indicator_number = fct_reorder(indicator_number, -n_obs, .fun = mean))

# make a subset of the themes to use in the first row
all_themes <- unique(d6$ta)
# which themes to include in the top row took a bit of trial and error, you could
# come up with a cleverer way to do it so there are the same number of indicators in each theme
t1 <- all_themes[c(1, 2, 4)]
sdg50 <- function(d, legend.position = "bottom") {
  p <- d |>
    ggplot(aes(x = indicator_number, y = pict, fill = n_obs_c)) +
    facet_row(~ta, scales = "free_x", space = "free") +
    geom_tile(colour = "white") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      legend.position = legend.position,
      strip.text = element_text(size = 10)
    ) +
    labs(x = "", y = "", fill = "Number of\ntime points") +
    scale_fill_manual(values = cat_cols)
  return(p)
}


# first row:
p6a <- d6 |>
  filter(ta %in% t1) |>
  sdg50()

# second row:
p6b <- d6 |>
  filter(!ta %in% t1) |>
  sdg50(legend.position = "none")


# combine into a single figure:
png(
  "output/indicator-sdg-themes_faceted-spaced.png",
  8500,
  6000,
  res = 600,
  type = "cairo-png"
)
print(p6a + p6b + plot_layout(ncol = 1))
dev.off()
