source("setup.R")

load("data/pacstat_pdo1_snapshot.rda")
load("data/sdg_series_codelist.rda")

p <- pacstat_pdo1_snapshot |>
  count(year) |>
  filter(year >= 1990) |>
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = spc_cols(2)) +
  scale_y_continuous(label = comma) +
  scale_x_continuous(breaks = 1990:max(pacstat_pdo1_snapshot$year)) +
  labs(
    x = "Year observation refers to",
    y = "Number of observations",
    title = "Number of values for core statistics available in PDH.Stat for IDA countries",
    subtitle = "Includes relevant SDGs, population, labour force, inflation, tourism, migration and disability"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor.x = element_blank()
  )

svg("output/core-statistics-count-by-year.svg", width = 11, height = 4)
print(p)
dev.off()
