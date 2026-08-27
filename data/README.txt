
This folder holds processed data suitable for reuse.

Contents:

## pacstat2-pdo.csv 
The definitive record which should grow over time of the number of observations against indicators in scope for PDO (Project Development Objective) 1 in the project PacStat 2.

This file is committed to Git and appended to over time.

## pacstat_pdo1_snapshot.rda
A snapshot of the latest full set of indicators - not observation values, but all the combinations of composite breakdown, time, country and indicator that are available.

This file is NOT committed to Git, and is replaced each time the script regular-use/pacstat2/pacstats2-pdo-monitor.R is run

## sdg_series_codelist.rda
A convenient map of SDG series to their full labels. Not committed to Git.
