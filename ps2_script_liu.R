######load library
library(tidyquant)
library(tidyverse)

######read file
# unemp_raw <- tq_get(
#  c("UNRATE","ORUR","CAUR","TXUR","NYUR"),
#    get = "economic.data",
#    from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw,"data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

######clean up the data
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )

######wrangle the data
##replace Oct 2025 with September 2025 value
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

##add a column called unemployment rate change to unemp equal to the month-over-month change in unemployment rate within each region
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

##create unemp_recent to include observations from January 2015 onward
unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01")) 

##compute, for each region, the mean, minimum and maximum unemployment rate over the whole sample
unemp_summary <- unemp |>
  group_by(region) |>
  summarise(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  ) 

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarise(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  ) 


######find the extremes
##find the five (plus ties) region-month combinations with the largest month-over-month increase anywhere in the data
unemp_top_increases <- unemp |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)

##find the five (plus ties) largest month-over-month decreases
unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)


######find the extremes excluding 2020
##create a dataset excluding 2020
unemp_no_covid <- unemp |>
  filter(year(date) != 2020)

##find the five (plus ties) region-month combinations with the largest month-over-month increase anywhere in the data
unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)

##find the five (plus ties) largest month-over-month decreases
unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)






