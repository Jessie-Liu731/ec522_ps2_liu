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


######Visualize the Data with ggplot2
##Produce a Line Plot
line_unemp_date <- unemp |>
  ggplot(aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Unemployment Rate Over Time by Region",
    x = "Date",
    y = "Unemployment Rate (%)",
  ) +
  theme_minimal()
line_unemp_date

##Produce Individual Line Plots
individual_line_unemp_date <- unemp |>
  ggplot(aes(x = date, y = unemployment_rate, color = region)) + 
  geom_line() + 
  facet_wrap(~region) +
  labs(
    title = "Unemployment Rate Over Time for Each Region",
    x = "Date",
    y = "Unemployment Rate (%)",
  ) +
  theme_minimal()
individual_line_unemp_date

##make a bar chart
bar_unemp_region <- unemp_summary |>
  ggplot(aes(x = fct_reorder(region, mean_rate), y = mean_rate,fill = region)) +
  geom_col() +
  labs(
    title = "Average Unemployment Rate by Region",
    x = "Region",
    y = "Average Unemployment Rate (%)",
  ) +
  theme_minimal() 
bar_unemp_region 


##my choice
individual_line_unemp_change_date <- unemp |>
  ggplot(aes(x = date, y = unemployment_rate_change, color = region)) + 
  geom_line(na.rm = TRUE) + 
  facet_wrap(~region) +
  labs(
    title = "Unemployment Rate Change Over Time for Each Region",
    x = "Date",
    y = "Unemployment Rate Change (%)",
  ) +
  theme_minimal()
individual_line_unemp_change_date

###This is a panel showing the month-over-month change in unemployment rate for each region over time. 
###The plot is organized in facets, with each facet representing a different region, allowing for easy comparison of trends and fluctuations in unemployment rate changes across the United States, Oregon, California, Texas, and New York. 
###We could compare the degree of fluctuation across regions in different times. For example, in 2008, Texas experienced the most significant fluctuations (higher than average in United States) when New York experienced less, while, in 2020, New York experienced the most significant fluctuations, higher than the average in United States when Texas experienced less fluctuation this time.
###The line plots highlight periods of significant increases or decreases in unemployment rates, providing insights into regional economic conditions and labor market dynamics.




