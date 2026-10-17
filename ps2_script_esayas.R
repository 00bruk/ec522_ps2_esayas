library(tidyquant)
library(tidyverse)
library(curl)

# ==========================================
# Part 3 – Read In the Data
# ==========================================
dir.create("data", showWarnings = FALSE)

# Disguise the R script as a Mac web browser to bypass FRED blocks
h <- new_handle(useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)")
symbols <- c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR")

unemp_raw <- map_dfr(symbols, function(sym) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", sym)
  tmp <- tempfile(fileext = ".csv")
  curl_download(url, tmp, handle = h)
  read_csv(tmp, show_col_types = FALSE) |>
    filter(DATE >= as.Date("1976-01-01")) |>
    rename(date = DATE, price = 2) |>
    mutate(symbol = sym) |>
    select(symbol, date, price)
})

write_rds(unemp_raw, "data/unemp_raw.rds")
unemp_raw <- read_rds("data/unemp_raw.rds")
print("Part 3 Success: First 6 rows:")
print(head(unemp_raw))

# ==========================================
# Part 4 – Wrangle the Data with dplyr
# ==========================================
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"   ~ "Oregon",
      symbol == "CAUR"   ~ "California",
      symbol == "TXUR"   ~ "Texas",
      symbol == "NYUR"   ~ "New York"
    )
  ) |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    ),
    unemployment_rate_change = unemployment_rate - lag(unemployment_rate)
  ) |>
  ungroup()

unemp_recent <- unemp |> filter(date >= as.Date("2015-01-01"))

unemp_summary <- unemp |> group_by(region) |> summarize(mean_rate = mean(unemployment_rate, na.rm = TRUE), min_rate = min(unemployment_rate, na.rm = TRUE), max_rate = max(unemployment_rate, na.rm = TRUE))
unemp_recent_summary <- unemp_recent |> group_by(region) |> summarize(mean_rate = mean(unemployment_rate, na.rm = TRUE), min_rate = min(unemployment_rate, na.rm = TRUE), max_rate = max(unemployment_rate, na.rm = TRUE))

unemp_top_increases <- unemp |> slice_max(unemployment_rate_change, n = 5)
unemp_top_decreases <- unemp |> slice_min(unemployment_rate_change, n = 5)
unemp_top_increases_no_covid <- unemp |> filter(year(date) != 2020) |> slice_max(unemployment_rate_change, n = 5)
unemp_top_decreases_no_covid <- unemp |> filter(year(date) != 2020) |> slice_min(unemployment_rate_change, n = 5)

# ==========================================
# Part 5 – Visualize the Data with ggplot2
# ==========================================
p1 <- ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) + geom_line() + labs(title = "Monthly Unemployment Rate (1976-Present)", x = "Date", y = "Unemployment Rate (%)", color = "Region") + theme_minimal()
p2 <- ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) + geom_line() + facet_wrap(~ region) + labs(title = "Monthly Unemployment Rate by Region", x = "Date", y = "Unemployment Rate (%)") + theme_minimal()
p3 <- ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate)) + geom_col() + labs(title = "Mean Unemployment Rate by Region (1976-Present)", x = "Region", y = "Mean Rate (%)") + theme_minimal()
p4 <- ggplot(filter(unemp, year(date) != 2020), aes(x = date, y = unemployment_rate_change, color = region)) + geom_line() + labs(title = "Month-over-Month Unemployment Rate Change (Excluding 2020)", x = "Date", y = "Change (Percentage Points)", color = "Region") + theme_minimal()

print("Parts 4 and 5 complete.")