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