# Step 1a: Data Transformation & Cleaning
# Purpose:  Convert all datasets into a long format,
#           add country codes
#           Standardise column names, derive log-wage and a
#           readable gender variable, and subset to the
#           variables needed for analysis.
# Inputs:   All files in data/raw
# Outputs:  data/interim/


# 0 Load Packages ===========================================================
library(readxl)
library(tidyverse)
library(countrycode)
library(janitor)

# 1 Create functions =======================================================

combine_longest_series <- function(data, indicator, sources) {
  
  counts <- data |>
    filter(year >= 1970, year <= 2025) |>
    group_by(iso3c) |>
    summarise(
      across(all_of(sources), ~sum(!is.na(.))),
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(source = sources[which.max(c_across(all_of(sources)))]) |>
    ungroup() |>
    select(iso3c, source)
  
  data |>
    left_join(counts, by = "iso3c") |>
    rowwise() |>
    mutate(
      !!indicator := get(source)
    ) |>
    ungroup()
}

clean_data <- function(data, indicator_col = NULL, country_col = country) {
  
  data <- data |>
    clean_names() |>
    pivot_longer(
      cols = starts_with("x"),
      names_to = "year",
      values_to = "value"
    ) |>
    mutate(
      iso3c = countrycode({{ country_col }}, origin = "country.name", destination = "iso3c"),
      year = as.integer(str_extract(year, "\\d{4}"))
    ) |>
    filter(!is.na(iso3c))
  
  if (!is.null(indicator_col)) {
    data <- data |>
      select(iso3c, year, value, {{ indicator_col }}) |>
      pivot_wider(
        names_from = {{ indicator_col }},
        values_from = value
      ) |>
      clean_names()
  } else {
    data <- data |>
      select(iso3c, year, value)
  }
  
  data
}

# 2 Read and Clean raw datasets ================================================

## 2.1 Laeven & Valencia Banking Crisis Dataset ============================

crises <- read_xlsx(
  path = "data/raw/SYSTEMIC_BANKING_CRISES_DATABASE_2026.xlsx",
  sheet = 2
)

# Select important columns
crises <- crises |> 
  clean_names() |> 
  select(country, start, end)

crises <- crises |> 
  
  # Remove rows that contain NA in column "start"
  filter(!is.na(start)) |> 
  
  # Extract annotation symbols (e.g. "6/")
  mutate(
    annotation_country = str_extract(country, "\\d+/"),
    annotation_year = str_extract(end, "\\d+/"),
    
    # Remove annotations from country and end
    country = str_trim(str_remove(country, "\\s*\\d+/")),
    end = str_trim(str_remove(end, "\\s*\\d+/"))
  ) |> 
  
  # Replace "..." in end with 2025
  mutate(
    end = ifelse(end == "…", "2025", end)
  ) |> 
  
  # Convert end to numeric
  mutate(
    end = as.numeric(end)
  )

# Remove duplicates

crises_merged <- crises |> 
  arrange(country, start) |> 
  group_by(country) |> 
  
  # start a new crisis group whenever there is a gap
  mutate(
    new_group = if_else(
      start > lag(end, default = first(start) - 2) + 1,
      1L,
      0L
    ),
    crisis_group = cumsum(new_group)
  ) |> 
  
  group_by(country, crisis_group) |> 
  summarise(
    start = min(start),
    end   = max(end),
    .groups = "drop"
  )

# crisis years
crisis_years <- crises_merged |> 
  rowwise() |> 
  mutate(year = list(start:end)) |> 
  unnest(year) |> 
  select(country, year) |> 
  mutate(crisis = 1)

crisis_start <- crises_merged |> 
  mutate(
    country,
    year = start,
    crisis_start = 1,
    .keep = "none"
  )


## 2.2 BIS ===================================================================

# Loans
credit_bis <- read.csv("data/raw/WS_TC_csv_col.csv")

credit_bis_clean <- credit_bis |> 
  clean_names() |> 
  pivot_longer(
    cols = starts_with("x"),
    names_to = "time",
    values_to = "value" 
  ) |> 
  filter(
    unit_type_2 == "Domestic currency (incl. conv. to current ccy made using a fix parity)", 
    valuation_method == "Market value",
    adjustment == "Adjusted for breaks",
    borrowing_sector %in% c("Private non-financial sector", "Households & NPISHs", "Non-financial corporations"),
    # Year end value
    str_detect(time, "q4")
  ) |> 
  mutate(
    year = as.integer(str_extract(time, "\\d{4}")),
    # Add country codes
    iso3c = countrycode(borrowers_country, origin = "country.name", destination = "iso3c")
  ) |> 
  select(c(borrowing_sector, lending_sector, year, value, iso3c)) |> 
  pivot_wider( 
    names_from = lending_sector,
    values_from = value
  ) |> 
  pivot_wider(
    names_from = borrowing_sector,
    values_from = c(`All sectors`, `Banks, domestic`)
  ) |> 
  mutate(
    year,
    iso3c,
    tloanscorp = `All sectors_Non-financial corporations`,
    tloanspriv = `All sectors_Private non-financial sector`,
    tloansh = `All sectors_Households & NPISHs`,
    bloanspriv = `Banks, domestic_Private non-financial sector`,
    .keep = "none"
  )

# Credit-to-GDP ratio
cgdp_bis <- read_csv("data/raw/WS_CREDIT_GAP_csv_col.csv")

cgdp_bis_clean <- cgdp_bis |> 
  pivot_longer(
    cols = !(FREQ:Series),
    names_to = "Time",
    values_to = "cgdppriv"
  ) |> 
  # Year end value
  filter(
    str_detect(Time, "Q4"),
    `Credit gap data type` == "Credit-to-GDP ratios (actual data)"
  ) |> 
  mutate(
    year = as.integer(str_extract(Time, "\\d{4}")),
    iso3c = countrycode(`Borrowers' country`, origin = "country.name", destination = "iso3c")
  ) |> 
  select(c(iso3c, year, cgdppriv))

# Consumer Price Index / Inflation 
bis_cpi <- read_csv("data/raw/WS_LONG_CPI_csv_col.csv")

bis_cpi_clean <- bis_cpi |> 
  pivot_longer(
    cols = !(FREQ:Series),
    names_to = "Year",
    values_to = "inflation"
  ) |> 
  filter(
    grepl("^\\d{4}$", Year),
    `Unit of measure` == "Year-on-year changes, in per cent",
    Frequency == "Annual"
  ) |> 
  select(c(`Reference area`, inflation, Year)) |> 
  mutate(
    year = as.integer(Year),
    iso3c = countrycode(`Reference area`, origin = "country.name", destination = "iso3c"),
    inflation,
    .keep = "none"
  )

# Property Prices
bis_propprices <- read_csv("data/raw/WS_SPP_csv_col.csv")

bis_propprices_clean <- bis_propprices |>
  pivot_longer(
    cols = !(FREQ:Series),
    names_to = "Time",
    values_to = "Price"
  ) |> 
  mutate(year = as.integer(str_extract(Time, "\\d{4}"))) |>
  filter(
    `Unit of measure` == "Year-on-year changes, in per cent",
    Value == "Real",
    year >= "1970",
    !(`Reference area` %in% c("Advanced economies", "Euro area", "World", "Emerging market economies (aggregate)"))
  ) |> 
  # annualize quarterly data
  summarize(ppgrowth = mean(Price, na.rm = TRUE), .by = c(REF_AREA, year, Value)) |> 
  mutate(iso3c = countrycode(REF_AREA, origin = "iso2c", destination = "iso3c")) |> 
  select(- c(REF_AREA, Value))
  

## 2.3 Eurostat ===============================================================
str_eurostat <- read_xlsx("data/raw/str_eurostat.xlsx", sheet = 2, skip = 7, na = c("", "NA", ":"))

str_eurostat_clean <- str_eurostat |> 
  select(-starts_with("..")) |> 
  slice(2:20) |> 
  mutate(`1970` = as.numeric(`1970`)) |> 
  pivot_longer(
    cols = !TIME,
    names_to = "year",
    values_to = "str_eurostat"
  ) |> 
  mutate(
    iso3c = countrycode(TIME, origin = "country.name", destination = "iso3c"),
    year = as.integer(year),
    str_eurostat,
    .keep = "none"
  )

## 2.4 IMF ====================================================================

### 2.4.1 Global Debt Database (GDD) ========================================
gdd <- read_csv("data/raw/GDD.csv")

gdd_clean <- clean_data(gdd, indicator_col = "indicator") |> 
  rename(
    "ngdpdc" = gross_domestic_product_gdp_current_prices_domestic_currency,
    "cgdppriv" = debt_securities_and_loans_private_sector_percent_of_gdp,
    "cgdpcorp" = debt_securities_and_loans_non_financial_corporations_percent_of_gdp,
    "cgdph" = debt_securities_and_loans_households_percent_of_gdp,
    "pubdebt" = debt_instruments_public_sector_percent_of_gdp,
    "pubnfdebt" = debt_instruments_public_non_financial_sector_percent_of_gdp,
    "privdebt" = debt_instruments_private_sector_percent_of_gdp,
    "corpdebt" = debt_instruments_non_financial_corporations_percent_of_gdp,
    "hdebt" = debt_instruments_households_percent_of_gdp,
    "ggovdebt" = debt_instruments_general_government_percent_of_gdp,
    "govcgdp" = debt_instruments_central_government_percent_of_gdp
  )

### 2.4.2 Africa Regional Economic Outlook (AFRREO) ===========================
afrreo <- read_csv("data/raw/AFRREO.csv")

afrreo_clean <- afrreo |> 
  clean_names() |> 
  filter(
    indicator %in% c(
      "All Items, Consumer price index (CPI), Period average, Year-over-year (YOY) percent change",
      "Broad money, Percent change",
      "Broad money, Percent of GDP",
      "Credit to the private sector, Percent change",
      "Credit to the private sector, Percent of GDP",
      "Current account balance (credit less debit), Percent of GDP",
      "Gross domestic product (GDP), Constant prices, Percent change"
    ),
    !(country %in% c(
      "Sub-Saharan Africa excluding Nigeria and South Africa", 
      "SSA oil-exporting countries excluding South Africa",
      "SSA oil-exporting countries excluding Nigeria",
      "SSA oil-importing countries excluding South Africa",
      "SSA middle-income countries excluding South Africa and Nigeria"
    ))
  ) |> 
  clean_data(indicator_col = "indicator") |> 
  rename(
    "bmoneygr" = broad_money_percent_change,
    "bmoneygdp" = broad_money_percent_of_gdp,
    "inflation" = all_items_consumer_price_index_cpi_period_average_year_over_year_yoy_percent_change,
    "bcagdp" = current_account_balance_credit_less_debit_percent_of_gdp,
    "privloansgr" = credit_to_the_private_sector_percent_change,
    "cgdppriv" = credit_to_the_private_sector_percent_of_gdp,
    "rgdpgrowth" = gross_domestic_product_gdp_constant_prices_percent_change
  )

### 2.4.3 World Economic Outlook (WEO) ========================================
weo <- read_csv("data/raw/WEO.csv")

weo_clean <- clean_data(weo, indicator_col = "indicator") |> 
  rename(
    "ngdpbil" = gross_domestic_product_gdp_current_prices_domestic_currency,
    "inflation" = all_items_consumer_price_index_cpi_period_average_percent_change,
    "govcgdp" = gross_debt_general_government_percent_of_gdp,
    "bcagdp" = current_account_balance_credit_less_debit_percent_of_gdp
  ) |> 
  mutate(
    ngdpmil = ngdpbil * 1000,
    ngdp = ngdpbil * 1000000000
  )

### 2.4.4 Public Finances in Modern History (PFMH) ==============================
pfmh <- read_xlsx("data/raw/PFMH.xlsx")

pfmh_clean <- pfmh |> 
  rename(
    "iso3c" = isocode,
    "rgdpgrowth" = rgc,
    "govcgdp_pfmh" = d
  )

### 2.4.5 Monetary Financial Statistics (MFS) ===================================

# Net Foreign Assets
nfa_mfs <- read_csv("data/raw/NetAssets_IMF_MFS.csv")

nfa_mfs <- nfa_mfs |> 
  clean_names() |> 
  mutate(
    priority = case_when(
      type_of_transformation == "Domestic currency" ~ 1,
      type_of_transformation == "Euro" ~ 2,
      type_of_transformation == "US dollar" ~ 3,
      TRUE ~ 99
    )
  ) |>
  arrange(country, priority) |>
  group_by(country) |>
  slice(1) |>
  ungroup()

nfa_mfs_clean <- clean_data(nfa_mfs) |>
  rename(nfa_mfs = value)


# Interest rates
mfs_str <- read_csv("data/raw/IMF_MFS_STR.csv")
mfs_ltr <- read_csv("data/raw/IMF_MFS_LTR.csv")

mfs_str_clean <- clean_data(mfs_str, indicator_col = "indicator") |> 
  rename(
    "tbyield" = government_securities_treasury_bills_yields_rate_percent_per_annum,
    "mmrate" = money_market_rate_percent_per_annum) |> 
  mutate(
    str_mfs = coalesce(mmrate, tbyield)
  )

mfs_ltr_clean <- clean_data(mfs_ltr) |>
  rename(mfs_ltr = value)

# Broad Money
bmoney_mfs <- read_csv("data/raw/IMF_MFS_BroadMoney.csv")

bmoney_mfs_clean <- bmoney_mfs |> 
  clean_names() |> 
  filter(!(indicator %in% c("Broad Money, Seasonally adjusted (SA)", "Broad Money, M5"))) |> 
  clean_data(indicator_col = "indicator")


# Loans-to-Deposit Ratio
ltd_mfs <- read_csv("data/raw/loans_to_deposit_mfs.csv")

ltd_mfs_clean <- clean_data(ltd_mfs, indicator_col = "indicator") |> 
  rename(
    "loans" = assets_claims_on_private_sector,
    "transdep" = liabilities_transferable_deposits_included_in_broad_money,
    "othdep" = liabilities_other_deposits_included_in_broad_money
  ) |> 
  mutate(ltd = (loans / (transdep + othdep)) * 100)


# Share prices
sp_mfs <- read_csv("data/raw/sp_mfs.csv")

sp_mfs_clean <- clean_data(sp_mfs, indicator_col = "type_of_transformation") |> 
  rename(
    "sppa" = period_average_index,
    "speop" = end_of_period_eo_p_index
  ) |> 
  # Choose the longest mfs share price series (period avrg. vs end-of-period)
  combine_longest_series("sp", c("sppa", "speop"))

### 2.4.6 National Economic Accounts (NEA) ================================
nea <- read_csv("data/raw/nea.csv")

nea_clean <- clean_data(nea, indicator_col = "price_type") |> 
  rename(
    "ngdp" = current_prices,
    "rgdp" = constant_prices
  ) |> 
  arrange(iso3c, year) |> 
  group_by(iso3c) |> 
  mutate(
    ngdpmil = ngdp / 1000000,
    ngdpbil = ngdp / 1000000000,
    rgdpgrowth = (log(rgdp) - lag(log(rgdp))) * 100
  )

## 2.5 OECD =========================================================

# Share Price Indices
sp_oecd <- read_xlsx("data/raw/sp_oecd.xlsx", skip = 5)

sp_oecd_clean <- sp_oecd |> 
  clean_names() |> 
  slice_head(n = -2) |> 
  slice_tail(n = -1) |> 
  select(-c(time_period_2, last_col())) |> 
  rename("country" = time_period_1) |> 
  clean_data() |> 
  rename("sp" = value)

# Interest Rates
ir_oecd <- read_xlsx("data/raw/ir_oecd.xlsx", skip = 4)

ir_oecd_clean <- ir_oecd |> 
  clean_names() |> 
  slice_head(n = -2) |> 
  slice_tail(n = -1) |> 
  select(-c(time_period_3, last_col())) |> 
  rename(
    "indicator" = time_period_1,
    "country" = time_period_2
  ) |> 
  clean_data(indicator_col = "indicator") |> 
  rename(
    "ltr_oecd" = long_term_interest_rates,
    "str_oecd" = short_term_interest_rates
  )

# Property Prices
pp_oecd <- read_xlsx("data/raw/pp_oecd.xlsx", skip = 5)

pp_oecd_clean <- pp_oecd |> 
  clean_names() |> 
  slice_head(n = -2) |> 
  slice_tail(n = -1) |> 
  select(-c(time_period_2, last_col())) |> 
  rename("country" = time_period_1) |> 
  clean_data() |> 
  rename("pp" = value)

# Calculate growth
pp_oecd_clean <- pp_oecd_clean |> 
  group_by(iso3c) |> 
  mutate(ppgrowth = (log(pp) - lag(log(pp))) * 100)

## 2.6 World Bank ==================================================

### 2.6.1 World Development Indicators (WDI) =======================
wdi1 <- read_csv("data/raw/wdi1.csv", na = c("", "NA", ".."))
wdi2 <- read_csv("data/raw/wdi2.csv", na = c("", "NA", ".."))

wdi1_clean <- wdi1 |> 
  slice_head(n = -5) |> 
  clean_data(indicator_col = "series_name", country_col = country_name) |> 
  rename(
    "bm" = broad_money_current_lcu,
    "bmtr" = broad_money_to_total_reserves_ratio,
    "bmgrowth" = broad_money_growth_annual_percent,
    "bmgdp" = broad_money_percent_of_gdp
  ) |> 
  mutate(
    bm = bm / 1000000
  )

wdi2_clean <- wdi2 |> 
  slice_head(n = -5) |> 
  clean_data(indicator_col = "series_name", country_col = country_name) |> 
  rename(
    "nfa" = net_foreign_assets_current_lcu,
    "bcagdp_wdi" = current_account_balance_percent_of_gdp,
    "ngdp" = gdp_current_lcu,
    "cgdppriv" = domestic_credit_to_private_sector_percent_of_gdp,
    "bcgdpriv" = domestic_credit_to_private_sector_by_banks_percent_of_gdp,
    "trd" = total_reserves_includes_gold_current_us,
    "inflation" = inflation_consumer_prices_annual_percent
  ) |> 
  mutate(
    ngdpmil = ngdp / 1000000,
    ngdpbil = ngdp / 1000000000,
    nfa_wdi = nfa / 1000000
  )

### 2.6.2 Global Financial Development (GFD) =============================
gfd <- read_csv("data/raw/gfd.csv", na = c("", "NA", ".."), locale = locale(encoding = "Latin1"))

gfd_clean <- gfd |> 
  slice_head(n = -5) |> 
  clean_data(indicator_col = "series_name", country_col = country_name) |> 
  rename(
    "spr" = stock_market_return_percent_year_on_year,
    "ltd" = bank_credit_to_bank_deposits_percent,
    "bmgdp" = liquid_liabilities_to_gdp_percent
  )

## 2.7 Jordà-Schularick-Taylor Macrohistory Database (JST) ====================
jst <- read_xlsx("data/raw/JSTdatasetR6.xlsx")

jst_clean <- jst |> 
  rename(
    "iso3c" = iso,
    "str_jst" = stir,
    "ltr_jst" = ltrate
  )

# 3 Save cleaned datasets =====================================================

clean_names <- ls(pattern = "_clean$")

walk(clean_names, ~ write_rds(get(.x), file.path("data/interim", paste0(.x, ".rds"))))

write_rds(x = crises_merged, file = "data/interim/crises_merged.rds")
write_rds(x = crisis_start, file = "data/interim/crisis_start.rds")
write_rds(x = crisis_years, file = "data/interim/crisis_years.rds")

message("Step 1a: Cleaned data saved to data/interim/")


## 2.2 Banking Crises Data =================================================



## 2.3 Create country-year panel ============================================

# Country-Year Panel
panel <- expand_grid(
  Country = unique(crises_merged$Country),
  Year = 1970:2025
)

# Add country codes
panel <- panel |> 
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))

panel <- panel |> 
  left_join(crisis_years,
            by = c("Country", "Year")) |> 
  mutate(
    Crisis = replace_na(Crisis, 0)
  )

panel <- panel |> 
  left_join(crisis_start,
            by = c("Country", "Year")) |> 
  mutate(
    Crisis_Start = replace_na(Crisis_Start, 0)
  )

# Pre-Crisis indicator
panel <- panel |>
  group_by(Country) |>
  arrange(Year) |>
  mutate(
    PreCrisis2 = as.integer(
      lead(Crisis_Start, 1, default = 0) +
        lead(Crisis_Start, 2, default = 0) > 0
    ),
    PreCrisis3 = as.integer(
      lead(Crisis_Start, 1, default = 0) +
        lead(Crisis_Start, 2, default = 0) +
        lead(Crisis_Start, 3, default = 0) > 0
    ),
    PreCrisis4 = as.integer(
      lead(Crisis_Start, 1, default = 0) +
        lead(Crisis_Start, 2, default = 0) +
        lead(Crisis_Start, 3, default = 0) + 
        lead(Crisis_Start, 4, default = 0) > 0
    )
  ) |>
  ungroup() |> 
  arrange(Country)

# Categorize countries in Advanced Economies and Emerging/Developing Economies
advanced <- c(
  "AUS", "AUT", "BEL", "CAN", "CHE", "CYP", "CZE",
  "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR",
  "GRC", "HRV", "IRL", "ISL", "ISR", "ITA", "JPN",
  "KOR", "LTU", "LUX", "LVA", "MLT", "NLD", "NOR",
  "NZL", "PRT", "SGP", "SVK", "SVN", "SWE", "USA"
)

panel <- panel |>
  mutate(
    country_group = if_else(
      iso3c %in% advanced,
      "Advanced Economies",
      "Emerging and Developing Economies"
    )
  )


## 2.4 GDP =============================================================

### 2.4.1 Nominal ======================================================

# # NEA
# gdp_nea <- nea_clean |> 
#   select(-rgdp) |> 
#   mutate(
#     ngdpmil = ngdp / 1000000,
#     ngdpbil = ngdp / 1000000000
#   )

# WEO
# gdp_weo <- weo_clean |> 
#   filter(INDICATOR == "Gross domestic product (GDP), Current prices, Domestic currency") |> 
#   select(COUNTRY, Year, Value) |> 
#   mutate(
#     iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
#     Year = as.integer(Year),
#     ngdpbil = Value,
#     ngdpmil = Value * 1000,
#     ngdp = Value * 1000000000,
#     .keep = "none"
#   )

# WDI
# gdp_wdi <- wdi2_clean |> 
#   select(Year, iso3c, ngdp) |> 
#   mutate(
#     ngdpmil = ngdp / 1000000,
#     ngdpbil = ngdp / 1000000000
#   )


# Combine datasets
ngdp_comb <- nea_clean |> 
  select(iso3c, year, ngdp, ngdpmil, ngdpbil) |> 
  full_join(weo_clean |> select(iso3c, year, ngdp, ngdpmil, ngdpbil), by = c("iso3c", "year"), suffix = c("_nea", "_weo")) |> 
  full_join(wdi2_clean |> select(iso3c, year, ngdp, ngdpmil, ngdpbil), by = c("iso3c", "year")) |> 
  rename(
    "ngdp_wdi" = ngdp,
    "ngdpmil_wdi" = ngdpmil,
    "ngdpbil_wdi" = ngdpbil
  )

# 
# gdp_comb <- gdp_nea |> 
#   full_join(gdp_weo, by = c("iso3c", "Year"), suffix = c("_nea", "_weo")) |> 
#   full_join(gdp_wdi, by = c("iso3c", "Year")) |> 
#   rename(
#     "ngdp_wdi" = ngdp,
#     "ngdpmil_wdi" = ngdpmil,
#     "ngdpbil_wdi" = ngdpbil
#   )

# Choose the longest series per country
ngdp <- combine_longest_series(
  ngdp_comb,
  "ngdp",
  c("ngdp_nea", "ngdp_weo", "ngdp_wdi")
)

ngdpmil <- combine_longest_series(
  ngdp_comb,
  "ngdpmil",
  c("ngdpmil_nea", "ngdpmil_weo", "ngdpmil_wdi")
)

ngdpbil <- combine_longest_series(
  ngdp_comb,
  "ngdpbil",
  c("ngdpbil_nea", "ngdpbil_weo", "ngdpbil_wdi")
)


# Add to panel
panel <- left_join(panel, ngdp |> select(iso3c, Year, ngdp), by = c("iso3c", "Year"))
panel <- left_join(panel, ngdpmil |> select(iso3c, Year, ngdpmil), by = c("iso3c", "Year"))
panel <- left_join(panel, ngdpbil |> select(iso3c, Year, ngdpbil), by = c("iso3c", "Year"))


### 2.4.2 Real Growth ==================================================

# rgdp_nea <- nea_clean |> 
#   select(-ngdp) |> 
#   group_by(iso3c) |> 
#   mutate(rgdpgrowth = (log(rgdp) - lag(log(rgdp))) * 100)

# rgdp_pfmh <- pfmh |> 
#   select(isocode, year, rgc) |> 
#   rename(
#     "iso3c" = isocode,
#     "Year" = year,
#     "rgdpgrowth" = rgc
#   )

rgdp_afrreo <- afrreo_clean |> select(iso3c, Year, rgdpgrowth)

# Combine datasets

rgdp_comb <- nea_clean |> 
  select(iso3c, year, rgdpgrowth) |> 
  full_join(pfmh_clean |> select(iso3c, year, rgdpgrowth), by = c("iso3c", "year"), suffix = c("_nea", "_pfmh")) |> 
  full_join(afrreo_clean |> select(iso3c, year, rgdpgrowth), by = c("iso3c", "year")) |> 
  rename("rgdpgrowth_afrreo" = "rgdpgrowth")

# rgdp_comb <- rgdp_nea |> 
#   full_join(rgdp_pfmh, by = c("iso3c", "Year"), suffix = c("_nea", "_pfmh")) |> 
#   full_join(rgdp_afrreo, by = c("iso3c", "Year")) |> 
#   rename("rgdpgrowth_afrreo" = "rgdpgrowth")

# Choose the longest series per country

rgdp_comb <- combine_longest_series(
  rgdp_comb,
  "rgdpgrowth",
  c("rgdpgrowth_nea", "rgdpgrowth_pfmh", "rgdpgrowth_afrreo")
)

# Add to panel

panel <- left_join(panel, rgdp_comb |> select(iso3c, Year, rgdpgrowth), by = c("iso3c", "Year"))


## 2.6 Inflation Data =================================================

# cpi_weo_clean <- weo_clean |> 
#   filter(INDICATOR == "All Items, Consumer price index (CPI), Period average, percent change") |> 
#   mutate(
#     iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
#     Year = as.integer(Year),
#     inflation = Value,
#     .keep = "none"
#   )

inflation_wdi <- wdi2_clean |> select(Year, iso3c, inflation)


# Combine BIS and WEO

infl_combined <- bis_cpi_clean |> 
  full_join(cpi_weo_long, by = c("iso3c", "Year"), suffix = c("_bis", "_weo")) |> 
  full_join(inflation_wdi, by = c("iso3c", "Year")) |> 
  rename("inflation_wdi" = inflation)

# Choose the longest series per country

infl_combined <- combine_longest_series(infl_combined, "inflation", c("inflation_bis", "inflation_weo", "inflation_wdi"))


# Add to panel

panel <- left_join(panel, infl_combined |> select(iso3c, Year, inflation), by = c("iso3c", "Year"))


## 2.5 Debt Variables ===============================================

### 2.5.1 Private Debt ===============================================

# Credit to GDP ratio

# Combine everything

cgdp_comb <- cgdp_bis_clean |> 
  full_join(gdd_clean |> select(cgdppriv, iso3c, Year), by = c("iso3c", "Year"), suffix = c("_bis", "_gdd")) |> 
  full_join(afrreo_clean |> select(cgdppriv, iso3c, Year), by = c("iso3c", "Year")) |> 
  full_join(wdi2_clean |> select(cgdppriv, iso3c, Year), by = c("iso3c", "Year"), suffix = c("_afrreo", "_wdi"))


# Choose the longest series per country

cgdp_comb <- combine_longest_series(
  cgdp_comb,
  "cgdppriv",
  c(
    "cgdppriv_bis",
    "cgdppriv_gdd",
    "cgdppriv_afrreo",
    "cgdppriv_wdi"
  )
)

# Add data to panel
panel <- left_join(panel, cgdp_comb |> select(iso3c, Year, cgdppriv), by = c("iso3c", "Year"))

# Corporate and household credit to GDP
panel <- left_join(panel, gdd_clean |> select(iso3c, Year, cgdpcorp, cgdph), by = c("iso3c", "Year"))


# Create approximated credit column
credit_approx <- panel |> 
  select(iso3c, Year, cgdppriv, cgdpcorp, cgdph, ngdpbil) |> 
  mutate(
    tloanspriv_approx = cgdppriv / 100 * ngdpbil,
    tloanscorp_approx = cgdpcorp / 100 * ngdpbil,
    tloansh_approx = cgdph / 100 * ngdpbil,
    Year,
    iso3c,
    .keep = "none"
  ) 

# Combine actual credit dataset with approximated values
credit_comb <- credit_bis_clean |> 
  full_join(credit_approx, by = c("iso3c", "Year"))

# # Check compatibility
# check <- credit_comb |> 
#   filter(!is.na(tloanspriv), !is.na(tloanspriv_approx)) |> 
#   group_by(iso3c) |> 
#   summarise(
#     mean_diff = mean(tloanspriv - tloanspriv_approx),
#     rmse = sqrt(mean((tloanspriv - tloanspriv_approx)^2)),
#     n = n()
#   )
# 
# country_corr <- credit_comb |> 
#   group_by(iso3c) |> 
#   summarise(
#     cor =
#       if(sum(!is.na(tloanspriv) & !is.na(tloanspriv_approx)) >= 5)
#         cor(tloanspriv, tloanspriv_approx, use = "complete.obs")
#     else NA_real_
#   )

# Fill in missing values in tloanspriv with the approximated values
credit_comb <- credit_comb |> 
  mutate(
    tlpriv = coalesce(tloanspriv, tloanspriv_approx),
    tlcorp = coalesce(tloanscorp, tloanscorp_approx),
    tlh = coalesce(tloansh, tloansh_approx)
  )

# Calculate credit growth
credit_comb <- credit_comb |> 
  arrange(iso3c, Year) |> 
  group_by(iso3c) |> 
  mutate(
    tlpriv_growth = (log(tlpriv) - lag(log(tlpriv))) * 100,
    tlcorp_growth = (log(tlcorp) - lag(log(tlcorp))) * 100,
    tlh_growth = (log(tlh) - lag(log(tlh))) * 100,
    blpriv_growth = (log(bloanspriv) - lag(log(bloanspriv))) * 100
  )

# Calculate real credit growth
credit_comb <- credit_comb |> 
  left_join(panel |> select(Year, iso3c, inflation), by = c("iso3c", "Year")) |> 
  arrange(iso3c, Year) |> 
  group_by(iso3c) |> 
  mutate(
    tlpriv_rgrowth = tlpriv_growth - inflation,
    tlcorp_rgrowth = tlcorp_growth - inflation,
    tlh_rgrowth = tlh_growth - inflation,
    blpriv_rgrowth = blpriv_growth - inflation
  ) |> 
  ungroup()

# Add to panel
panel <- left_join(panel, credit_comb |>  select(Year, iso3c, ends_with("rgrowth")), by = c("iso3c", "Year"))


### 2.5.2 Public Debt =================================================

# govcgdp_weo <- weo_clean |> 
#   filter(INDICATOR == "Gross debt, General government, Percent of GDP") |> 
#   select(COUNTRY, INDICATOR, Year, Value) |> 
#   pivot_wider(
#     names_from = INDICATOR,
#     values_from = Value
#   ) |> 
#   mutate(
#     iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
#     Year = as.integer(Year),
#     govcgdp_weo = `Gross debt, General government, Percent of GDP`,
#     .keep = "none"
#   ) |> 
#   filter(!is.na(iso3c))

# govcgdp_gdd <- gdd_clean |> 
#   select(Year, iso3c, cgovdebt) |> 
#   rename(
#     "govcgdp_gdd" = cgovdebt
#   )

# govcgdp_pfmh <- pfmh |> 
#   select(year, isocode, d) |> 
#   rename(
#     "Year" = year,
#     "iso3c" = isocode,
#     "govcgdp_pfmh" = d
#   )

# Combine Datasets

govcgdp_comb <- govcgdp_gdd |> 
  full_join(govcgdp_pfmh, by = c("iso3c", "Year")) |> 
  full_join(govcgdp_weo, by = c("iso3c", "Year"))

# Choose the longest series per country

govcgdp_comb <- combine_longest_series(
  govcgdp_comb,
  "govcgdp",
  c(
    "govcgdp_weo",
    "govcgdp_pfmh",
    "govcgdp_gdd"
  )
)

# Add to panel

panel <- left_join(panel, govcgdp_comb |> select(iso3c, Year, govcgdp), by = c("iso3c", "Year"))

## 2.7 Current account balance (% of GDP) =============================

# wdi
bca_wdi_clean <- wdi2_clean |> select(c(Year, iso3c, bcagdp_wdi)) 

# WEO

# bca_weo_clean <- weo_clean |> 
#   filter(INDICATOR == "Current account balance (credit less debit), Percent of GDP") |> 
#   select(COUNTRY, Year, Value) |> 
#   mutate(
#     iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
#     Year = as.integer(Year),
#     bca_weo = Value,
#     .keep = "none"
#   )

# Combine datasets
bca_comb <- bca_wdi_clean |> 
  full_join(bca_weo_long, by = c("iso3c", "Year"))

# Choose the longest series per country

bca_comb <- combine_longest_series(
  bca_comb,
  "bcagdp",
  c(
    "bcagdp_wdi",
    "bca_weo"
  )
)

# Add to panel
panel <- left_join(panel, bca_comb |> select(iso3c, Year, bcagdp), by = c("iso3c", "Year"))



## 2.8 Property Prices =======================================

# Combine datasets
pp_comb <- bis_propprices_clean_annual |> 
  full_join(pp_oecd_long, by = c("iso3c", "Year"), suffix = c("_bis", "_oecd"))

# Choose longest series
pp_comb <- combine_longest_series(
  pp_comb,
  "ppgrowth",
  c(
    "ppgrowth_bis",
    "ppgrowth_oecd"
  )
)

# Add to panel
panel <- left_join(panel, pp_comb |> select(iso3c, Year, ppgrowth), by = c("iso3c", "Year"))




## 2.9 Net foreign assets ===================================================

# nfa_wdi_clean <- wdi2_clean |> 
#   select(Year, iso3c, nfa) |> 
#   mutate(
#     nfa_wdi = nfa / 1000000,
#     Year,
#     iso3c,
#     .keep = "none")

# Combine datasets

nfa_combined <- full_join(
  nfa_mfs_long,
  nfa_wdi_long,
  by = c("iso3c", "Year")
)


# Choose the longest series per country

nfa_combined <- combine_longest_series(
  nfa_combined,
  "nfa",
  c(
    "nfa_mfs",
    "nfa_wdi"
  )
)

# Compute NFA-to-GDP ratio
nfa_combined <- nfa_combined |> 
  left_join(panel |> select(Year, iso3c, ngdpmil), by = c("iso3c", "Year")) |> 
  mutate(nfagdp = (nfa / ngdpmil) * 100)

# Add to panel

panel <- left_join(panel, nfa_combined |> select(Year, iso3c, nfagdp), by = c("iso3c", "Year"))

## 2.10 Yield curve ===================================

# OECD

# IMF MFS

# Eurostat Short term rates

# IMF PFMH (Real government bond yield)

# pfmh_ltr <- pfmh |> 
#   select(isocode, year, rltir) |> 
#   rename(
#     "iso3c" = isocode,
#     "Year" = year
#   )

# JST

# ir_jst <- jst |> 
#   select(year, iso, stir, ltrate) |> 
#   rename(
#     "Year" = year,
#     "iso3c" = iso,
#     "str_jst" = stir,
#     "ltr_jst" = ltrate
#   )

# Merge all datasets

ir_comb <- ir_oecd_clean |> 
  full_join(mfs_str_long, by = c("iso3c", "Year")) |> 
  full_join(mfs_ltr_long, by = c("iso3c", "Year")) |> 
  full_join(str_eurostat_long, by = c("iso3c", "Year")) |> 
  full_join(pfmh_ltr, by = c("iso3c", "Year")) |> 
  full_join(ir_jst, by = c("iso3c", "Year")) |> 
  # Inflation for approximating long term nominal interest rate
  full_join(panel |> select(Year, iso3c, inflation), by = c("iso3c", "Year")) |> 
  mutate(
    str = coalesce(str_oecd, str_mfs, str_eurostat, str_jst),
    ltr_approx = rltir + inflation,
    ltr = coalesce(ltr_oecd, ltr_mfs, ltr_approx, ltr_jst),
    ycurve = ltr - str
  )

# Add variables to panel
panel <- left_join(panel, ir_comb |> select(iso3c, Year, ycurve), by = c("iso3c", "Year"))


## 2.11 Broad Money ========================================================

# bmoney_wdi_clean <- wdi1_clean |> 
#   select(-bmgrowth) |> 
#   mutate(
#     bm = bm / 1000000) 

# bmoney_jst <- jst |> 
#   select(year, iso, money) |> 
#   rename(
#     "iso3c" = iso,
#     "Year" = year
#     )

bmgdp_gfd <- gfd_clean |> select(Year, iso3c, bmgdp)


# Combine datasets

bmoney_combined <- bmoney_mfs_clean |> 
  full_join(bmoney_wdi_long, by = c("iso3c", "Year"), suffix = c("_mfs", "_wdi")) |>
  full_join(bmgdp_gfd, by = c("iso3c", "Year"), suffix = c("_wdi", "_gfd")) |> 
  # Nominal GDP for approximating broad money
  full_join(panel |> select(Year, iso3c, ngdpmil), by = c("iso3c", "Year")) |> 
  # full_join(bmoney_jst, by = c("iso3c", "Year")) |> 
  rename(
    "bmoney_mfs" = bmoney,
    "bmoney_wdi" = bm
    # "bmoney_jst" = money
  )

# Choose the longest series per country
bmgdp <- combine_longest_series(
  bmoney_combined,
  "bmgdp",
  c(
    "bmgdp_wdi",
    "bmgdp_gfd"
  )
)

# Approximate broad money
bmoney_combined <- bmoney_combined |> 
  full_join(bmgdp |> select(Year, iso3c, bmgdp), by = c("Year", "iso3c")) |>  
  mutate(bmoney_approx = bmgdp / 100 * ngdpmil)

# check <- bmoney_combined |> 
#   filter(!is.na(bmoney_wdi), !is.na(bmoney_approx)) |>
#   group_by(iso3c) |>
#   summarise(
#     mean_diff = mean(bmoney_wdi - bmoney_approx),
#     rmse = sqrt(mean((bmoney_wdi - bmoney_approx)^2)),
#     n = n()
#   )

bmoney <- combine_longest_series(
  bmoney_combined,
  "bmoney",
  c(
    "bmoney_mfs",
    "bmoney_wdi",
    "bmoney_approx"
    # "bmoney_jst"
  )
)

# # Rescale the JST broad money values for CAN, FRA, DEU and ITA from billions to millions 
# bmoney <- bmoney |>
#   mutate(
#     bmoney = if_else(
#       source == "bmoney_jst" &
#         iso3c %in% c("CAN", "FRA", "DEU", "ITA"),
#       bmoney * 1000,
#       bmoney
#     )
#   )


# Calculate broad money growth rate

bmoney <- bmoney |>
  arrange(iso3c, Year) |>
  group_by(iso3c) |>
  mutate(
    bmgrowth = (log(bmoney) - lag(log(bmoney))) * 100
  )

# Calculate real broad money growth
bmoney <- bmoney |>
  left_join(panel |> select(Year, iso3c, inflation), by = c("iso3c", "Year")) |> 
  arrange(iso3c, Year) |>
  group_by(iso3c) |>
  mutate(
    bm_rgrowth = bmgrowth - inflation
  )

# Add to panel

panel <- left_join(panel, bmoney |> select(Year, iso3c, bmtr, bm_rgrowth),
                   by = c("iso3c", "Year"))
panel <- left_join(panel, bmgdp |> select(Year, iso3c, bmgdp), by = c("iso3c", "Year"))


## 2.12 Loans-to-deposit ratio ===============================================

ltd_gfd_clean <- gfd_clean |> select(Year, iso3c, ltd)

ltd_jst <- jst |> select(year, iso, ltd) |> rename("Year" = year, "iso3c" = iso)

# Combine datasets
ltd_comb <- ltd_mfs_clean |> 
  full_join(ltd_gfd_long, by = c("iso3c", "Year"), suffix = c("_mfs", "_gfd")) |> 
  full_join(ltd_jst, by = c("iso3c", "Year")) |> 
  rename("ltd_jst" = ltd)

# Choose the longest series per country

ltd_comb <- combine_longest_series(
  ltd_comb,
  "ltd",
  c(
    "ltd_mfs",
    "ltd_gfd",
    "ltd_jst"
  )
)

# Add to panel

panel <- left_join(panel, ltd_comb |> select(iso3c, Year, ltd), by = c("iso3c", "Year"))


## 2.13 Share prices =========================================================

# GFD
spr_gfd <- gfd_clean |> select(Year, iso3c, spr) |> rename("spr_gfd" = spr)

# OECD


# IMF MFS


# Choose the longest mfs share price series (period avrg. vs end-of-period)

# sp_mfs_clean <- combine_longest_series(
#   sp_mfs_long,
#   "sp",
#   c(
#     "sppa",
#     "speop"
#   )
# )

# Combine datasets
sp_comb <- sp_oecd_clean |> 
  full_join(sp_mfs_clean |> select(Year, iso3c, sp), by = c("iso3c", "Year"), suffix = c("_oecd", "_mfs")) |> 
  full_join(spr_gfd, by = c("iso3c", "Year")) |> 
  # Convert values of 0 into NA
  mutate(
    sp_oecd = na_if(sp_oecd, 0),
    sp_mfs = na_if(sp_mfs, 0),
    spr_gfd = na_if(spr_gfd, 0)
  )

# Calculate returns
sp_comb <- sp_comb |> 
  group_by(iso3c) |> 
  mutate(
    spr_oecd = (log(sp_oecd) - lag(log(sp_oecd))) * 100,
    spr_mfs = (log(sp_mfs) - lag(log(sp_mfs))) * 100
  )

country_corr <- sp_comb |> 
  group_by(iso3c) |> 
  summarise(
    cor_oecd_mfs =
      if(sum(!is.na(spr_oecd) & !is.na(spr_mfs)) >= 5)
        cor(spr_oecd, spr_mfs, use = "complete.obs")
    else NA_real_
  )

# Choose the longest series

sp_comb <- combine_longest_series(
  sp_comb,
  "spr",
  c(
    "spr_oecd",
    "spr_mfs",
    "spr_gfd"
  )
)

# Compute real stock market return

sp_comb <- sp_comb |> 
  left_join(panel |> select(Year, iso3c, inflation), by = c("iso3c", "Year")) |> 
  arrange(iso3c, Year) |> 
  group_by(iso3c) |> 
  mutate(
    sprr = spr - inflation,
  ) |> 
  ungroup()

# Add to panel
panel <- left_join(panel, sp_comb |> select(iso3c, Year, sprr), by = c("iso3c", "Year"))


## 2.14 Check how many obs ===================================================

check <- panel |>
  group_by(Country) |>
  summarise(
    across(
      - (Year:PreCrisis4),
      ~ sum(!is.na(.x)),
      .names = "n_{.col}"
    )
  )

sort(colSums(check[,-1]), decreasing = T)

panel |> 
  select(cgdppriv, rgdpgrowth, inflation, govcgdp, bcagdp, bmgdp, ltd, nfagdp) |> 
  complete.cases() |> 
  sum()

predictors <- c(
  "cgdppriv", "rgdpgrowth", "inflation", "govcgdp", "bcagdp", "bmgdp", "ltd", "nfagdp"
)

panel_complete <- panel |> 
  filter(if_all(all_of(predictors), ~ !is.na(.)), Crisis != 1) 

panel_complete |> 
  summarize(
    n_precrisis2 = sum(PreCrisis2),
    n_precrisis3 = sum(PreCrisis3),
    n_precrisis4 = sum(PreCrisis4)
  )
