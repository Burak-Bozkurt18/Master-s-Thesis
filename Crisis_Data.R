# Project Information =========================================================
# Project:       Master's Thesis
# Author:        Burak Bozkurt
# Last modified: 30 June 2026

# 1 Load Packags ===========================================================
library(readxl)
library(tidyverse)
library(countrycode)

# 1 Create functions =======================================================

combine_longest_series <- function(data, indicator, sources) {
  
  counts <- data |>
    filter(Year >= 1970, Year <= 2025) |>
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

# 2 Read and Clean Data ====================================================

## 2.1 Read Datasets =======================================================

crises <- read_xlsx(
  path = "SYSTEMIC_BANKING_CRISES_DATABASE_2026.xlsx",
  sheet = 2
  )

# BIS
credit_bis <- read.csv("WS_TC_csv_col.csv")
cgdp_bis <- read_csv("WS_CREDIT_GAP_csv_col.csv")
bis_cpi <- read_csv("WS_LONG_CPI_csv_col.csv")

# Eurostat
str_eurostat <- read_xlsx("str_eurostat.xlsx", sheet = 2, skip = 7, na = c("", "NA", ":"))

# IMF Global Debt Database
gdd <- read_csv("GDD.csv")

gdd_long <- gdd |> 
  pivot_longer(
    cols = !(DATASET:SERIES_NAME),
    names_to = "Year",
    values_to = "value"
  ) |>
  select(COUNTRY, INDICATOR, Year, value) |> 
  pivot_wider(
    names_from = INDICATOR,
    values_from = value
  ) |> 
  mutate(
    iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
    Year = as.integer(Year),
    ngdpdc = `Gross domestic product (GDP), Current prices, Domestic currency`,
    cgdppriv = `Debt securities and loans, Private sector, Percent of GDP`,
    cgdpcorp = `Debt securities and loans, Non-financial corporations, Percent of GDP`,
    cgdph = `Debt securities and loans, Households, Percent of GDP`,
    pubdebt = `Debt instruments, Public sector, Percent of GDP`,
    pubnfdebt = `Debt instruments, Public non-financial Sector, Percent of GDP`,
    privdebt = `Debt instruments, Private sector, Percent of GDP`,
    corpdebt = `Debt instruments, Non-financial corporations, Percent of GDP`,
    hdebt = `Debt instruments, Households, Percent of GDP`,
    ggovdebt = `Debt instruments, General government, Percent of GDP`,
    cgovdebt = `Debt instruments, Central government, Percent of GDP`,
    .keep = "none"
  )

# IMF Africa Regional Economic Outlook
afrreo <- read_csv("AFRREO.csv")

afrreo_long <- afrreo |> 
  filter(
    INDICATOR %in% c(
      "All Items, Consumer price index (CPI), Period average, Year-over-year (YOY) percent change",
      "Broad money, Percent change",
      "Broad money, Percent of GDP",
      "Credit to the private sector, Percent change",
      "Credit to the private sector, Percent of GDP",
      "Current account balance (credit less debit), Percent of GDP",
      "Gross domestic product (GDP), Constant prices, Percent change"
    ),
    !(COUNTRY %in% c(
      "Sub-Saharan Africa excluding Nigeria and South Africa", 
      "SSA oil-exporting countries excluding South Africa",
      "SSA oil-exporting countries excluding Nigeria",
      "SSA middle-income countries excluding South Africa and Nigeria"
    ))
  ) |> 
  pivot_longer(
    cols = !(DATASET:AUTHOR),
    names_to = "Year",
    values_to = "value"
  ) |>
  select(COUNTRY, INDICATOR, Year, value) |> 
  pivot_wider(
    names_from = INDICATOR,
    values_from = value
  ) |> 
  mutate(
    iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
    Year = as.integer(Year),
    bmoneygr = `Broad money, Percent change`,
    bmoneygdp = `Broad money, Percent of GDP`,
    inflation = `All Items, Consumer price index (CPI), Period average, Year-over-year (YOY) percent change`,
    bcagdp = `Current account balance (credit less debit), Percent of GDP`,
    privloansgr = `Credit to the private sector, Percent change`,
    cgdppriv = `Credit to the private sector, Percent of GDP`,
    rgdpgrowth = `Gross domestic product (GDP), Constant prices, Percent change`,
    .keep = "none"
  )

# IMF World Economic Outlook
weo <- read_csv("WEO.csv")

weo_long <- weo |> 
  pivot_longer(
    cols = !(DATASET:PRIMARY_DOMESTIC_CURRENCY),
    names_to = "Year",
    values_to = "Value"
  ) 

# IMF Public Finances in Modern History
pfmh <- read_xlsx("PFMH.xlsx")

# IMF Monetary Financial Statistics
nfa_mfs <- read_csv("NetAssets_IMF_MFS.csv")
nfa_mfs <- nfa_mfs |>
  mutate(
    priority = case_when(
      TYPE_OF_TRANSFORMATION == "Domestic currency" ~ 1,
      TYPE_OF_TRANSFORMATION == "Euro" ~ 2,
      TYPE_OF_TRANSFORMATION == "US dollar" ~ 3,
      TRUE ~ 99
    )
  ) |>
  arrange(COUNTRY, priority) |>
  group_by(COUNTRY) |>
  slice(1) |>
  ungroup()

mfs_str <- read_csv("IMF_MFS_STR.csv")
mfs_ltr <- read_csv("IMF_MFS_LTR.csv")
bmoney_mfs <- read_csv("IMF_MFS_BroadMoney.csv")
ltd_mfs <- read_csv("loans_to_deposit_mfs.csv")
sp_mfs <- read_csv("sp_mfs.csv")

# OECD
sp_oecd <- read_xlsx("sp_oecd.xlsx", skip = 5)
ir_oecd <- read_xlsx("OECD_STIR_LTIR.xlsx", skip = 3)

# World Development Indicators
wdi1 <- read_csv("wdi1.csv", na = c("", "NA", ".."))
wdi2 <- read_csv("wdi2.csv", na = c("", "NA", ".."))

wdi1_long <- wdi1 |> 
  slice(1:748) |> 
  pivot_longer(
    cols = !(`Country Name`:`Series Code`),
    names_to = "Year",
    values_to = "Value"
  ) |> 
  mutate(
    iso3c = `Country Code`,
    `Series Name`,
    Year = as.integer(str_extract(Year, "\\d{4}")),
    Value,
    .keep = "none"
  ) |> 
  pivot_wider(
    names_from = `Series Name`,
    values_from = Value
  ) |> 
  rename(
    "bm" = `Broad money (current LCU)`,
    "bmtr" = `Broad money to total reserves ratio`,
    "bmgrowth" = `Broad money growth (annual %)`,
    "bmgdp" = `Broad money (% of GDP)`
  )
  

wdi2_long <- wdi2 |> 
  slice(1:561) |> 
  pivot_longer(
    cols = !(`Country Name`:`Series Code`),
    names_to = "Year",
    values_to = "Value"
  ) |> 
  mutate(
    iso3c = `Country Code`,
    `Series Name`,
    Year = as.integer(str_extract(Year, "\\d{4}")),
    Value,
    .keep = "none"
  ) |> 
  pivot_wider(
    names_from = `Series Name`,
    values_from = Value
  ) |> 
  rename(
    "nfa" = `Net foreign assets (current LCU)`,
    "bcagdp_wdi" = `Current account balance (% of GDP)`,
    "gdp" = `GDP (current LCU)`,
    "cgdppriv" = `Domestic credit to private sector (% of GDP)`,
    "bcgdpriv" = `Domestic credit to private sector by banks (% of GDP)`,
    "trd" = `Total reserves (includes gold, current US$)`
  )

# Jordà-Schularick-Taylor Macrohistory Database
jst <- read_xlsx("JSTdatasetR6.xlsx")

## 2.2 Banking Crises Data =================================================

# Select important columns
crises <- crises |> 
  select(Country, Start, End)


crises <- crises |> 
  
  # Remove rows that contain NA in column "Start"
  filter(!is.na(Start)) |> 
  
  # Extract annotation symbols (e.g. "6/")
  mutate(
    annotation_country = str_extract(Country, "\\d+/"),
    annotation_year = str_extract(End, "\\d+/"),
    
    # Remove annotations from Country and End
    Country = str_trim(str_remove(Country, "\\s*\\d+/")),
    End = str_trim(str_remove(End, "\\s*\\d+/"))
  ) |> 
  
  # Replace "..." in End with 2025
  mutate(
    End = ifelse(End == "…", "2025", End)
  ) |> 
  
  # Convert End to numeric
  mutate(
    End = as.numeric(End)
  )

# Remove duplicates

crises_merged <- crises |> 
  arrange(Country, Start) |> 
  group_by(Country) |> 
  
  # Start a new crisis group whenever there is a gap
  mutate(
    new_group = if_else(
      Start > lag(End, default = first(Start) - 2) + 1,
      1L,
      0L
    ),
    crisis_group = cumsum(new_group)
  ) |> 
  
  group_by(Country, crisis_group) |> 
  summarise(
    Start = min(Start),
    End   = max(End),
    .groups = "drop"
  )

# Crisis years
crisis_years <- crises_merged |> 
  rowwise() |> 
  mutate(Year = list(Start:End)) |> 
  unnest(Year) |> 
  select(Country, Year) |> 
  mutate(Crisis = 1)

crisis_start <- crises_merged |> 
  mutate(
    Country,
    Year = Start,
    Crisis_Start = 1,
    .keep = "none"
  )

## 2.3 Create country-year panel ============================================

# Country-Year Panel
panel <- expand_grid(
  Country = unique(crises_merged$Country),
  Year = 1970:2025
)


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

# Add country codes

panel <- panel |> 
  mutate(iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"))

## 2.4 GDP =============================================================

### 2.4.1 Nominal ======================================================

# WEO
gdp_weo <- weo_long |> 
  filter(INDICATOR == "Gross domestic product (GDP), Current prices, Domestic currency") |> 
  select(COUNTRY, Year, Value) |> 
  mutate(
    iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
    Year = as.integer(Year),
    gdpbil = Value,
    gdpmil = Value * 1000,
    gdp = Value * 1000000000,
    .keep = "none"
  )

# WDI
gdp_wdi <- wdi2_long |> 
  select(Year, iso3c, gdp) |> 
  mutate(
    gdpmil = gdp / 1000000,
    gdpbil = gdp / 1000000000
    )


# Combine datasets
gdp_comb <- gdp_weo |> 
  full_join(gdp_wdi, by = c("iso3c", "Year"), suffix = c("_weo", "_wdi"))

# Choose the longest series per country
gdp <- combine_longest_series(
  gdp_comb,
  "gdp",
  c("gdp_weo", "gdp_wdi")
)

gdpmil <- combine_longest_series(
  gdp_comb,
  "gdpmil",
  c("gdpmil_weo", "gdpmil_wdi")
)

gdpbil <- combine_longest_series(
  gdp_comb,
  "gdpbil",
  c("gdpbil_weo", "gdpbil_wdi")
)


# Add to panel
panel <- left_join(panel, gdp |> select(iso3c, Year, gdp), by = c("iso3c", "Year"))
panel <- left_join(panel, gdpmil |> select(iso3c, Year, gdpmil), by = c("iso3c", "Year"))
panel <- left_join(panel, gdpbil |> select(iso3c, Year, gdpbil), by = c("iso3c", "Year"))


### 2.4.2 Real Growth ==================================================

rgdp_pfmh <- pfmh |> 
  select(isocode, year, rgc) |> 
  rename(
    "iso3c" = isocode,
    "Year" = year
  )

rgdp_afrreo <- afrreo_long |> select(iso3c, Year, rgdpgrowth)

# Combine datasets
rgdp_comb <- rgdp_pfmh |> 
  full_join(rgdp_afrreo, by = c("iso3c", "Year"))

# Choose the longest series per country

rgdp_comb <- combine_longest_series(
  rgdp_comb,
  "rgdp",
  c("rgc", "rgdpgrowth")
)

# Add to panel

panel <- left_join(panel, rgdp_comb |> select(iso3c, Year, rgdp), by = c("iso3c", "Year"))



## 2.5 Debt Variables ===============================================

### 2.5.1 Private Debt ===============================================

# Turn into long format
credit_bis_long <- credit_bis |> 
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Time",
    values_to = "Value" 
  ) |> 
  filter(
    Unit.type == "Domestic currency (incl. conv. to current ccy made using a fix parity)", 
    Valuation.method == "Market value",
    Adjustment == "Adjusted for breaks",
    Borrowing.sector %in% c("Private non-financial sector", "Households & NPISHs", "Non-financial corporations"),
    # Year end value
    str_detect(Time, "Q4")
    ) |> 
  mutate(
    Year = as.integer(str_extract(Time, "\\d{4}")),
    # Add country codes
    iso3c = countrycode(Borrowers..country, origin = "country.name", destination = "iso3c")
    ) |> 
  select(c(Borrowing.sector, Lending.sector, Year, Value, iso3c)) |> 
  pivot_wider( 
    names_from = Lending.sector,
    values_from = Value
  ) |> 
  pivot_wider(
    names_from = Borrowing.sector,
    values_from = c(`All sectors`, `Banks, domestic`)
  ) |> 
  mutate(
    Year,
    iso3c,
    tloanscorp = `All sectors_Non-financial corporations`,
    tloanspriv = `All sectors_Private non-financial sector`,
    tloansh = `All sectors_Households & NPISHs`,
    bloanspriv = `Banks, domestic_Private non-financial sector`,
    .keep = "none"
  )


# Credit to GDP ratio

cgdp_bis_long <- cgdp_bis |> 
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
    Year = as.integer(str_extract(Time, "\\d{4}")),
    iso3c = countrycode(`Borrowers' country`, origin = "country.name", destination = "iso3c")
    ) |> 
  select(c(iso3c, Year, cgdppriv))



# Combine everything

cgdp_comb <- cgdp_bis_long |> 
  full_join(gdd_long |> select(cgdppriv, iso3c, Year), by = c("iso3c", "Year"), suffix = c("_bis", "_gdd")) |> 
  full_join(afrreo_long |> select(cgdppriv, iso3c, Year), by = c("iso3c", "Year")) |> 
  full_join(wdi2_long |> select(cgdppriv, iso3c, Year), by = c("iso3c", "Year"), suffix = c("_afrreo", "_wdi"))


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
panel <- left_join(panel, gdd_long |> select(iso3c, Year, cgdpcorp, cgdph), by = c("iso3c", "Year"))


# Create approximated credit column
credit_approx <- panel |> 
  select(iso3c, Year, cgdppriv, cgdpcorp, cgdph, gdpbil) |> 
  mutate(
    tloanspriv_approx = cgdppriv / 100 * gdpbil,
    tloanscorp_approx = cgdpcorp / 100 * gdpbil,
    tloansh_approx = cgdph / 100 * gdpbil,
    Year,
    iso3c,
    .keep = "none"
  ) 

# Combine actual credit dataset with approximated values
credit_comb <- credit_bis_long |> 
  full_join(credit_approx, by = c("iso3c", "Year"))

# Check compatibility
check <- credit_comb |> 
  filter(!is.na(tloanspriv), !is.na(tloanspriv_approx)) |> 
  group_by(iso3c) |> 
  summarise(
    mean_diff = mean(tloanspriv - tloanspriv_approx),
    rmse = sqrt(mean((tloanspriv - tloanspriv_approx)^2)),
    n = n()
  )

country_corr <- credit_comb |> 
  group_by(iso3c) |> 
  summarise(
    cor =
      if(sum(!is.na(tloanspriv) & !is.na(tloanspriv_approx)) >= 5)
        cor(tloanspriv, tloanspriv_approx, use = "complete.obs")
    else NA_real_
  )

# Fill in missing values in tloanspriv with the approximated values
credit_comb <- credit_comb |> 
  mutate(
    tloanspriv = coalesce(tloanspriv, tloanspriv_approx),
    tloanscorp = coalesce(tloanscorp, tloanscorp_approx),
    tloansh = coalesce(tloansh, tloansh_approx)
    )

# Add to panel
panel <- left_join(panel, credit_comb |> select(- c(tloanspriv_approx, tloanscorp_approx, tloansh_approx) ), by = c("iso3c", "Year"))

# Calculate credit growth

panel <- panel |>
  arrange(iso3c, Year) |>
  group_by(iso3c) |>
  mutate(
    tloanspriv_growth = (log(tloanspriv) - lag(log(tloanspriv))) * 100,
    tloanscorp_growth = (log(tloanscorp) - lag(log(tloanscorp))) * 100,
    tloansh_growth = (log(tloansh) - lag(log(tloansh))) * 100
  ) |>
  ungroup()


### 2.5.2 Public Debt =================================================

govcgdp_gdd <- gdd_long |> 
  select(Year, iso3c, cgovdebt) |> 
  rename(
    "govcgdp_gdd" = cgovdebt
  )

govcgdp_pfmh <- pfmh |> 
  select(year, isocode, d) |> 
  rename(
    "Year" = year,
    "iso3c" = isocode,
    "govcgdp_pfmh" = d
    )

# Combine Datasets

govcgdp_comb <- govcgdp_gdd |> 
  full_join(govcgdp_pfmh, by = c("iso3c", "Year"))

# Choose the longest series per country

govcgdp_comb <- combine_longest_series(
  govcgdp_comb,
  "govcgdp",
  c(
    "govcgdp_pfmh",
    "govcgdp_gdd"
  )
)

# Add to panel

panel <- left_join(panel, govcgdp_comb |> select(iso3c, Year, govcgdp), by = c("iso3c", "Year"))




## 2.6 Inflation Data =================================================



bis_cpi_long <- bis_cpi |> 
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
    Year = as.integer(Year),
    iso3c = countrycode(`Reference area`, origin = "country.name", destination = "iso3c"),
    inflation,
    .keep = "none"
    )





cpi_weo_long <- weo_long |> 
  filter(INDICATOR == "All Items, Consumer price index (CPI), Period average, percent change") |> 
  mutate(
    iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
    Year = as.integer(Year),
    inflation = Value,
    .keep = "none"
  )


# Combine BIS and WEO

infl_combined <- bis_cpi_long |> 
  full_join(cpi_weo_long, by = c("iso3c", "Year"), suffix = c("_bis", "_weo"))

# Choose the longest series per country

infl_combined <- combine_longest_series(
  infl_combined,
  "inflation",
  c(
    "inflation_bis",
    "inflation_weo"
  )
)


# Add to panel

panel <- left_join(panel, infl_combined |> select(iso3c, Year, inflation), by = c("iso3c", "Year"))


## 2.7 Current account balance (% of GDP) =============================

# wdi
bca_wdi_long <- wdi2_long |> select(c(Year, iso3c, bcagdp_wdi)) 

# WEO

bca_weo_long <- weo_long |> 
  filter(INDICATOR == "Current account balance (credit less debit), Percent of GDP") |> 
  select(COUNTRY, Year, Value) |> 
  mutate(
    iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
    Year = as.integer(Year),
    bca_weo = Value,
    .keep = "none"
  )

# Combine datasets
bca_comb <- bca_wdi_long |> 
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

bis_propprices <- read_csv("WS_SPP_csv_col.csv")

bis_propprices_long <- bis_propprices |>
  pivot_longer(
    cols = !(FREQ:Series),
    names_to = "Time",
    values_to = "Price"
  ) |> 
  mutate(Year = as.integer(str_extract(Time, "\\d{4}"))) |>
  filter(
    `Unit of measure` == "Year-on-year changes, in per cent",
    Value == "Real",
    Year >= "1970",
    !(`Reference area` %in% c("Advanced economies", "Euro area", "World", "Emerging market economies (aggregate)"))
    )

  
# Annualize quarterly data
bis_propprices_long_annual <- bis_propprices_long |>
  summarize(proppgrowth = mean(Price, na.rm = TRUE), .by = c(REF_AREA, Year, Value)) |> 
  mutate(iso3c = countrycode(REF_AREA, origin = "iso2c", destination = "iso3c")) |> 
  select(- c(REF_AREA, Value))

# Add to panel
panel <- left_join(panel, bis_propprices_long_annual,
                                by = c("iso3c", "Year"))



## 2.9 Net foreign assets ===================================================

nfa_wdi_long <- wdi2_long |> 
  select(Year, iso3c, nfa) |> 
  mutate(
    nfa_wdi = nfa / 1000000,
    Year,
    iso3c,
    .keep = "none")


nfa_mfs_long <- nfa_mfs |> 
  pivot_longer(
    cols = !(DATASET:SCALE),
    names_to = "Year",
    values_to = "Value"
  ) |> 
  mutate(
    iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
    Year = as.integer(Year),
    nfa_mfs = Value,
    .keep = "none")


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



# Add to panel

panel <- left_join(panel, nfa_combined |> select(Year, iso3c, nfa), by = c("iso3c", "Year"))

## 2.10 Yield curve ===================================

# OECD

# Get rid of useless rows and column
ir_oecd <- ir_oecd |> 
  slice_head(n = -2) |> 
  select(-last_col())

ir_oecd_long <- ir_oecd |>
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "Year",
    values_to = "Value"
  ) |> 
  pivot_wider(
    names_from = Measure,
    values_from = Value
  ) |> 
  mutate(
    Year = as.integer(Year),
    iso3c = countrycode(`Reference area`, origin = "country.name", destination = "iso3c"),
    ltr_oecd = `Long-term interest rates`,
    str_oecd = `Short-term interest rates`,
    .keep = "none"
  )



# IMF MFS

mfs_str_long <- mfs_str |> 
  pivot_longer(
    cols = !(DATASET:SCALE),
    names_to = "Year",
    values_to = "Value"
  ) |> 
  mutate(
    COUNTRY,
    INDICATOR,
    Year = as.integer(Year),
    Value,
    .keep = "none") |> 
  pivot_wider(
    names_from = INDICATOR,
    values_from = Value
  ) |> 
  rename(
    "tbyield" = `Government securities: Treasury bills yields, Rate, Percent per annum`,
    "mmrate" = `Money market Rate, Percent per annum`) |> 
  mutate(
    str_mfs = coalesce(mmrate, tbyield),
    iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
    Year,
    .keep = "none"
  )

mfs_ltr_long <- mfs_ltr |> 
  pivot_longer(
    cols = !(DATASET:SCALE),
    names_to = "Year",
    values_to = "Value"
  ) |> 
  mutate(
    iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
    Year = as.integer(Year),
    ltr_mfs = Value,
    .keep = "none")


# Eurostat Short term rates


str_eurostat_long <- str_eurostat |> 
  select(-starts_with("..")) |> 
  slice(2:20) |> 
  mutate(`1970` = as.numeric(`1970`)) |> 
  pivot_longer(
    cols = !TIME,
    names_to = "Year",
    values_to = "str_eurostat"
  ) |> 
  mutate(
    iso3c = countrycode(TIME, origin = "country.name", destination = "iso3c"),
    Year = as.integer(Year),
    str_eurostat,
    .keep = "none"
  )



# IMF PFMH (Real government bond yield)

pfmh_ltr <- pfmh |> 
  select(isocode, year, rltir) |> 
  rename(
    "iso3c" = isocode,
    "Year" = year
  )

# Merge all datasets

ir_comb <- ir_oecd_long |> 
  full_join(mfs_str_long, by = c("iso3c", "Year")) |> 
  full_join(mfs_ltr_long, by = c("iso3c", "Year")) |> 
  full_join(str_eurostat_long, by = c("iso3c", "Year")) |> 
  full_join(pfmh_ltr, by = c("iso3c", "Year")) |> 
  # Inflation for approximating long term nominal interest rate
  full_join(panel |> select(Year, iso3c, inflation), by = c("iso3c", "Year")) |> 
  mutate(
    str = coalesce(str_oecd, str_mfs, str_eurostat),
    ltr_approx = rltir + inflation,
    ltr = coalesce(ltr_oecd, ltr_mfs, ltr_approx),
    ycurve = ltr - str
  )

# Add variables to panel
panel <- left_join(panel, ir_comb |> select(iso3c, Year, ycurve), by = c("iso3c", "Year"))


## 2.11 Broad Money ========================================================

bmoney_wdi_long <- wdi1_long |> 
  select(-bmgrowth) |> 
  mutate(
    bm = bm / 1000000) 

bmoney_mfs_long <- bmoney_mfs |> 
  pivot_longer(
    cols = !(DATASET:SCALE),
    names_to = "Year",
    values_to = "bmoney"
  ) |> 
  filter(
    !(INDICATOR %in% c("Broad Money, Seasonally adjusted (SA)", "Broad Money, M5"))
  ) |> 
  mutate(
    iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
    Year = as.integer(Year),
    bmoney,
    .keep = "none")

bmoney_jst <- jst |> 
  select(year, iso, money) |> 
  rename(
    "iso3c" = iso,
    "Year" = year
    )

# Combine datasets

bmoney_combined <- bmoney_mfs_long |> 
  full_join(bmoney_wdi_long, by = c("iso3c", "Year"), suffix = c("_mfs", "_wdi")) |>
  full_join(bmoney_jst, by = c("iso3c", "Year")) |> 
  rename(
    "bmoney_mfs" = bmoney,
    "bmoney_wdi" = bm,
    "bmoney_jst" = money
  )

# Choose the longest series per country
bmoney_combined <- combine_longest_series(
  bmoney_combined,
  "bmoney",
  c(
    "bmoney_mfs",
    "bmoney_wdi",
    "bmoney_jst"
  )
)

# Rescale the JST broad money values for CAN, FRA, DEU and ITA from billions to millions 
bmoney_combined <- bmoney_combined |>
  mutate(
    bmoney = if_else(
      source == "bmoney_jst" &
        iso3c %in% c("CAN", "FRA", "DEU", "ITA"),
      bmoney * 1000,
      bmoney
    )
  )


# Calculate broad money growth rate

bmoney_combined <- bmoney_combined |>
  arrange(iso3c, Year) |>
  group_by(iso3c) |>
  mutate(
    bmgrowth = (log(bmoney) - lag(log(bmoney))) * 100
  ) |>
  ungroup()

# Add to panel

panel <- left_join(panel, bmoney_combined |> select(Year, iso3c, bmoney, bmtr, bmgrowth, bmgdp),
                      by = c("iso3c", "Year"))


## 2.12 Loans-to-deposit ratio ===============================================

ltd_gfd <- read_csv("loans_to_deposit_gfd.csv", na = c("", "NA", ".."), locale = locale(encoding = "Latin1"))


ltd_gfd_long <- ltd_gfd |> 
  slice(11:225) |> 
  pivot_longer(
    cols = !(`Series Name`:`Country Code`),
    names_to = "Year",
    values_to = "ltd"
  ) |> 
  mutate(
    iso3c = `Country Code`,
    Year = as.integer(str_extract(Year, "^\\d{4}")),
    ltd,
    .keep = "none"
  )

ltd_mfs_long <- ltd_mfs |> 
  pivot_longer(
    cols = !(DATASET:SCALE),
    names_to = "Year",
    values_to = "Value"
  ) |> 
  select(COUNTRY, INDICATOR, Year, Value) |> 
  pivot_wider(
    names_from = INDICATOR,
    values_from = Value
  ) |> 
  mutate(
    iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
    Year = as.integer(Year),
    loans = `Assets, Claims on Private sector`,
    transdep = `Liabilities, Transferable Deposits, Included In Broad Money`,
    othdep = `Liabilities, Other Deposits, Included In Broad Money`,
    .keep = "none")

ltd_mfs_long <- ltd_mfs_long |> 
  mutate(ltd = (loans / (transdep + othdep)) * 100)

ltd_jst <- jst |> select(year, iso, ltd) |> rename("Year" = year, "iso3c" = iso)

# Combine datasets
ltd_comb <- ltd_mfs_long |> 
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

# OECD
sp_oecd_long <- sp_oecd |> 
  slice(2:49) |> 
  select(-c(`Time period...2`, `...76`)) |> 
  rename("Country" = `Time period...1`) |> 
  pivot_longer(
    cols = -Country,
    names_to = "Year",
    values_to = "sp"
  ) |> 
  mutate(
    Year = as.integer(Year),
    iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"),
    sp,
    .keep = "none"
  )

# IMF MFS
sp_mfs_long <- sp_mfs |> 
  pivot_longer(
    cols = !(DATASET:SCALE),
    names_to = "Year",
    values_to = "sp"
  ) |> 
  select(COUNTRY, Year, sp, TYPE_OF_TRANSFORMATION) |> 
  pivot_wider(
    names_from = TYPE_OF_TRANSFORMATION,
    values_from = sp
  ) |> 
  mutate(
    iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c"),
    Year = as.integer(Year),
    sppa = `Period average, Index`,
    speop = `End-of-period (EoP), Index`,
    .keep = "none")

# Choose the longest mfs share price series (period avrg. vs end-of-period)

sp_mfs_long <- combine_longest_series(
  sp_mfs_long,
  "sp",
  c(
    "sppa",
    "speop"
  )
)

# Combine datasets
sp_comb <- sp_oecd_long |> 
  full_join(sp_mfs_long |> select(Year, iso3c, sp), by = c("iso3c", "Year"), suffix = c("_oecd", "_mfs"))

# Rebase the MFS series
# IMF -> 2015 = 100

base2015 <- sp_comb |>
  filter(Year == 2015) |>
  select(iso3c, base2015 = sp_mfs)

sp_comb <- sp_comb |>
  left_join(base2015, by = "iso3c") |>
  mutate(
    sp_mfs2015 = sp_mfs / base2015 * 100
  ) |>
  select(-base2015)

# Choose the longest series

sp_comb <- combine_longest_series(
  sp_comb,
  "sp",
  c(
    "sp_oecd",
    "sp_mfs2015"
  )
)

# Add to panel
panel <- left_join(panel, sp_comb |> select(iso3c, Year, sp), by = c("iso3c", "Year"))


## 2.14 Check how many obs ===================================================

check <- panel |>
  group_by(Country) |>
  summarise(
    across(
      - (Year:iso3c),
      ~ sum(!is.na(.x)),
      .names = "n_{.col}"
    )
  )

sort(colSums(check[,-1]), decreasing = T)

panel |> 
  select(cgdppriv, rgdp, inflation, govcgdp, bcagdp, tloanspriv_growth, ltd, bmgrowth, nfa, bmgdp, bmtr, sp) |> 
  complete.cases() |> 
  sum()

check |>
  mutate(total_obs = rowSums(across(-Country), na.rm = TRUE)) |>
  arrange(desc(total_obs))

