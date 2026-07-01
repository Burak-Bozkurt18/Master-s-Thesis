# Project Information =========================================================
# Project:       Master's Thesis
# Author:        Burak Bozkurt
# Last modified: 30 June 2026

# 1 Load Packags ===========================================================
library(readxl)
library(tidyverse)
library(countrycode)

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
    "gdp" = `GDP (current LCU)`
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

# WDI

panel <- left_join(panel, wdi2_long |> select(Year, iso3c, gdp) , by = c("iso3c", "Year"))

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

counts <- rgdp_comb |> 
  filter(Year >= "1970" & Year <= "2025") |> 
  group_by(iso3c) |> 
  summarise(
    n_pfmh = sum(!is.na(rgc)),
    n_afrreo = sum(!is.na(rgdpgrowth)),
  ) |>
  mutate(
    source = case_when(
      n_pfmh >= n_afrreo  ~ "pfmh",
      TRUE            ~ "afrreo"
    )
  )

rgdp_comb <- rgdp_comb |> 
  left_join(counts |> select(iso3c, source), by = "iso3c")

rgdp_comb <- rgdp_comb |> 
  mutate(
    rgdp = case_when(
      source == "pfmh"    ~ rgc,
      source == "afrreo"    ~ rgdpgrowth,
    )
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
  rename(
    "cgdppriv_afrreo" = cgdppriv
  )





# Choose the longest series per country

counts <- cgdp_comb |> 
  filter(Year >= "1970" & Year <= "2025") |> 
  group_by(iso3c) |> 
  summarise(
    n_bis = sum(!is.na(cgdppriv_bis)),
    n_gdd = sum(!is.na(cgdppriv_gdd)),
    n_afr = sum(!is.na(cgdppriv_afrreo))
  ) |>
  mutate(
    source = case_when(
      n_bis >= n_gdd & n_bis >= n_afr ~ "bis",
      n_gdd >= n_afr                  ~ "gdd",
      TRUE                            ~ "afrreo"
    )
  )

cgdp_comb <- cgdp_comb |> 
  left_join(counts |> select(iso3c, source), by = "iso3c")

cgdp_comb <- cgdp_comb |> 
  mutate(
    cgdppriv = case_when(
      source == "bis"    ~ cgdppriv_bis,
      source == "gdd"    ~ cgdppriv_gdd,
      source == "afrreo" ~ cgdppriv_afrreo
    )
  )

# Add data to panel
panel <- left_join(panel, cgdp_comb |> select(iso3c, Year, cgdppriv), by = c("iso3c", "Year"))

# Corporate and household credit to GDP
panel <- left_join(panel, gdd_long |> select(iso3c, Year, cgdpcorp, cgdph), by = c("iso3c", "Year"))


# Create approximated credit column
credit_approx <- panel |> 
  select(iso3c, Year, cgdppriv, gdp) |> 
  mutate(
    tloanspriv_approx = cgdppriv / 100 * gdp,
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
  mutate(tloanspriv = coalesce(tloanspriv, tloanspriv_approx))

# Add to panel
panel <- left_join(panel, credit_comb |> select(-tloanspriv_approx), by = c("iso3c", "Year"))

# Calculate credit growth

panel <- panel |>
  arrange(iso3c, Year) |>
  group_by(iso3c) |>
  mutate(
    tloanspriv_growth = (log(tloanspriv) - lag(log(tloanspriv))) * 100
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

counts <- govcgdp_comb |> 
  filter(Year >= "1970" & Year <= "2025") |> 
  group_by(iso3c) |> 
  summarise(
    n_pfmh = sum(!is.na(govcgdp_pfmh)),
    n_gdd = sum(!is.na(govcgdp_gdd)),
  ) |>
  mutate(
    source = case_when(
      n_pfmh >= n_gdd  ~ "pfmh",
      TRUE            ~ "gdd"
    )
  )

govcgdp_comb <- govcgdp_comb |> 
  left_join(counts |> select(iso3c, source), by = "iso3c")

govcgdp_comb <- govcgdp_comb |> 
  mutate(
    govcgdp = case_when(
      source == "pfmh"    ~ govcgdp_pfmh,
      source == "gdd"    ~ govcgdp_gdd,
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

counts <- infl_combined |> 
  filter(Year >= "1970" & Year <= "2025") |> 
  group_by(iso3c) |> 
  summarise(
    n_bis = sum(!is.na(inflation_bis)),
    n_weo = sum(!is.na(inflation_weo)),
  ) |>
  mutate(
    source = case_when(
      n_bis >= n_weo  ~ "bis",
      TRUE            ~ "weo"
    )
  )

infl_combined <- infl_combined |> 
  left_join(counts |> select(iso3c, source), by = "iso3c")

infl_combined <- infl_combined |> 
  mutate(
    inflation = case_when(
      source == "bis"    ~ inflation_bis,
      source == "weo"    ~ inflation_weo,
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

counts <- bca_comb |> 
  filter(Year >= "1970" & Year <= "2025") |> 
  group_by(iso3c) |> 
  summarise(
    n_wdi = sum(!is.na(bcagdp_wdi)),
    n_weo = sum(!is.na(bca_weo)),
  ) |>
  mutate(
    source = case_when(
      n_weo >= n_wdi  ~ "weo",
      TRUE            ~ "wdi"
    )
  )

bca_comb <- bca_comb |> 
  left_join(counts |> select(iso3c, source), by = "iso3c")

bca_comb <- bca_comb |> 
  mutate(
    bcagdp = case_when(
      source == "weo"    ~ bca_weo,
      source == "wdi"    ~ bcagdp_wdi,
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

counts <- nfa_combined |> 
  filter(Year >= "1970" & Year <= "2025") |> 
  group_by(iso3c) |> 
  summarise(
    n_mfs = sum(!is.na(nfa_mfs)),
    n_wdi = sum(!is.na(nfa_wdi)),
  ) |>
  mutate(
    source = case_when(
      n_mfs >= n_wdi  ~ "mfs",
      TRUE            ~ "wdi"
    )
  )

nfa_combined <- nfa_combined |> 
  left_join(counts |> select(iso3c, source), by = "iso3c")

nfa_combined <- nfa_combined |> 
  mutate(
    nfa = case_when(
      source == "mfs"    ~ nfa_mfs,
      source == "wdi"    ~ nfa_wdi,
    )
  )

# Add to panel

panel <- left_join(panel, nfa_combined |> select(Year, iso3c, nfa), by = c("iso3c", "Year"))

## 2.10 Yield curve ===================================

oecd <- read_xlsx("OECD_STIR_LTIR.xlsx", skip = 3)

# Get rid of useless rows and column
oecd <- oecd |> 
  slice_head(n = -2) |> 
  select(-last_col())

oecd_long <- oecd |>
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "Year",
    values_to = "Value"
  ) |> 
  mutate(Year = as.integer(Year))

# Add country codes
oecd_long <- oecd_long |> 
  mutate(iso3c = countrycode(`Reference area`, origin = "country.name", destination = "iso3c"))

# Split OECD into long term and short term interest rates

oecd_str <- oecd_long |>
  filter(Measure == "Short-term interest rates") |> 
  rename("str_oecd" = Value) |> 
  select(Year, iso3c, str_oecd)

oecd_ltr <- oecd_long |>
  filter(Measure == "Long-term interest rates")


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
    COUNTRY,
    Year = as.integer(Year),
    Value,
    .keep = "none")

mfs_ltr_long <- mfs_ltr_long |> 
  mutate(
    iso3c = countrycode(COUNTRY, origin = "country.name", destination = "iso3c")
  )


# Eurostat Short term rates
str_eurostat <- read_xlsx("str_eurostat.xlsx", sheet = 2, skip = 7, na = c("", "NA", ":"))

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

# Merge OECD, IMF and Eurostat
# Short term rates

str_comb <- oecd_str |> 
  full_join(mfs_str_long, by = c("iso3c", "Year")) |> 
  full_join(str_eurostat_long, by = c("iso3c", "Year")) |> 
  mutate(
    ShortRate = coalesce(str_oecd, str_mfs, str_eurostat),
    Year,
    iso3c,
    .keep = "none"
    )


# Long term rates

ltr_combined <- full_join(
  oecd_ltr,
  mfs_ltr_long,
  by = c("iso3c", "Year"),
  suffix = c("_oecd", "_imf")
) |>
  mutate(
    LongRate = coalesce(Value_oecd, Value_imf)
  ) |>
  select(iso3c, Year, LongRate)

# Combine both rates
interest_rates <- full_join(
  str_comb,
  ltr_combined,
  by = c("iso3c", "Year")
)

# Add variables to panel
panel <- left_join(panel, interest_rates,
                                by = c("iso3c", "Year"))


# IMF PFMH (Real government bond yield)

pfmh_ltr <- read_xls("rltr_IMF_PFMH.xls", na = c("", "no data"))

pfmh_ltr <- pfmh_ltr |> 
  rename("Country" = `Real long term government bond yield, percent`) |> 
  # Remove unnecessary column
  select(-`Estimates start after`) |> 
  # only keep country rows
  slice(3:240)

pfmh_ltr_long <- pfmh_ltr |> 
  pivot_longer(
    cols = !Country,
    names_to = "Year",
    values_to = "rltr"
  ) |> 
  mutate(
    Year = as.integer(Year),
    iso3c = countrycode(Country, origin = "country.name", destination = "iso3c"),
    rltr,
    .keep = "none"
  )

panel <- left_join(panel, pfmh_ltr_long,
                      by = c("iso3c", "Year"))

# Approximate long term interest
panel <- panel |> 
  mutate(ltr_approx = rltr + inflation)

panel <- panel |> 
  mutate(LongRateCompl = coalesce(LongRate, ltr_approx))



# Construct the yield curve
panel <- panel |> 
  mutate(ycurve = LongRateCompl - ShortRate)




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

counts <- bmoney_combined |> 
  filter(Year >= "1970" & Year <= "2025") |> 
  group_by(iso3c) |> 
  summarise(
    n_mfs = sum(!is.na(bmoney_mfs)),
    n_wdi = sum(!is.na(bmoney_wdi)),
    n_jst = sum(!is.na(bmoney_jst))
  ) |>
  mutate(
    source = case_when(
      n_mfs >= n_wdi & n_mfs >= n_jst ~ "mfs",
      n_wdi >= n_jst ~ "wdi",
      TRUE            ~ "jst"
    )
  )

bmoney_combined <- bmoney_combined |> 
  left_join(counts |> select(iso3c, source), by = "iso3c")

bmoney_combined <- bmoney_combined |> 
  mutate(
    bmoney = case_when(
      source == "mfs"    ~ bmoney_mfs,
      source == "wdi"    ~ bmoney_wdi,
      source == "jst"    ~ bmoney_jst
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

counts <- ltd_comb |> 
  filter(Year >= "1970" & Year <= "2025") |> 
  group_by(iso3c) |> 
  summarise(
    n_mfs = sum(!is.na(ltd_mfs)),
    n_gfd = sum(!is.na(ltd_gfd)),
    n_jst = sum(!is.na(ltd_jst))
  ) |>
  mutate(
    source = case_when(
      n_mfs >= n_gfd & n_mfs >= n_jst ~ "mfs",
      n_gfd >= n_jst ~ "gfd",
      TRUE            ~ "jst"
    )
  )

ltd_comb <- ltd_comb |> 
  left_join(counts |> select(iso3c, source), by = "iso3c")

ltd_comb <- ltd_comb |> 
  mutate(
    ltd = case_when(
      source == "mfs"    ~ ltd_mfs,
      source == "gfd"    ~ ltd_gfd,
      source == "jst"    ~ ltd_jst
    )
  )


# Add to panel

panel <- left_join(panel, ltd_comb |> select(iso3c, Year, ltd), by = c("iso3c", "Year"))


## 2.13 Check how many obs ===================================================

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
  select(cgdppriv, rgc, inflation, nfa, bmgrowth, govcgdp, tloanspriv_growth, ltd, bmtr, cgdpcorp, cgdph) |> 
  complete.cases() |> 
  sum()

