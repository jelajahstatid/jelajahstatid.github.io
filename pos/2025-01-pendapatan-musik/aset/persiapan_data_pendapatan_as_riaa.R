library(tidyverse)

# Impor data ----
riaa_infl <- read_csv(
  "pos/2025-01-pendapatan-musik/aset/pendapatan_as_riaa.csv",
  show_col_types = FALSE
) |> 
  rename(
    pendapatan_infl = pendapatan
  )
riaa <- read_csv(
  "pos/2025-01-pendapatan-musik/aset/riaa_us_revenue_data_not_adjusted.csv",
  show_col_types = FALSE
) |> 
  select(Format, Year, `Value (Actual)`) |> 
  rename(
    format = Format,
    tahun = Year,
    pendapatan = `Value (Actual)`
  ) |> 
  mutate(
    tahun = as.Date(paste0(tahun, "-01-01"))
  )
# Menggabungkan data ----
pendapatan_as_riaa <- left_join(
  riaa, riaa_infl, by = c("format", "tahun")
)
# Mengekspor data ----
write.csv(
  x = pendapatan_as_riaa,
  file = "pos/2025-01-pendapatan-musik/aset/pendapatan_as_riaa.csv",
  row.names = FALSE
)


