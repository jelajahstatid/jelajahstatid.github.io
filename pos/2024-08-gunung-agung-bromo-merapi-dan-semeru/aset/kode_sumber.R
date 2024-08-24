library(tidyverse)
library(terra)
library(rayshader)

# Mengimpor data ----
# Sumber data ketinggian: https://dwtkns.com/srtm30m/
S08E110 <- terra::rast("pos/2024-08-gunung-agung-bromo-merapi-dan-semeru/aset/S08E110.hgt")
S08E112 <- terra::rast("pos/2024-08-gunung-agung-bromo-merapi-dan-semeru/aset/S08E112.hgt")
S08E113 <- terra::rast("pos/2024-08-gunung-agung-bromo-merapi-dan-semeru/aset/S08E113.hgt")
S09E112 <- terra::rast("pos/2024-08-gunung-agung-bromo-merapi-dan-semeru/aset/S09E112.hgt")
S09E113 <- terra::rast("pos/2024-08-gunung-agung-bromo-merapi-dan-semeru/aset/S09E113.hgt")
S09E115 <- terra::rast("pos/2024-08-gunung-agung-bromo-merapi-dan-semeru/aset/S09E115.hgt")


matriks_elevasi <- terra::merge(S08E110, S08E112, S08E113,
                        S09E112, S09E113, S09E115)
df_elevasi <- terra::as.data.frame(matriks_elevasi, xy = TRUE) |> 
  as_tibble()
df_elevasi <- df_elevasi |> 
  rename(elevasi = S08E110)

## Merapi ----
df_merapi <- df_elevasi |> 
  filter(
    x >= 110.3967,
    x <= 110.4967,
    y >= -7.590278,
    y <= -7.490278
  )
df_merapi <- df_merapi |> 
  mutate(gunung = "Merapi")

df_merapi_top <- df_merapi |> 
  slice_max(
    order_by = elevasi,
    n = 10
  )

df_merapi |> 
  ggplot(aes(x, y)) + 
  geom_raster(aes(fill = elevasi)) + 
  coord_fixed()

## Semeru ----
df_semeru <- df_elevasi |> 
  filter(
    x >= 112.8722,
    x <= 112.9722,
    y >= -8.157778,
    y <= -8.057778
  )
df_semeru <- df_semeru |> 
  mutate(
    gunung = "Semeru"
  )

df_semeru_top <- df_semeru |> 
  slice_max(
    order_by = elevasi,
    n = 10
  )

df_semeru |> 
  ggplot(aes(x, y)) + 
  geom_raster(aes(fill = elevasi)) + 
  coord_fixed()

## Bromo ----
df_bromo <- df_elevasi |> 
  filter(
    x >= 112.9,
    x <= 113,
    y >= -8,
    y <= -7.9
  )
df_bromo <- df_bromo |> 
  mutate(
    gunung = "Bromo"
  )

df_bromo_top <- df_bromo |> 
  slice_max(
    order_by = elevasi,
    n = 10
  )

df_bromo |> 
  ggplot(aes(x, y)) + 
  geom_raster(aes(fill = elevasi)) + 
  coord_fixed()

## Agung ----
df_agung <- df_elevasi |> 
  filter(
    x >= 115.4562,
    x <= 115.5562,
    y >= -8.393267,
    y <= -8.293267
  )
df_agung <- df_agung |> 
  mutate(
    gunung = "Agung"
  )

df_agung_top <- df_agung |> 
  slice_max(
    order_by = elevasi,
    n = 10
  )

df_agung |> 
  ggplot(aes(x, y)) + 
  geom_raster(aes(fill = elevasi)) + 
  coord_fixed()

## Empat gunung ----
df_gunung <- bind_rows(
  df_agung, df_bromo, df_merapi, df_semeru
)

df_gunung <- df_gunung |> 
  group_by(gunung) |> 
  mutate(
    x_trans = x - min(x),
    y_trans = y - min(y)
  ) |> 
  ungroup()
df_gunung |> 
  filter(gunung == "Bromo") |> 
  slice_max(
    order_by = y_trans,
    n = 5
  )

df_gunung |> 
  group_by(gunung) |> 
  slice_sample(n = 5) |> 
  ungroup() |> 
  as.data.frame()

df_gunung |> 
  group_by(gunung) |> 
  summarise(
    x_min = min(x),
    y_min = min(y),
    .groups = "drop"
  ) |> 
  as.data.frame()
df_gunung_trans |> 
  ggplot(
    aes(x_trans, y_trans)
  ) + 
  geom_raster(aes(fill = elevasi)) + 
  geom_contour(
    aes(z = elevasi), 
    breaks = c(seq(500, 3000, by = 500)),
    color = "white",
    linewidth = .25,
    alpha = .3
  ) + 
  scale_fill_viridis_c() + 
  facet_wrap(vars(gunung)) + 
  coord_fixed() +
  theme_minimal() + 
  theme(
    axis.title = element_blank(),
    axis.text = element_blank()
  )

