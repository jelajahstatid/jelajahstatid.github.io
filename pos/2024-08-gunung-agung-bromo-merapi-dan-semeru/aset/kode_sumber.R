library(tidyverse)
library(raster)
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
matriks_elevasi <- terra::merge(
  S08E110, S08E112, S08E113,
  S09E112, S09E113, S09E115
)
varnames(matriks_elevasi) <- "elevasi"
names(matriks_elevasi) <- "elevasi"
# Gunung Agung
batas_agung <- extent(
  115.4562, 115.5562,
  -8.393267, -8.293267
)
matriks_agung <- crop(matriks_elevasi, batas_agung)
df_agung <- as.data.frame(matriks_agung, xy = TRUE) |> 
  as_tibble() |> 
  mutate(gunung = "Agung")
# Gunung Bromo
batas_bromo <- extent(
  112.9, 113.0,
  -8.0, -7.9
)
matriks_bromo <- crop(matriks_elevasi, batas_bromo)
df_bromo <- as.data.frame(matriks_bromo, xy = TRUE) |> 
  as_tibble() |> 
  mutate(gunung = "Bromo")
# Gunung Merapi
batas_merapi <- extent(
  110.3967, 110.4967,
  -7.590278, -7.490278
)
matriks_merapi <- crop(matriks_elevasi, batas_merapi)
df_merapi <- as.data.frame(matriks_merapi, xy = TRUE) |> 
  as_tibble() |> 
  mutate(gunung = "Merapi")
# Gunung Semeru
batas_semeru <- extent(
  112.8722, 112.9722,
  -8.157778, -8.057778
)
matriks_semeru <- crop(matriks_elevasi, batas_semeru)
df_semeru <- as.data.frame(matriks_semeru, xy = TRUE) |> 
  as_tibble() |> 
  mutate(gunung = "Semeru")
# Empat gunung
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
# Matriks
matriks_agung <- matriks_agung |> 
  raster_to_matrix()
matriks_bromo <- matriks_bromo |> 
  raster_to_matrix()
matriks_merapi <- matriks_merapi |> 
  raster_to_matrix()
matriks_semeru <- matriks_semeru |> 
  raster_to_matrix()
# Simpan
save(
  df_gunung,
  matriks_agung,
  matriks_bromo,
  matriks_merapi,
  matriks_semeru,
  file = "pos/2024-08-gunung-agung-bromo-merapi-dan-semeru/aset/df_gunung.RData"
)

# Visualisasi data ----
# Tema
tema_gunung <- function(){
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    plot.margin = margin(
      t = 10, r = 10,
      b = 10, l = 10,
      unit = "pt"
    )
  )
}
# Visualisasi 2D
plot_2d_gunung <- df_gunung |> 
  ggplot(
    aes(x_trans, y_trans)
  ) + 
  geom_tile(aes(fill = elevasi)) + 
  geom_contour(
    aes(z = elevasi), 
    breaks = c(seq(500, 3500, by = 250)),
    color = "white",
    linewidth = .25,
    alpha = .3
  ) + 
  scale_fill_gradient(
    name = "Elevasi (m)",
    low = "#6DAA55",
    high = "#205544"
  ) + 
  facet_wrap(vars(gunung)) + 
  coord_fixed() +
  tema_gunung()
print(plot_2d_gunung)
# Visualisasi 3D
plot_gunung <- plot_2d_gunung + 
  theme(
    legend.position = "none"    # Menghapus legenda
  )
plot_gg(
  plot_gunung,
  scale = 150
)
render_camera(
  theta = 45,
  phi = 55,
  zoom = .85
)
render_snapshot()
