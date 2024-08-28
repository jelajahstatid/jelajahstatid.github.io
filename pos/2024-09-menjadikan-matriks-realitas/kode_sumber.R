library(tidyverse)
library(raster)
library(terra)
library(rayshader)

# Data ----
# Data raster
raster_diy <- terra::rast("pos/2024-08-gunung-agung-bromo-merapi-dan-semeru/aset/S08E110.hgt")
pusat_merapi <- c(110.4467, -7.540278)
batas_merapi <- extent(
  110.3967, 110.4967, -7.605278, -7.505278
)
raster_merapi <- terra::crop(raster_diy, batas_merapi)
names(raster_merapi) <- "elevasi_m"
terra::varnames(raster_merapi) <- "elevasi_m"
raster_merapi_simpel <- terra::aggregate(
  raster_merapi, fact = 72, fun = "max"
)

# Data matriks
matriks_merapi <- rayshader::raster_to_matrix(raster_merapi)
matriks_merapi_simpel <- rayshader::raster_to_matrix(raster_merapi_simpel)

# Data tibble
data_merapi <- as.data.frame(
  raster_merapi, xy = TRUE
) |> as_tibble()
data_merapi_simpel <- as.data.frame(
  raster_merapi_simpel, xy = TRUE
) |> as_tibble()

# Visualisasi ide dasar ----
plot_simpel <- data_merapi_simpel |> 
  ggplot(aes(x, y, fill = elevasi_m)) + 
  geom_tile() + 
  scale_fill_viridis_c() + 
  coord_fixed() + 
  theme(
    legend.position = "none",
    plot.margin = margin(
      t = 20, r = 20, b = 20, l = 20, unit = "pt"
    ),
    axis.title = element_blank()
  )
plot_gg(
  plot_simpel,
  scale = 250
)

# Visualisasi 2D dan 3D Gunung Merapi ----
# Visualisasi 2D
matriks_merapi |> 
  sphere_shade() |> 
  add_shadow(ray_shade(matriks_merapi), 0.5) |> 
  add_shadow(ambient_shade(matriks_merapi), 0) |> 
  plot_map()

# Visualisasi 3D (maket)
matriks_merapi |> 
  sphere_shade() |> 
  add_shadow(ray_shade(matriks_merapi), 0.5) |> 
  add_shadow(ambient_shade(matriks_merapi), 0) |> 
  plot_3d(
    heightmap = matriks_merapi,
    zscale = 20,
    solid = TRUE
  )
render_camera(fov = 0, theta = 30, zoom = 0.85, phi = 30)

# Mencetak maket Gunung Merapi
render_snapshot()
save_3dprint(
  filename = "pos/2024-09-menjadikan-matriks-realitas/aset/maket_gunung_merapi.stl",
  maxwidth = 100,
  unit = "mm"
)



