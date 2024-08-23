library(pRecipe)
library(giscoR)
library(terra)
library(tidyverse)
library(rayshader)
library(sf)
library(classInt)

# Data negara ----
sf_negara <- giscoR::gisco_get_countries(
  country = "ID",
  resolution = 1
)

# Data curah hujan ----
pRecipe::download_data(
  dataset = "mswep",
  path = getwd(),
  domain = "raw",
  timestep = "monthly"
)

data_mswep <- terra::rast(
  "mswep_tp_mm_global_197902_202301_025_monthly.nc"
) |> 
  terra::crop(
    sf_negara
  )

barisan_tanggal <- as.character(
  seq(as.Date("1979-02-01"), as.Date("2023-01-01"), by = "month")
)

names(data_mswep) <- barisan_tanggal

df_mswep <- data_mswep |>
  as.data.frame(xy = TRUE) |> 
  as_tibble()

df_mswep <- df_mswep |> 
  pivot_longer(
    !c("x", "y"),
    names_to = "tanggal_bawah",
    values_to = "curah_hujan"
  ) |> 
  mutate(
    tanggal_bawah = as.Date(tanggal_bawah)
  )

df_mswep_tahunan <- df_mswep |> 
  filter(
    tanggal_bawah >= as.Date("1980-01-01"),
    tanggal_bawah <= as.Date("2022-12-01")
  ) |> 
  mutate(tahun = year(tanggal_bawah)) |> 
  group_by(x, y, tahun) |> 
  summarise(
    curah_hujan_tahunan = sum(curah_hujan, na.rm = TRUE),
    .groups = "drop"
  )

df_mswep_19_22 <- df_mswep_tahunan |> 
  filter(
    tahun %in% c(2019, 2020, 2021, 2022)
  )


# Tema, breaks, dan warna ----

tema_peta <- function(){
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "bottom",
      panel.grid.major = element_line(
        color = NA
      ),
      panel.grid.minor = element_line(
        color = NA
      ),
      plot.background = element_rect(
        fill = NA, color = NA
      ),
      legend.background = element_rect(
        fill = "white", color = NA
      ),
      panel.border = element_rect(
        fill = NA, color = NA
      ),
      plot.margin = unit(
        c(
          t = 0, r = 0,
          b = 0, l = 0
        ), "lines"
      )
    )
}

warna <- c("#DDF2FD", "#9BBEC8", "#427D9D")

# Curah hujan bulanan (mm per bulan)
batas_int_bulanan <- c(0, 200, 400, ceiling(max(df_mswep$curah_hujan)))

batas_int_bulanan_baku <- (batas_int_bulanan - min(batas_int_bulanan)) / (max(batas_int_bulanan) - min(batas_int_bulanan))

# Peta dua dimensi ----

peta_2023_01 <- df_mswep |> 
  filter(
    tanggal_bawah == as.Date("2023-01-01")
  ) |> 
  ggplot() + 
  geom_raster(
    aes(
      x = x,
      y = y,
      fill = curah_hujan
    )
  ) +
  geom_contour(
    aes(
      x = x,
      y = y,
      z = curah_hujan 
    ),
    breaks = c(0, 200, 400),
    color = "white",
    linewidth = .25
  ) +
  geom_sf(
    data = sf_negara,
    fill = "transparent",
    color = "grey10",
    linewidth = .5
  ) + 
  scale_fill_gradientn(
    name = "Curah hujan\n(mm/bulan)",
    colors = warna,
    values = batas_int_bulanan_baku,
    breaks = c(0, 200, 400),
    labels = c(0, 200, 400)
  ) + 
  tema_peta()

# Peta 3D
rayshader::plot_gg(
  ggobj = peta_2023_01,
  width = 7,
  height = 7,
  scale = 250,
  solid = FALSE,
  shadow = TRUE,
  shadowcolor = "white",
  shadowwidth = 0,
  shadow_intensity = 1,
  zoom = .7,
  phi = 85,
  theta = 0
)

rayshader::render_camera(
  phi = 75,
  theta = 30
)

# Render ----

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/air_museum_playground_4k.hdr"

fail_hdri <- basename(u)

download.file(
  url = u,
  destfile = fail_hdri,
  mode = "wb"
)

rayshader::render_highquality(
  filename = "curah_hujan_indonesia_fitur.png",
  preview = TRUE,
  interactive = FALSE,
  parallel = TRUE,
  light = TRUE,
  environment_light = fail_hdri,
  intensity = .45,
  rotate_env = 90,
  width = 800,
  height = 600
)




