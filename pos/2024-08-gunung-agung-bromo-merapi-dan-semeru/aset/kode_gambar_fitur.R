data_bromo <- df_gunung |> 
  filter(
    gunung == "Bromo"
  )

plot_bromo <- data_bromo |> 
  ggplot(
    aes(x_trans, y_trans)
  ) + 
  geom_raster(
    aes(fill = elevasi),
    show.legend = FALSE
  ) + 
  geom_contour(
    aes(z = elevasi), 
    breaks = c(seq(500, 3500, by = 250)),
    color = "white",
    linewidth = .25,
    alpha = .3
  ) + 
  scale_fill_gradient(
    name = "Elevasi (m)",
    low = "#9BBEC8",
    high = "#164863"
  ) + 
  coord_fixed() +
  theme_minimal() + 
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

plot_gg(
  plot_bromo,
  multicore = TRUE,
  sunangle = 135,
  width = 5,
  height = 5,
  scale = 400
)

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/air_museum_playground_4k.hdr"

fail_hdri <- basename(u)

render_highquality(
  filename = "gunung_bromo_3d.png",
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

# Bromo ----
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
names(matriks_elevasi) <- "elevasi"
varnames(matriks_elevasi) <- "elevasi"
batas_agung <- extent(115.4562, 115.5562, -8.393267, -8.293267)
batas_bromo <- extent(112.9, 113.0, -8.0, -7.9)
batas_merapi <- extent(110.3967, 110.4967, -7.590278, -7.490278)
batas_semeru <- extent(112.8722, 112.9722, -8.157778, -8.057778)
matriks_agung <- crop(matriks_elevasi, batas_agung) |> 
  raster_to_matrix()
matriks_bromo <- crop(matriks_elevasi, batas_bromo) |> 
  raster_to_matrix()
matriks_merapi <- crop(matriks_elevasi, batas_merapi) |> 
  raster_to_matrix()
matriks_semeru <- crop(matriks_elevasi, batas_semeru) |> 
  raster_to_matrix()
save(
  df_gunung,
  matriks_agung,
  matriks_bromo,
  matriks_merapi,
  matriks_semeru,
  file = "pos/2024-08-gunung-agung-bromo-merapi-dan-semeru/aset/df_gunung.RData"
)

# Gunung Agung
matriks_agung |> 
  sphere_shade(
    texture = "desert"
  ) |> 
  add_shadow(ray_shade(matriks_agung), 0.5) |>
  add_shadow(ambient_shade(matriks_agung), 0) |> 
  plot_3d(
    matriks_agung,
    fov = 0,
    zscale = 30,
    theta = 135,
    zoom = 0.75,
    phi = 45,
    windowsize = c(800, 600),
    background = "#164863"
  )

render_camera(theta = 30, phi = 45, zoom = 0.75)
render_clouds(
  matriks_agung,
  zscale = 30,
  start_altitude = 2000,
  end_altitude = 2300,
  attenuation_coef = 1,
  fractal_levels = 32,
  cloud_cover = .5,
  offset_x = -100,
  offset_y = -100,
  clear_clouds = TRUE
)
render_snapshot(
  filename = "pos/2024-08-gunung-agung-bromo-merapi-dan-semeru/aset/gunung_agung.png",
  clear = TRUE
)

# Gunung Bromo
matriks_bromo |> 
  sphere_shade(
    texture = "desert"
  ) |> 
  add_shadow(ray_shade(matriks_bromo), 0.5) |>
  add_shadow(ambient_shade(matriks_bromo), 0) |> 
  plot_3d(
    matriks_bromo,
    fov = 0,
    zscale = 30,
    theta = 135,
    zoom = 0.75,
    phi = 45,
    windowsize = c(800, 600),
    background = "#164863"
  )

render_camera(theta = 30, phi = 45, zoom = 0.75)
render_clouds(
  matriks_bromo,
  zscale = 30,
  start_altitude = 2000,
  end_altitude = 2300,
  attenuation_coef = 1,
  fractal_levels = 32,
  cloud_cover = .5,
  clear_clouds = TRUE
)
render_snapshot(
  filename = "pos/2024-08-gunung-agung-bromo-merapi-dan-semeru/aset/gunung_bromo.png",
  clear = TRUE
)

# Gunung Merapi
matriks_merapi |> 
  sphere_shade(
    texture = "desert"
  ) |> 
  add_shadow(ray_shade(matriks_merapi), 0.5) |>
  add_shadow(ambient_shade(matriks_merapi), 0) |> 
  plot_3d(
    matriks_merapi,
    fov = 0,
    zscale = 30,
    theta = 135,
    zoom = 0.75,
    phi = 45,
    windowsize = c(800, 600),
    background = "#164863"
  )

render_camera(theta = 30, phi = 45, zoom = 0.75)
render_clouds(
  matriks_merapi,
  zscale = 30,
  start_altitude = 2000,
  end_altitude = 2300,
  attenuation_coef = 1,
  fractal_levels = 32,
  cloud_cover = .4,
  offset_x = -50,
  offset_y = -175,
  clear_clouds = TRUE
)
render_snapshot(
  filename = "pos/2024-08-gunung-agung-bromo-merapi-dan-semeru/aset/gunung_merapi.png",
  clear = TRUE
)

# Gunung Merapi
matriks_semeru |> 
  sphere_shade(
    texture = "desert"
  ) |> 
  add_shadow(ray_shade(matriks_semeru), 0.5) |>
  add_shadow(ambient_shade(matriks_semeru), 0) |> 
  plot_3d(
    matriks_semeru,
    fov = 0,
    zscale = 30,
    theta = 135,
    zoom = 0.75,
    phi = 45,
    windowsize = c(800, 600),
    background = "#164863"
  )

render_camera(theta = 30, phi = 45, zoom = 0.75)
render_clouds(
  matriks_semeru,
  zscale = 30,
  start_altitude = 2000,
  end_altitude = 2300,
  attenuation_coef = 1,
  fractal_levels = 32,
  cloud_cover = .55,
  offset_x = -50,
  offset_y = -100,
  clear_clouds = TRUE
)
render_snapshot(
  filename = "pos/2024-08-gunung-agung-bromo-merapi-dan-semeru/aset/gunung_semeru.png",
  clear = TRUE
)

