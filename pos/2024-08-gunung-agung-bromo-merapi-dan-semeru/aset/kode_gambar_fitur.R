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
