library(tidyverse)
library(camcorder)
library(ggtext)

# Data ----
dat_jarum_buffon <- tibble(
  x1 = runif(1000, min = .5, max = 15.5),
  y1 = runif(1000, min = .5, max = 8.5),
  sudut = runif(1000, min = 0, max = 2 * pi)
) |> 
  mutate(
    x2 = x1 + cos(sudut),
    y2 = y1 + sin(sudut),
    xt = (x1 + x2) / 2,
    yt = (y1 + y2) / 2,
    d = if_else(
      yt - floor(yt) <= ceiling(yt) - yt, yt - floor(yt), ceiling(yt) - yt
    ),
    s_lancip = if_else(
      sudut >= 0 & sudut <= pi / 2, sudut,
      if_else(
        sudut > pi / 2 & sudut <= pi, pi - sudut,
        if_else(
          sudut > pi & sudut <= 3 * pi / 2, sudut - pi,
          2 * pi - sudut
        )
      )
    ),
    lewat = if_else(
      1 / 2 * sin(s_lancip) >= d, 1, 0
    ),
    id = row_number()
  )

# Rekam ----
gg_record(
  dir = "pos/2025-10-simulasi-monte-carlo/aset/rekaman/",
  device = "png",
  width = 800,
  height = 600, 
  units = "px",
  dpi = 72,
  bg = "white"
)

# Visualisasi masalah ----
dat_jarum_buffon |> 
  mutate(
    lewat = if_else(
      lewat == 1, "Ya", "Tidak"
    )
  ) |> 
  ggplot() + 
  geom_hline(
    color = "gray80",
    yintercept = 0:10,
    linewidth = .5
  ) + 
  geom_segment(
    aes(
      x = x1, y = y1,
      xend = x2, yend = y2, colour = lewat
    ),
    linewidth = 1.5
  ) + 
  coord_equal(
    xlim = c(0, 16),
    ylim = c(0, 9)
  ) + 
  scale_color_manual(
    name = "Melintasi garis?",
    values = c(
      "Ya" = "#164863",
      "Tidak" = "#9BBEC8"
    )
  ) + 
  theme_minimal(base_size = 24) + 
  theme(
    legend.position = "none",
    plot.title = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10, r = 5, b = 5, l = 5),
    ),
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 5, r = 5, b = 10, l = 5)
    ),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) + 
  labs(
    title = "Masalah Jarum Buffon",
    subtitle = "Sebuah jarum dilempar pada selembar kertas bergaris. Berapa peluang jarum tersebut melintasi garis?"
  )

record_polaroid()
