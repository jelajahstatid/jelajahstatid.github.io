library(tidyverse)
library(ggsci)
library(gganimate)

# Data ----
dat_jarum_buffon <- tibble(
  x1 = runif(1000, min = 1, max = 9),
  y1 = runif(1000, min = 1, max = 9),
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

warna_npg <- c("#4269d0","#efb118","#ff725c","#6cc5b0","#3ca951","#ff8ab7","#a463f2","#97bbf5","#9c6b4e","#9498a0")
dat_jarum_buffon <- dat_jarum_buffon |> 
  mutate(
    warna = warna_npg[(id %% 10) + 1]
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
      xend = x2, yend = y2, colour = as_factor(lewat)
    ),
    color = dat_jarum_buffon$warna,
    linewidth = 1
  ) + 
  scale_color_npg(
    name = "Lewat?"
  ) + 
  coord_equal(
    xlim = c(0, 10),
    ylim = c(0, 10)
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.margin = unit(c(.5, .5, .5, .5), "cm")
  )

# Animasi jarum ----
plot_buffon <- ggplot(dat_jarum_buffon) +
  # kertas bergaris
  geom_hline(
    yintercept = seq(0, 10, by = 1),
    color = "gray80",
    linewidth = 0.5
  ) +
  # jarum
  geom_segment(
    aes(
      x = x1, y = y1,
      xend = x2, yend = y2,
      group = id
    ),
    color = dat_jarum_buffon$warna,
    linewidth = 1.2, lineend = "round"
  ) +
  coord_equal(xlim = c(0, 10), ylim = c(0, 10)) +
  theme_void() + 
  theme(
    legend.position = "none"
  ) + 
  # animasi
  transition_reveal(along = id)

# Menyimpan animasi ----
anim <- animate(
  plot_buffon,
  nframes = 300,
  fps = 5,
  width = 480,
  height = 480
)
anim_save("simulasi_buffon.gif", animation = anim)
