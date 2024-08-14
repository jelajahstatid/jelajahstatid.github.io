library(tidyverse)
library(camcorder)

# Membuat data ----
set.seed(1000)
x_data <- rnorm(100, mean = 50, sd = 10)
galat_data <- rnorm(100, mean = 0, sd = 15)
y_data <- x_data + galat_data
dat <- tibble(
  x = x_data,
  y = y_data
)

# Memulai perekaman ----
gg_record(
  dir = "aset/rekaman/",
  device = "svg",
  width = 128,
  height = 128,
  units = "px",
  dpi = 72
)

# Visualisasi data ----
dat |> 
  ggplot(aes(x, y)) + 
  geom_point(
    size = 6,
    alpha = .6,
    colour = "#DDF2FD"
  ) + 
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    colour = "white",
    linewidth = 1
  ) + 
  theme_void() + 
  theme(
    panel.background = element_rect(
      fill = "#427D9D",
      colour = "#427D9D"
    ),
    plot.background = element_rect(
      fill = "#427D9D",
      colour = "#427D9D"
    )
  )

# Menghentikan perekaman
gg_stop_recording()





