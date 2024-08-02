library(tidyverse)
library(gganimate)
library(ggtext)

# Menyiapkan data ----
load(url("https://github.com/jennybc/gapminder/raw/main/data/gapminder.rdata"))
terjemahan_benua <- c(
  "Asia" = "Asia",
  "Europe" = "Eropa",
  "Africa" = "Afrika",
  "Americas" = "Amerika",
  "Oceania" = "Oseania"
)
gapminder <- gapminder |> 
  mutate(
    continent = recode(continent, !!!terjemahan_benua)
  )

# Membuat objek animasi ----
anim <- gapminder |> 
  ggplot(
    aes(x = gdpPercap, y = lifeExp)
  ) + 
  geom_point(
    aes(color = continent, size = pop),
    alpha = .5
  ) + 
  scale_size(
    range = c(5, 50),
    guide = "none"
  ) + 
  scale_color_manual(
    name = "Benua",
    values = c(
      "Afrika" = "#164863",
      "Asia" = "#427D9D",
      "Eropa" = "#427D9D",
      "Amerika" = "#9BBEC8",
      "Oseania" = "#9BBEC8"
    )
  ) + 
  theme_minimal(base_size = 18) + 
  theme(
    legend.position = "bottom",
    plot.title = element_textbox_simple(size = 32)
  ) +
  
  # Penggunaan {gganimate} mulai dari sini
  labs(
    title = "<b>Tahun: {frame_time}</b>",
    x = "PDB per kapita",
    y = "Angka harapan hidup",
    caption = "Data: Jenny Bryan dkk. / Github"
  ) + 
  transition_time(year) + 
  ease_aes("linear")

# Merender dan menyimpan animasi ----
animate(
  plot = anim,
  width = 800,
  height = 600,
  units = "px",
  bg = "white",
  fps = 10,
  duration = 5,
  renderer = gifski_renderer()
)

anim_save(
  filename = "pos/2024-08-animasi-diagram-statistik/animasi.gif"
)
