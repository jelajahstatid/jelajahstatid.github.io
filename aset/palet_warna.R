library(tidyverse)

palet_warna <- c(
  "warna_1" = "#164863",
  "warna_2" = "#427D9D",
  "warna_3" = "#9BBEC8",
  "warna_4" = "#DDF2FD"
)

data <- tibble(
  warna = paste0("warna_", 1:4),
  hex = c("#164863", "#427D9D", "#9BBEC8", "#DDF2FD"),
  x = 1,
  y = 1:4
)

data |> 
  ggplot(aes(fill = warna)) + 
  geom_rect(
    aes(
      xmin = 0, xmax = x,
      ymin = y - .5, ymax = y + .5
    ),
    show.legend = FALSE
  ) + 
  geom_label(
    aes(x = x / 2, y = y, label = hex),
    show.legend = FALSE,
    fill = "white"
  ) + 
  scale_fill_manual(
    values = palet_warna
  ) + 
  theme_void()


