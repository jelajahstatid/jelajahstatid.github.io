library(tidyverse)

set.seed(as.numeric(as.Date("2024-08-10")))

x = round(rnorm(1000, 50, 10), 2)
y = round(rnorm(1000, 50, 10), 1)
size = c(round(rnorm(900, 50, 10), 0), round(rnorm(100, 500, 10), 0))
col = round(rnorm(1000, 50, 10), 0)
df <- data.frame(
  x = x, y = y, size = size, col = col
)
df |> 
  ggplot(aes(x, y, size = size, col = col)) + 
  geom_point(
    alpha = .8,
    show.legend = F
  ) + 
  ylim(0, 100) + 
  scale_color_gradient(
    low = "#DDF2FD",
    high = "#427D9D"
  ) + 
  scale_size(
    range = c(1, 20)
  ) + 
  theme_void()






