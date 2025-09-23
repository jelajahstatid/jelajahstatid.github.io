library(tidyverse)
library(ggstream)
library(camcorder)
library(ggtext)
library(cowplot)

# Data ----
data("faithful")
faithful <- faithful |> 
  mutate(
    eruptions_std = scale(eruptions),
    waiting_std = scale(waiting)
  )

faithful_1_1 <- faithful |> 
  select(eruptions_std, waiting_std) |> 
  mutate(
    d1 = (eruptions_std - (-1.4))^2 + (waiting_std - 1)^2,
    d2 = (eruptions_std - 1.4)^2 + (waiting_std - (-1))^2,
    d = if_else(d1 <= d2, d1, d2),
    klaster = if_else(d1 <= d2, "Klaster 1", "Klaster 2")
  )

pusat_klaster_1_2 <- faithful_1_1 |> 
  group_by(klaster) |> 
  summarise(
    x = mean(eruptions_std),
    y = mean(waiting_std)
  )
faithful_1_2 <- faithful_1_1 |> 
  select(eruptions_std, waiting_std, klaster) |> 
  mutate(
    d1 = (eruptions_std - pusat_klaster_1_2[[1,2]])^2 + (waiting_std - pusat_klaster_1_2[[1,3]])^2,
    d2 = (eruptions_std - pusat_klaster_1_2[[2,2]])^2 + (waiting_std - pusat_klaster_1_2[[2,3]])^2,
    d = if_else(klaster == "Klaster 1", d1, d2)
  )

faithful_2_1 <- faithful_1_2 |> 
  select(-d, -klaster) |> 
  mutate(
    d = if_else(d1 <= d2, d1, d2),
    klaster = if_else(d1 <= d2, "Klaster 1", "Klaster 2")
  )

pusat_klaster_2_2 <- 
  faithful_2_1 |> 
  group_by(klaster) |> 
  summarise(
    x = mean(eruptions_std),
    y = mean(waiting_std)
  )
faithful_2_2 <- faithful_2_1 |> 
  select(eruptions_std, waiting_std, klaster) |> 
  mutate(
    d1 = (eruptions_std - pusat_klaster_2_2[[1,2]])^2 + (waiting_std - pusat_klaster_2_2[[1,3]])^2,
    d2 = (eruptions_std - pusat_klaster_2_2[[2,2]])^2 + (waiting_std - pusat_klaster_2_2[[2,3]])^2,
    d = if_else(klaster == "Klaster 1", d1, d2)
  )

faithful_3_1 <- faithful_2_2 |> 
  select(-d, -klaster) |> 
  mutate(
    d = if_else(d1 <= d2, d1, d2),
    klaster = if_else(d1 <= d2, "Klaster 1", "Klaster 2")
  )

pusat_klaster_3_2 <- faithful_3_1 |> 
  group_by(klaster) |> 
  summarise(
    x = mean(eruptions_std),
    y = mean(waiting_std)
  )
faithful_3_2 <- faithful_3_1 |> 
  select(eruptions_std, waiting_std, klaster) |> 
  mutate(
    d1 = (eruptions_std - pusat_klaster_3_2[[1,2]])^2 + (waiting_std - pusat_klaster_3_2[[1,3]])^2,
    d2 = (eruptions_std - pusat_klaster_3_2[[2,2]])^2 + (waiting_std - pusat_klaster_3_2[[2,3]])^2,
    d = if_else(klaster == "Klaster 1", d1, d2)
  )

faithful_4_1 <- faithful_3_2 |> 
  select(-d, -klaster) |> 
  mutate(
    d = if_else(d1 <= d2, d1, d2),
    klaster = if_else(d1 <= d2, "Klaster 1", "Klaster 2")
  )

pusat_klaster_4_2 <- faithful_4_1 |> 
  group_by(klaster) |> 
  summarise(
    x = mean(eruptions_std),
    y = mean(waiting_std),
    .groups = "drop"
  )
faithful_4_2 <- faithful_4_1 |> 
  select(eruptions_std, waiting_std, klaster) |> 
  mutate(
    d1 = (eruptions_std - pusat_klaster_4_2[[1,2]])^2 + (waiting_std - pusat_klaster_4_2[[1,3]])^2,
    d2 = (eruptions_std - pusat_klaster_4_2[[2,2]])^2 + (waiting_std - pusat_klaster_4_2[[2,3]])^2,
    d = if_else(klaster == "Klaster 1", d1, d2)
  )

# Rekam ----
gg_record(
  dir = "pos/2025-09-klaterisasi-k-rerata/aset/rekaman/",
  device = "png",
  width = 800,
  height = 600, 
  units = "px",
  dpi = 72,
  bg = "white"
)

# Plot ----
plot_1 <- faithful |> 
  ggplot() + 
  geom_point(
    aes(x = eruptions_std, y = waiting_std),
    color = "#588395"
  ) + 
  geom_point(
    data = data.frame(
      x = c(-1.4, 1.4),
      y = c(1, -1),
      klaster = c("Klaster 1", "Klaster 2")
    ),
    aes(x = x, y = y, color = klaster),
    shape = 4,
    size = 5,
    stroke = 2
  ) + 
  scale_color_manual(
    values = c(
      "Klaster 1" = "#164863",
      "Klaster 2" = "#9BBEC8"
    )
  ) + 
  theme_minimal() + 
  theme(
    axis.title = element_blank(),
    legend.position = "none"
  ) + 
  labs(
    title = "Inisialisasi"
  )

plot_1_1 <- faithful_1_1 |> 
  ggplot() + 
  geom_point(
    aes(
      x = eruptions_std,
      y = waiting_std,
      color = klaster
    )
  ) + 
  geom_point(
    data = tibble(
      x = c(-1.4, 1.4),
      y = c(1, -1),
      klaster = c("Klaster 1", "Klaster 2")
    ),
    aes(x, y),
    shape = 4,
    size = 5,
    stroke = 3,
    color = "white"
  ) + 
  geom_point(
    data = tibble(
      x = c(-1.4, 1.4),
      y = c(1, -1),
      klaster = c("Klaster 1", "Klaster 2")
    ),
    aes(x, y, color = klaster),
    shape = 4,
    size = 5,
    stroke = 1.5
  ) + 
  geom_abline(
    slope = 7/5,
    intercept = 0,
    color = "#588395",
    linewidth = 1
  ) + 
  scale_color_manual(
    values = c(
      "Klaster 1" = "#164863",
      "Klaster 2" = "#9BBEC8"
    )
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title = element_blank()
  ) + 
  labs(
    title = "Iterasi 1-1"
  )

plot_1_2 <- faithful_1_2 |> 
  ggplot() + 
  geom_point(
    aes(
      x = eruptions_std,
      y = waiting_std,
      color = klaster
    )
  ) + 
  geom_point(
    data = pusat_klaster_1_2,
    aes(
      x = x,
      y = y
    ),
    shape = 4,
    size = 5,
    stroke = 3,
    color = "white"
  ) + 
  geom_point(
    data = pusat_klaster_1_2,
    aes(
      x = x,
      y = y,
      color = klaster
    ),
    shape = 4,
    size = 5,
    stroke = 1.5
  ) + 
  scale_color_manual(
    values = c(
      "Klaster 1" = "#164863",
      "Klaster 2" = "#9BBEC8"
    )
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title = element_blank()
  ) + 
  labs(
    title = "Iterasi 1-2"
  )

plot_2_1 <- faithful_2_1 |> 
  ggplot() + 
  geom_point(
    aes(
      x = eruptions_std,
      y = waiting_std,
      color = klaster
    )
  ) + 
  geom_point(
    data = pusat_klaster_1_2,
    aes(
      x = x,
      y = y
    ),
    shape = 4,
    size = 5,
    stroke = 2,
    color = "white"
  ) + 
  geom_point(
    data = pusat_klaster_1_2,
    aes(
      x = x,
      y = y,
      color = klaster
    ),
    shape = 4,
    size = 5,
    stroke = 1.5
  ) + 
  geom_abline(
    slope = -1.624589,
    intercept = -0.058,
    color = "#588395",
    linewidth = 1
  ) + 
  scale_color_manual(
    values = c(
      "Klaster 1" = "#164863",
      "Klaster 2" = "#9BBEC8"
    )
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title = element_blank()
  ) + 
  labs(
    title = "Iterasi 2-1"
  )

plot_2_2 <- faithful_2_2 |> 
  ggplot() + 
  geom_point(
    aes(
      x = eruptions_std,
      y = waiting_std,
      color = klaster
    )
  ) + 
  geom_point(
    data = pusat_klaster_2_2,
    aes(
      x = x,
      y = y
    ),
    shape = 4,
    size = 5,
    stroke = 3,
    color = "white"
  ) + 
  geom_point(
    data = pusat_klaster_2_2,
    aes(
      x = x,
      y = y,
      color = klaster
    ),
    shape = 4,
    size = 5,
    stroke = 1.5,
  ) + 
  scale_color_manual(
    values = c(
      "Klaster 1" = "#164863",
      "Klaster 2" = "#9BBEC8"
    )
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title = element_blank()
  ) + 
  labs(
    title = "Iterasi 2-2"
  )

plot_3_1 <- faithful_3_1 |> 
  ggplot() + 
  geom_point(
    aes(
      x = eruptions_std,
      y = waiting_std,
      color = klaster
    )
  ) + 
  geom_point(
    data = pusat_klaster_2_2,
    aes(
      x = x,
      y = y
    ),
    shape = 4,
    size = 5,
    stroke = 3,
    color = "white"
  ) + 
  geom_point(
    data = pusat_klaster_2_2,
    aes(
      x = x,
      y = y,
      color = klaster
    ),
    shape = 4,
    size = 5,
    stroke = 1.5
  ) + 
  geom_abline(
    slope = -1.0436,
    intercept = -0.486595,
    color = "#588395",
    linewidth = 1
  ) + 
  scale_color_manual(
    values = c(
      "Klaster 1" = "#164863",
      "Klaster 2" = "#9BBEC8"
    )
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title = element_blank()
  ) + 
  labs(
    title = "Iterasi 3-1"
  )

plot_3_2 <- faithful_3_2 |> 
  ggplot() + 
  geom_point(
    aes(
      x = eruptions_std,
      y = waiting_std,
      color = klaster
    )
  ) + 
  geom_point(
    data = pusat_klaster_3_2,
    aes(
      x = x,
      y = y
    ),
    shape = 4,
    size = 5,
    stroke = 3,
    color = "white"
  ) + 
  geom_point(
    data = pusat_klaster_3_2,
    aes(
      x = x,
      y = y,
      color = klaster
    ),
    shape = 4,
    size = 5,
    stroke = 1.5,
  ) + 
  scale_color_manual(
    values = c(
      "Klaster 1" = "#164863",
      "Klaster 2" = "#9BBEC8"
    )
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title = element_blank()
  ) + 
  labs(
    title = "Iterasi 3-2"
  )

plot_4_1 <- faithful_4_1 |> 
  ggplot() + 
  geom_point(
    aes(
      x = eruptions_std,
      y = waiting_std,
      color = klaster
    )
  ) + 
  geom_point(
    data = pusat_klaster_3_2,
    aes(
      x = x,
      y = y
    ),
    shape = 4,
    size = 5,
    stroke = 3,
    color = "white"
  ) + 
  geom_point(
    data = pusat_klaster_3_2,
    aes(
      x = x,
      y = y,
      color = klaster
    ),
    shape = 4,
    size = 5,
    stroke = 1.5
  ) + 
  geom_abline(
    slope = -1.0508,
    intercept = -0.534564,
    color = "#588395",
    linewidth = 1
  ) + 
  scale_color_manual(
    values = c(
      "Klaster 1" = "#164863",
      "Klaster 2" = "#9BBEC8"
    )
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title = element_blank()
  ) + 
  labs(
    title = "Iterasi 4-1"
  )

plot_4_2 <- faithful_4_2 |> 
  ggplot() + 
  geom_point(
    aes(
      x = eruptions_std,
      y = waiting_std,
      color = klaster
    )
  ) + 
  geom_point(
    data = pusat_klaster_4_2,
    aes(
      x = x,
      y = y
    ),
    shape = 4,
    size = 5,
    stroke = 3,
    color = "white"
  ) + 
  geom_point(
    data = pusat_klaster_4_2,
    aes(
      x = x,
      y = y,
      color = klaster
    ),
    shape = 4,
    size = 5,
    stroke = 1.5,
  ) + 
  scale_color_manual(
    values = c(
      "Klaster 1" = "#164863",
      "Klaster 2" = "#9BBEC8"
    )
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title = element_blank()
  ) + 
  labs(
    title = "Iterasi 4-2"
  )

# Plot all ----
plot_grid(
  plot_1, plot_1_1, plot_1_2,
  plot_2_1, plot_2_2, plot_3_1,
  plot_3_2, plot_4_1, plot_4_2,
  ncol = 3
)
