library(tidyverse)
library(ggstream)
library(camcorder)
library(ggtext)

# Data ----
html <- read_html("https://id.wikipedia.org/w/index.php?title=Indonesia_pada_Olimpiade&oldid=26147360")

daftar_tabel <- html |> 
  html_elements(".wikitable") |> 
  html_table()

data_dsr_olimpiade <- daftar_tabel[[2]]

data_dsr_olimpiade <- data_dsr_olimpiade |> 
  slice_head(n = 19)

data_dsr_olimpiade <- data_dsr_olimpiade |> 
  select(Olimpiade, `01  Emas`, `02  Perak`, `03  Perunggu`, Peringkat) |> 
  pivot_longer(
    cols = contains("0"),
    names_to = "medali",
    values_to = "banyak"
  ) |> 
  rename(
    olimpiade = Olimpiade,
    peringkat = Peringkat
  ) |> 
  mutate(
    tahun = as.integer(str_extract(olimpiade, "\\d{4}")),
    peringkat = ifelse(
      peringkat == "" | peringkat == "Tidak ikut",
      NA, as.integer(peringkat)
    ),
    medali = substr(medali, 5, nchar(medali)),
    banyak = ifelse(
      banyak == "Tidak ikut",
      NA, as.integer(banyak)
    )
  ) |> 
  select(
    tahun, olimpiade, medali, banyak, peringkat
  )

# Rekam ----
gg_record(
  dir = "pos/2024-08-indonesia-di-olimpiade/aset/rekaman/",
  device = "png",
  width = 800,
  height = 600, 
  units = "px",
  dpi = 72,
  bg = "white"
)

# Visualisasi data ----
data_dsr_olimpiade |> 
  ggplot(aes(x = tahun, y = banyak, fill = medali)) + 
  geom_vline(
    aes(xintercept = tahun),
    color = "grey",
    linewidth = .5
  ) + 
  geom_stream() + 
  geom_text(
    aes(label = olimpiade),
    y = -4.9,
    size = 6,
    angle = 90,
    hjust = 0,
    nudge_x = -1.2,
    color = "grey60"
  ) + 
  scale_fill_manual(
    name = "Medali",
    values = c(
      "Emas" = "#164863",
      "Perak" = "#427D9D",
      "Perunggu" = "#9BBEC8"
    )
  ) + 
  scale_x_continuous(
    limits = c(1948, 2026),
    expand = c(0, 0)
  ) + 
  scale_y_continuous(
    limits = c(-5, 3),
    expand = c(0, 0)
  ) + 
  theme_void(base_size = 24) + 
  theme(
    legend.position = "none",
    plot.title = element_textbox_simple(
      margin = margin(t = 20, r = 20, b = 5, l = 20)
    ),
    plot.subtitle = element_textbox_simple(
      margin = margin(t = 5, r = 20, b = 10, l = 20)
    ),
    plot.caption = element_text(
      margin = margin(t = 10, r = 20, b = 10, l = 40)
    )
  ) + 
  labs(
    title = "**Medali Indonesia di Olimpiade**",
    subtitle = "Perolehan medali <b><span style='color:#164863'>emas</span></b>, <b><span style='color:#427D9D'>perak</span></b>, dan <b><span style='color:#9BBEC8'>perunggu</span></b> Indonesia pada olimpiade 1952 -- 2024",
    caption = "Data: Wikipedia"
  )

# Stop perekaman ----
gg_stop_recording()
