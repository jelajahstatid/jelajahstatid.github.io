library(tidyverse)
library(ggstream)
library(ggtext)
library(showtext)
library(camcorder)

# Mengimpor data ----
# Data `realisasi_penanaman_modal` berikut merentang dari 2010-01-01 sampai 2024-06-30
realisasi_penanaman_modal <- read_csv("pos/2024-08-dirgahayu-indonesia/aset/realisasi_penanaman_modal.csv")

# Mengimpor fon ----
font_add_google("Lato", "lato")
showtext_auto()

fon_judul <- "lato"
fon_batang_tubuh <- "lato"

# Menyiapkan data ----
realisasi_penanaman_modal <- realisasi_penanaman_modal |> 
  pivot_longer(
    cols = starts_with("20"),
    names_to = "tahun",
    values_to = "nilai_investasi"
  ) |> 
  mutate(tahun = as.integer(tahun)) |> 
  filter(tahun < 2024)

realisasi_penanaman_modal_benua <- realisasi_penanaman_modal |> 
  group_by(benua, tahun) |> 
  summarise(
    total_investasi = sum(nilai_investasi, na.rm = TRUE),
    .groups = "drop"
  )
realisasi_penanaman_modal_tahun <- realisasi_penanaman_modal |> 
  group_by(tahun) |> 
  summarise(
    total_investasi = sum(nilai_investasi, na.rm = TRUE),
    .groups = "drop"
  )
rerata_kenaikan <- mean(diff(realisasi_penanaman_modal_tahun$total_investasi))

daftar_benua <- realisasi_penanaman_modal_benua |> 
  group_by(benua) |> 
  summarise(
    total_investasi = sum(total_investasi, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  arrange(-total_investasi)
total_asia <- sum(filter(realisasi_penanaman_modal, benua == "Asia")$nilai_investasi, na.rm = TRUE)
total_semua <- sum(realisasi_penanaman_modal$nilai_investasi, na.rm = TRUE)
prop_asia <- total_asia / total_semua * 100

# Menentukan warna dan fon ----
warna_latar <- "white"
warna_teks_judul <- "#343a40"
warna_teks_batang_tubuh <- "#343a40"
warna_benua <- c(
  "Asia" = "#900C3F",
  "Amerika" = "#FFC300",
  "Eropa" = "#C70039",
  "Afrika" = "#FF5733",
  "Australia" = "#ff8d1a",
  "Joint" = "grey85"
)

# Memulai perekaman ----
gg_record(
  dir = "pos/2024-08-dirgahayu-indonesia/aset/rekaman/",
  device = "png",
  width = 1280,
  height = 960,
  units = "px",
  dpi = 72
)

# Menentukan teks ----
judul <- "Realisasi Penanaman Modal Asing"
anak_judul <- "Terdapat tren yang naik untuk total realisasi penanaman modal asing dari berbagai negara dari benua Asia, Amerika, Eropa, Afrika, dan Australia. Mulai tahun 2010 sampai 2023, rerata kenaikannya sekitar 2,6 milyar dolar AS per tahunnya."
anak_judul_2 <- "Terdapat tren yang naik untuk total realisasi penanaman modal asing dari berbagai negara dari benua <span style='color:#900C3F'><b>Asia,</b></span> <span style='color:#FFC300'><b>Amerika,</b></span> <span style='color:#C70039'><b>Eropa,</b></span> <span style='color:#FF5733'><b>Afrika,</b></span> dan <span style='color:#FF8D1a'><b>Australia.</b></span> Mulai tahun 2010 sampai 2023, rerata kenaikannya sekitar 2,6 milyar dolar AS per tahunnya."
takarir <- "Data: Kementerian Investasi/BKPM"
cerita_1 <- "<b>Penanam modal terbesar</b><br>Apabila ditotal, negara-negara dari Asia menanamkan modal terbesar dibandingkan dengan benua-benua lainnya (sekitar 68% dari keseluruhan)."
cerita_2 <- "<b>Kenaikan terbesar</b><br>Kenaikan penanaman modal asing terbesar terjadi di antara tahun 2021 dan 2023."

# Visualisasi data ----
realisasi_penanaman_modal_benua |> 
  ggplot(aes(x = tahun, y = total_investasi)) + 
  geom_segment(
    data = data.frame(tahun = seq(2012, 2022, 2)),
    aes(x = tahun, y = 0, yend = -5e+07),
    linewidth = 1,
    linetype = "dashed",
    alpha = .4,
    colour = warna_teks_batang_tubuh
  ) +
  geom_text(
    data = data.frame(tahun = seq(2012, 2022, 2)),
    mapping = aes(x = tahun, y = -5.5e+07, label = tahun),
    colour = warna_teks_batang_tubuh,
    family = fon_batang_tubuh,
    size = 6
  ) + 
  # Cerita 1
  geom_segment(
    x = 2015.1,
    y = 4.5e+07,
    yend = 0,
    colour = warna_teks_batang_tubuh,
    linewidth = 1.5
  ) +
  geom_textbox(
    x = 2015,
    y = 4.5e+07,
    label = cerita_1,
    colour = warna_teks_batang_tubuh,
    family = fon_batang_tubuh,
    vjust = 0.95,
    size = 6,
    lineheight = 1.5,
    hjust = 1,
    halign = 1,
    box.colour = "transparent",
    fill = "transparent",
    width = .35
  ) + 
  # Cerita 2
  geom_segment(
    x = 2022.6,
    y = 5.5e+07,
    yend = 0,
    colour = warna_teks_batang_tubuh,
    linewidth = 1.5
  ) + 
  geom_textbox(
    x = 2022.5,
    y = 5.5e+07,
    label = cerita_2,
    colour = warna_teks_batang_tubuh,
    family = fon_batang_tubuh,
    vjust = 0.95,
    size = 6,
    lineheight = 1.5,
    hjust = 1,
    halign = 1,
    box.colour = "transparent",
    fill = "transparent",
    width = .3
  ) + 
  geom_stream(
    aes(fill = benua),
    type = "mirror",
    sorting = "onset"
  ) + 
  scale_y_continuous(limits = c(-6e+07, 6e+07)) + 
  scale_fill_manual(
    values = warna_benua
  ) + 
  coord_cartesian(expand = FALSE) + 
  theme_void(base_size = 24, base_family = fon_batang_tubuh) + 
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 0),
    plot.background = element_rect(
      fill = warna_latar, colour = warna_latar
    ),
    panel.background = element_rect(
      fill = warna_latar, colour = warna_latar
    ),
    plot.title = element_textbox_simple(
      colour = warna_teks_judul,
      hjust = 0,
      halign = 0,
      margin = margin(l = 30, b = 5, t = 30),
      lineheight = 1.5,
      family = fon_judul,
      face = "bold",
      size = 40
    ),
    plot.subtitle = element_textbox_simple(
      colour = warna_teks_batang_tubuh,
      hjust = 0,
      halign = 0,
      margin = margin(l = 30, b = 15, t = 5),
      lineheight = 1.5,
      family = fon_batang_tubuh
    ),
    plot.caption = element_textbox_simple(
      colour = warna_teks_batang_tubuh,
      hjust = 0,
      halign = 0,
      margin = margin(l = 30, b = 20, t = 10),
      lineheight = 1.5,
      family = fon_batang_tubuh
    )
  ) + 
  labs(
    title = judul,
    subtitle = anak_judul,
    caption = takarir
  )

realisasi_penanaman_modal_tahun |> 
  ggplot(aes(tahun, total_investasi)) + 
  geom_line(linewidth = 3) + 
  geom_point(size = 10) + 
  geom_smooth(
    method = "lm",
    formula = y ~ x
  ) + 
  theme_minimal(base_size = 28)

gg_stop_recording()

# Sumber ----
# https://nrennie.rbind.io/tidytuesday-shiny-app/
# https://github.com/nrennie/tidytuesday/blob/main/2023/2023-10-31/20231031.R
# https://nrennie.rbind.io/blog/adding-social-media-icons-ggplot2/
# https://bkpm.go.id/id/info/realisasi-investasi


