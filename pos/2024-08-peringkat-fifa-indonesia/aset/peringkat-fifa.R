library(tidyverse)
library(scales)
library(gganimate)
library(ggtext)
library(ggimage)

# Impor data ----
data_dunia <- read_csv(
  "https://raw.githubusercontent.com/hericlibong/Fifa-Api-Ranking-Scraper/main/FifaMenRanking/FifaMenRanking/spiders/data.csv",
  col_types = cols(date = col_date(format = "%Y-%m-%d"))
)

# Mempersiapkan data ----
data_aff <- data_dunia |> 
  filter(
    (country == "Australia" & date >= as.Date("2013-01-01")) | 
      (country == "Brunei Darussalam" & date >= as.Date("1984-01-01")) | 
      (country == "Cambodia" & date >= as.Date("1996-01-01")) | 
      (country == "Timor-Leste" & date >= as.Date("2004-01-01")) | 
      (country == "Indonesia" & date >= as.Date("1984-01-01")) | 
      (country == "Laos" & date >= as.Date("1996-01-01")) | 
      (country == "Malaysia" & date >= as.Date("1984-01-01")) | 
      (country == "Myanmar" & date >= as.Date("1996-01-01")) | 
      (country == "Philippines" & date >= as.Date("1984-01-01")) | 
      (country == "Singapore" & date >= as.Date("1984-01-01")) | 
      (country == "Thailand" & date >= as.Date("1984-01-01")) | 
      (country == "Vietnam" & date >= as.Date("1996-01-01"))
  ) |> 
  arrange(desc(date), -total_points)
data_id <- data_aff |> 
  group_by(date) |> 
  mutate(
    IDN = ifelse(
      country == "Indonesia", TRUE, FALSE
    ),
    rank_aff = 1:n(),
    tahun = year(date),
    poin = ifelse(
      total_points >= 250,
      as.character(total_points), ""
    )
  ) |> 
  select(date, tahun, country, IDN, rank_aff, total_points, poin)

pemetaan_bendera <- tribble(
  ~country, ~negara, ~bendera,
  "Australia", "Australia", "pos/2024-08-peringkat-fifa-indonesia/aset/AUS.png",
  "Thailand", "Thailand", "pos/2024-08-peringkat-fifa-indonesia/aset/THA.png",
  "Vietnam", "Vietnam", "pos/2024-08-peringkat-fifa-indonesia/aset/VIE.png",
  "Indonesia", "Indonesia", "pos/2024-08-peringkat-fifa-indonesia/aset/IDN.png",
  "Malaysia", "Malaysia", "pos/2024-08-peringkat-fifa-indonesia/aset/MAS.png",
  "Philippines", "Filipina", "pos/2024-08-peringkat-fifa-indonesia/aset/PHI.png",
  "Singapore", "Singapura", "pos/2024-08-peringkat-fifa-indonesia/aset/SGP.png",
  "Myanmar", "Myanmar", "pos/2024-08-peringkat-fifa-indonesia/aset/MYA.png",
  "Cambodia", "Kamboja", "pos/2024-08-peringkat-fifa-indonesia/aset/CAM.png",
  "Laos", "Laos", "pos/2024-08-peringkat-fifa-indonesia/aset/LAO.png",
  "Brunei Darussalam", "Brunei Darussalam", "pos/2024-08-peringkat-fifa-indonesia/aset/BRU.png",
  "Timor-Leste", "Timor-Leste", "pos/2024-08-peringkat-fifa-indonesia/aset/TLS.png"
)

data_id <- data_id |> 
  left_join(pemetaan_bendera, by = join_by(country))

# Plot statis ----
peringkat_aff <- data_id |> 
  ggplot() + 
  geom_rect(
    aes(
      xmin = 0, xmax = total_points,
      ymin = rank_aff - .45, ymax = rank_aff + .45,
      fill = IDN
    )
  ) + 
  geom_image(
    aes(x = total_points + 150, y = rank_aff, image = bendera),
    size = .1
  ) + 
  geom_text(
    aes(x = total_points - 20, y = rank_aff, label = poin),
    color = "white",
    hjust = "right",
    size = 7
  ) + 
  geom_text(
    col = "grey10",
    hjust = "right",
    aes(label = negara, y = rank_aff),
    x = -50,
    size = 8
  ) + 
  geom_text(
    aes(label = as.character(tahun)),
    x = 1800,
    y = -12,
    size = 18,
    col = "grey20",
    hjust = "right"
  ) + 
  scale_x_continuous(
    limits = c(-600, 1800),
    breaks = c(0, 500, 1000, 1500)
  ) + 
  scale_y_reverse() + 
  scale_fill_manual(
    values = c(
      "TRUE" = "#EC1C24",
      "FALSE" = "grey30"
    )
  ) + 
  theme_classic(base_size = 18) + 
  theme(
    plot.title = element_text(face = "bold", size = 28),
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_blank(),
    legend.background = element_rect(fill = "#F2ECE9"),
    plot.background = element_rect(fill = "#F2ECE9"),
    panel.background = element_rect(fill = "#F2ECE9")
  ) + 
  labs(
    title = "Peringkat FIFA Negara-Negara AFF",
    x = "Total Poin",
    caption = "Data: FIFA.com"
  ) + 
  gganimate::transition_time(date)

animate(
  plot = peringkat_aff,
  fps = 10,
  duration = 30,
  end_pause = 20,
  width = 800,
  height = 600,
  units = "px",
  bg = "#F2ECE9",
  renderer = gifski_renderer()
)

anim_save(
  filename = "peringkat-fifa-aff.gif",
  animation = last_animation()
)
