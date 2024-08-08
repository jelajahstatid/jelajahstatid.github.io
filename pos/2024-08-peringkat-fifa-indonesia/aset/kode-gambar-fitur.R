library(tidyverse)
library(scales)
library(gganimate)
library(ggtext)
library(ggimage)
library(camcorder)

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
  arrange(desc(date), -total_points) |> 
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

data_aff <- data_aff |> 
  left_join(pemetaan_bendera, by = join_by(country)) |> 
  mutate(
    bendera = paste0(
      "<img src='",
      bendera,
      "' height=30 />"
    ),
    label = paste0("<b>", negara, "</b>, <span style='font-size:13pt'>", total_points, "</span>")
  )

# Mulai perekaman ----
gg_record(
  dir = "pos/2024-08-peringkat-fifa-indonesia/aset/rekaman/",
  device = "png",
  width = 800,
  height = 600,
  units = "px",
  dpi = 72
)

# Plot statis ----
data_aff |> 
  filter(date == as.Date("2024-07-18")) |> 
  mutate(
    bendera = fct_reorder(bendera, total_points)
  ) |> 
  ggplot(
    aes(x = total_points, y = bendera)
  ) + 
  geom_col(
    aes(fill = IDN),
    just = .9,
    width = .5
  ) + 
  geom_textbox(
    aes(y = bendera, label = label),
    x = 10,
    hjust = 0,
    nudge_y = .25,
    size = 6,
    bg = "transparent",
    box.colour = "transparent",
    width = .5
  ) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_discrete() + 
  scale_fill_manual(
    values = c(
      "TRUE" = "#164863",
      "FALSE" = "#9BBEC8"
    )
  ) + 
  theme_minimal(base_size = 24) + 
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(
      linetype = "dashed",
      colour = "grey",
      linewidth = .5
    ),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line.y = element_line(),
    plot.title = element_textbox_simple(
      margin = margin(l = -58, b = 5)
    ),
    plot.subtitle = element_textbox_simple(
      margin = margin(l = -58, b = 15)
    ),
    plot.caption = element_markdown(
      hjust = 1
    ),
    plot.background = element_rect(fill = "white", colour = "white")
  ) + 
  labs(
    title = "**Sepak Bola Negara-Negara AFF**",
    subtitle = " Peringkat dan poin didasarkan pada pemeringkatan FIFA, 18 Juli 2024",
    caption = "Data: inside.fifa.com"
  )


gg_stop_recording()

