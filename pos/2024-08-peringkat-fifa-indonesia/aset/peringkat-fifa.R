library(tidyverse)
library(scales)
library(gganimate)

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
    tahun = year(date)
  ) |> 
  select(date, tahun, country, IDN, rank_aff, total_points)

# Plot statis ----
data_id |> 
  ggplot() + 
  geom_rect(
    aes(
      xmin = 0, xmax = total_points,
      ymin = rank_aff - .45, ymax = rank_aff + .45,
      fill = IDN
    )
  ) + 
  geom_text(
    col = "gray10",
    hjust = "right",
    aes(label = country, y = rank_aff),
    x = -50
  ) + 
  geom_text(
    aes(label = as.character(tahun)),
    x = 1300,
    y = -12,
    size = 18
  ) + 
  scale_x_continuous(
    limits = c(-600, 1600),
    breaks = c(0, 500, 1000, 1500)
  ) + 
  scale_y_reverse() + 
  scale_fill_manual(
    values = c(
      "TRUE" = "red",
      "FALSE" = "grey20"
    )
  ) + 
  theme_classic(base_size = 18) + 
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_blank()
  ) + 
  labs(
    x = "Total Poin",
    caption = "Data: FIFA.com"
  ) + 
  gganimate::transition_time(date)

animate(
  plot = last_plot(),
  renderer = gifski_renderer()
)
