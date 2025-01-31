library(readxl)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggraph)
library(igraph)
library(ggtext)
library(camcorder)
library(scales)
library(sigmajs)

# Peta kolaborasi ----
## Impor data ----
data_cerme14 <- read_excel(
  "pos/2025-02-cerme14/aset/CERME14_metadata.xlsx"
)

## Mempersiapkan data ----
# Memisah negara ke dalam barisnya sendiri
data_cerme14_panjang <- data_cerme14 |> 
  separate_rows(country, sep = "; ") |> 
  mutate(country = trimws(country))

# Menghitung banyaknya artikel/poster tiap negara
n_negara <- data_cerme14_panjang |> 
  count(country, sort = TRUE)

# Membuat data kolaborasi antarnegara
kolaborasi_antarnegara <- data_cerme14 |> 
  filter(str_detect(country, ";")) |> 
  separate_rows(country, sep = "; ") |> 
  mutate(country = trimws(country)) |> 
  select(id, country) |> 
  group_by(id) |> 
  reframe(
    kolaborasi = combn(country, 2, FUN = paste, collapse = " - "),
    .groups = "drop"
  ) |> 
  separate_rows(kolaborasi, sep = "\n") |> 
  count(kolaborasi, sort = TRUE)

# Memuat peta dunia
peta_dunia <- ne_countries(scale = "medium", returnclass = "sf")

# Menggabungkan `peta_dunia` dan `n_negara`
data_peta <- peta_dunia |> 
  left_join(n_negara, by = c("name" = "country"))

# Membuat data posisi negara
posisi_negara <- peta_dunia |> 
  select(admin, geometry) |>
  st_centroid() |> 
  st_coordinates() |> 
  as.data.frame() |> 
  rename(lon = X, lat = Y) |> 
  cbind(country = peta_dunia$admin)

# Menggabungkan `kolaborasi_antarnegara` dan `posisi_negara`
sisi <- kolaborasi_antarnegara |> 
  separate(
    kolaborasi,
    into = c("negara1", "negara2"),
    sep = " - "
  ) |> 
  left_join(
    posisi_negara, by = c("negara1" = "country")
  ) |> 
  rename(lon1 = lon, lat1 = lat) |> 
  left_join(posisi_negara, by = c("negara2" = "country")) |> 
  rename(lon2 = lon, lat2 = lat)

# Membersikan `sisi`
sisi_rapi <- sisi |> 
  drop_na(lon1, lat1, lon2, lat2) |> 
  filter(negara1 != negara2)

## Memplot kolaborasi antarnegara ----
palet_warna <- c(
  "#FBCB00",
  "#FDCA00",
  "#00A1DF",
  "#00A3E2",
  "#3FB8E7",
  "#7FD0EF",
  "#BFE7F7"
)

# Mulai merekam
gg_record(
  dir = "pos/2025-02-cerme14/aset/rekaman/",
  device = "png",
  width = 1600,
  height = 900, 
  units = "px",
  dpi = 72,
  bg = "white"
)

# Fon
sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = "pos/2025-02-cerme14/aset/Font Awesome 6 Brands-Regular-400.otf"
)
showtext::showtext_auto()

github_icon <- "&#xf09b"
x_icon <- "&#xe61a"
github_username <- "ydkristanto"
x_username <- "yosepdwik"

takarir_sosial <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
  <span style='font-family:sans;'>{github_username}</span>  <span style='font-family:\"Font Awesome 6 Brands\";'>{x_icon};</span>
  <span style='font-family:sans;'>{x_username}</span>"
)


ggplot() +
  geom_sf(
    data = data_peta, 
    aes(fill = n),
    color = "white"
  ) + 
  geom_curve(
    data = sisi_rapi,
    aes(
      x = lon1, y = lat1,
      xend = lon2, yend = lat2, 
      linewidth = n
    ), 
    color = "#FBCB00",
    curvature = 0.2, alpha = 0.4,
    show.legend = FALSE
  ) +
  scale_linewidth(range = c(0.5, 3)) + 
  scale_fill_gradient(
    high = "#00506F",
    low = "#3FB8E7",
    na.value = "grey90"
  ) + 
  theme_classic(base_size = 28) + 
  theme(
    axis.title = element_blank(),
    legend.position = "none",
    plot.title = element_textbox_simple(
      face = "bold",
      family = "sans",
      margin = margin(t = 20, r = 20, b = 5, l = 20)
    ),
    plot.subtitle = element_textbox_simple(
      lineheight = 1.5, hjust = 0.5,
      margin = margin(t = 15, r = 20, b = 10, l = 20),
      colour = "#202020",
      family = "sans"
    ),
    plot.caption = element_textbox_simple(
      colour = "#202020",
      halign = .5
    )
  ) + 
  labs(
    title = "CERME 14: Bozen-Bolzano (Italy), 4–8 February 2025",
    subtitle = "Collaboration network of countries based on CERME 14 co-authorships in conference papers and posters. Curves indicate <b><span style='color:#FBCB00'>collaboration strength</span></b>, while <b><span style='color:#00506F'>darker country colors</span></b> represent a higher number of contributions.",
    caption = takarir_sosial
  )

# Stop perekaman
gg_stop_recording()

# Co-word analysis ----
## Mempersiapkan data ----

# Assuming 'cerme14_data' is your dataset with the 'keywords' column
edge_list <- data_cerme14 |> 
  filter(!is.na(keywords) & keywords != "") |>  # Remove rows with missing or empty keywords
  separate_rows(keywords, sep = ", ") |>  # Split keywords into separate rows
  mutate(keywords = tolower(keywords)) |>  # Convert all keywords to lowercase
  group_by(id) |>  # Group by paper ID
  mutate(keywords = sort(keywords)) |>  # Sort keywords alphabetically within each group
  # Filter out rows with less than two keywords before applying combn
  filter(length(keywords) > 1) |> 
  summarise(
    keyword_pairs = list(combn(keywords, 2, FUN = paste, collapse = ", ")), 
    .groups = "drop"
  ) |> 
  unnest(keyword_pairs) |>  # Unnest the pairs
  separate(keyword_pairs, into = c("keyword1", "keyword2"), sep = ", ") |> 
  filter(keyword1 != keyword2) |>  # Remove identical pairs
  count(keyword1, keyword2, name = "Weight") |> 
  rename(Source = keyword1, Target = keyword2) |>   # Rename columns to match your desired output
  arrange(desc(Weight))

# View the edge list
head(edge_list)

## Menyimpan data ----
write.csv(
  edge_list, "pos/2025-02-cerme14/aset/edge_list_cerme14.csv",
  row.names = FALSE
)








