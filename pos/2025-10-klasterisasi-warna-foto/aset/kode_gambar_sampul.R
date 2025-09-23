library(magick)
library(tidyverse)
library(cowplot)

# Menyiapkan data ----
# Memuat dan menskala gambar
foto_ku <- image_read(
  "pos/2025-10-klasterisasi-warna-foto/aset/karuna_donau.png"
) |> 
  image_scale("256x256!")

# Ekstrak data mentah piksel
data_mentah_foto <- image_data(foto_ku)

# Ukuran foto
lebar_foto  <- dim(data_mentah_foto)[2]
tinggi_foto <- dim(data_mentah_foto)[3]

# Konversi ke data frame dan membalik koordinat y
rgb_foto_df <- as.data.frame.table(data_mentah_foto, responseName = "nilai") |>
  mutate(
    kanal = as.integer(Var1),
    koord_x = as.integer(Var2),
    koord_y = as.integer(Var3),
    nilai   = as.integer(nilai)
  ) |>
  select(kanal, koord_x, koord_y, nilai) |>
  pivot_wider(
    names_from = kanal,
    values_from = nilai,
    names_prefix = "ch"
  ) |>
  transmute(
    koord_x,
    koord_y = tinggi_foto - koord_y + 1,
    R = ch1,
    G = ch2,
    B = ch3,
    R_norm = R / 255,
    G_norm = G / 255,
    B_norm = B / 255,
    warna_hex = rgb(R_norm, G_norm, B_norm)
  )

rgb_foto_df |> 
  distinct(warna_hex) |> 
  nrow()

matriks_foto <- rgb_foto_df |> 
  select(R_norm, G_norm, B_norm) |> 
  as.matrix()

set.seed(123)

# Pilih nilai k
k <- 4

# Lakukan klasterisasi k-rerata
hasil_krerata <- kmeans(
  x = matriks_foto,
  centers = k
)

rgb_foto_klaster_df <- rgb_foto_df |> 
  mutate(
    klaster = hasil_krerata$cluster,
    pusat_R = hasil_krerata$centers[klaster, "R_norm"],
    pusat_G = hasil_krerata$centers[klaster, "G_norm"],
    pusat_B = hasil_krerata$centers[klaster, "B_norm"]
  ) |> 
  mutate(
    pusat_hex = rgb(pusat_R, pusat_G, pusat_B)
  ) |> 
  select(koord_x, koord_y, R, G, B, klaster, pusat_hex)

## Klasterisasi ----
# Klasterisasi
hasil_krerata2 <- kmeans(
  x = matriks_foto,
  centers = 2
)

hasil_krerata16 <- kmeans(
  x = matriks_foto,
  centers = 16
)

# Menyiapkan data
rgb_3_klaster_df <- rgb_foto_df |> 
  mutate(
    klaster4 = hasil_krerata$cluster,
    pusat_R = hasil_krerata$centers[klaster4, "R_norm"],
    pusat_G = hasil_krerata$centers[klaster4, "G_norm"],
    pusat_B = hasil_krerata$centers[klaster4, "B_norm"]
  ) |> 
  mutate(
    pusat_hex4 = rgb(pusat_R, pusat_G, pusat_B)
  ) |> 
  select(koord_x, koord_y, R, G, B, klaster4, pusat_hex4) |> 
  mutate(
    klaster2 = hasil_krerata2$cluster,
    pusat_R = hasil_krerata2$centers[klaster2, "R_norm"],
    pusat_G = hasil_krerata2$centers[klaster2, "G_norm"],
    pusat_B = hasil_krerata2$centers[klaster2, "B_norm"]
  ) |> 
  mutate(
    pusat_hex2 = rgb(pusat_R, pusat_G, pusat_B)
  ) |> 
  select(-pusat_R, -pusat_G, -pusat_B) |> 
  mutate(
    klaster16 = hasil_krerata16$cluster,
    pusat_R = hasil_krerata16$centers[klaster16, "R_norm"],
    pusat_G = hasil_krerata16$centers[klaster16, "G_norm"],
    pusat_B = hasil_krerata16$centers[klaster16, "B_norm"]
  ) |> 
  mutate(
    pusat_hex16 = rgb(pusat_R, pusat_G, pusat_B)
  ) |> 
  select(-pusat_R, -pusat_G, -pusat_B)

# Plot foto
p1 <- rgb_foto_df |> 
  ggplot(
    aes(x = koord_x, y = koord_y, fill = warna_hex)
  ) +
  geom_tile() +
  scale_fill_identity() +
  coord_equal() +
  theme_minimal() + 
  theme(
    axis.title = element_blank(),
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Foto Asli (39.987 Warna)"
  )

p2 <- rgb_3_klaster_df |> 
  ggplot(
    aes(x = koord_x, y = koord_y, fill = pusat_hex16)
  ) +
  geom_tile() +
  scale_fill_identity() +
  coord_equal() +
  theme_minimal() + 
  theme(
    axis.title = element_blank(),
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Klasterisasi Warna (k = 16)"
  )

p3 <- rgb_3_klaster_df |> 
  ggplot(
    aes(x = koord_x, y = koord_y, fill = pusat_hex4)
  ) +
  geom_tile() +
  scale_fill_identity() +
  coord_equal() +
  theme_minimal() + 
  theme(
    axis.title = element_blank(),
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Klasterisasi Warna (k = 4)"
  )

p4 <- rgb_3_klaster_df |> 
  ggplot(
    aes(x = koord_x, y = koord_y, fill = pusat_hex2)
  ) +
  geom_tile() +
  scale_fill_identity() +
  coord_equal() +
  theme_minimal() + 
  theme(
    axis.title = element_blank(),
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Klasterisasi Warna (k = 2)"
  )

plot_grid(
  p1, p2, p3, p4, ncol = 2
)
