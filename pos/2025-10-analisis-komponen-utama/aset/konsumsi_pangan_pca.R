library(tidyverse)
library(readxl)
library(ggrepel)

# Impor data ----
# Data diunduh dari https://satudata.badanpangan.go.id/datasetpublications/gsp/konsumsi-provinsi pada tanggal 1 Oktober 2025
# Arsip: https://web.archive.org/web/20251001124310/https://satudata.badanpangan.go.id/datasetpublications/gsp/konsumsi-provinsi
dat <- read_excel("pos/2025-10-analisis-komponen-utama/aset/rata-rata-konsumsi-per-jenis-pangan.xlsx")

dat_kepr_gula <- dat |> 
  filter(
    Provinsi == "Kepulauan Riau",
    `Kelompok Bahan Pangan` == "Gula"
  )

dat_kepr_sayur <- dat |> 
  filter(
    Provinsi == "Kepulauan Riau",
    `Kelompok Bahan Pangan` == "Sayuran dan buah"
  )

dat_kepr <- dat |> 
  filter(
    Provinsi == "Kepulauan Riau"
  ) |> 
  group_by(Tahun, `Kelompok Bahan Pangan`) |> 
  summarise(
    konsumsi = max(Konsumsi_Pangan),
    .groups = "drop"
  ) |> 
  rename(
    tahun = Tahun,
    bahan_pangan = `Kelompok Bahan Pangan`
  )

# Memproses data ----
dat_sumatera_2024 <- dat |> 
  filter(
    Tahun == 2024,
    Provinsi %in% c(
      "Aceh", "Sumatera Utara", "Sumatera Barat", "Riau",
      "Jambi", "Sumatera Selatan", "Bengkulu",
      "Lampung", "Kepulauan Bangka Belitung", "Kepulauan Riau",
      ""
    )
  ) |> 
  select(
    Tahun, Provinsi, `Kelompok Bahan Pangan`, Konsumsi_Pangan
  ) |> 
  rename(
    tahun = Tahun,
    prov = Provinsi,
    bahan_pangan = `Kelompok Bahan Pangan`,
    konsumsi = Konsumsi_Pangan
  ) |> 
  tibble()

dat_ringkasan <- dat_sumatera_2024 |> 
  filter(bahan_pangan != "Gula") |> 
  group_by(prov, bahan_pangan) |> 
  summarise(
    jumlah = max(konsumsi),
    .groups = "drop"
  )

dat_lebar <- dat_ringkasan |> 
  pivot_wider(
    names_from = bahan_pangan,
    values_from = jumlah
  )

dat_lebar_t <- dat_lebar |> 
  column_to_rownames("prov") |> 
  t() |> 
  as_tibble(rownames = "variable") |> 
  column_to_rownames("variable")

# Visualisasi Data ----
dat_ringkasan |> 
  ggplot(aes(x = jumlah, y = bahan_pangan)) + 
  geom_col(aes(fill = bahan_pangan)) + 
  facet_wrap(~ prov, ncol = 2) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  ) + 
  labs(
    x = "Rata-rata konsumsi (kg/kap/tahun)"
  )

# Visualisasi konsumsi gula Prov. Kep. Riau
dat_kepr |> 
  filter(bahan_pangan == "Gula") |> 
  ggplot(aes(x = tahun, y = konsumsi)) + 
  geom_line(
    col = "#2d70b3"
  ) + 
  geom_point(
    shape = 1,
    size = 2,
    stroke = 1.5,
    col = "#fa7e19"
  ) + 
  theme_minimal() + 
  labs(
    x = "Tahun",
    y = "Rata-rata konsumsi (kg/kap/tahun)",
    title = "Prov. Kepulauan Riau",
    subtitle = "Rata-rata konsumsi gula",
    caption = "Data: Direktorat Penganekaragaman Konsumsi Pangan"
  )

# Visualisasi konsumsi sayur dan buah Kep. Riau
dat_kepr |> 
  filter(bahan_pangan == "Sayuran dan buah") |> 
  ggplot(aes(x = tahun, y = konsumsi)) + 
  geom_col(
    aes(fill = konsumsi)
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none"
  ) + 
  labs(
    x = "Tahun",
    y = "Rata-rata konsumsi (kg/kap/tahun)",
    title = "Prov. Kepulauan Riau",
    subtitle = "Rata-rata konsumsi sayuran dan buah",
    caption = "Data: Direktorat Penganekaragaman Konsumsi Pangan"
  )

# PCA ----
hasil_pca <- prcomp(dat_lebar_t, center = TRUE, scale. = TRUE)
dat_pca_prov <- hasil_pca$rotation |> 
  as.data.frame() |> 
  rownames_to_column() |> 
  rename(
    prov = rowname
  ) |> 
  as_tibble()

# Visualisasi hasil PCA ----
# Dimensi satu (PC1)
dat_pca_prov |> 
  ggplot(aes(x = PC1, y = 0)) + 
  geom_hline(yintercept = 0) + 
  geom_point(
    size = 3,
    alpha = .5,
    col = "#fa7e19"
  ) + 
  geom_text_repel(
    aes(label = prov),
    size = 3, color = "black",
    max.overlaps = 100
  ) + 
  theme_minimal() + 
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

# Dimensi dua (PC1 vs. PC2)
dat_pca_prov |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point(
    size = 3,
    alpha = .5,
    col = "#fa7e19"
  ) + 
  geom_text_repel(
    aes(label = prov),
    size = 3, color = "black",
    max.overlaps = 100
  ) + 
  theme_minimal()

