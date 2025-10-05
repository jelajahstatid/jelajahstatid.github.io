library(tidyverse)
library(readxl)
library(ggrepel)
library(cowplot)
library(glue)
library(factoextra)

# Memproses data ----
dat <- read_csv(
  file = "pos/2025-10-analisis-komponen-utama/aset/1744360375.csv",
  show_col_types = FALSE
)
dat_bersih <- dat |> 
  select(
    Tahun, Provinsi, `Kelompok Bahan Pangan`,
    Komoditas, Konsumsi_Pangan
  ) |> 
  rename(
    tahun = Tahun,
    prov = Provinsi,
    bahan_pangan = `Kelompok Bahan Pangan`,
    komoditas = Komoditas,
    konsumsi = Konsumsi_Pangan
  ) |> 
  group_by(tahun, prov, bahan_pangan) |> 
  summarise(
    konsumsi = max(konsumsi),
    .groups = "drop"
  )

# Tahun 2018 ----
## Memproses data ----
dat_2018 <- dat_bersih |> 
  filter(tahun == 2018) |> 
  select(-tahun) |> 
  pivot_wider(
    names_from = bahan_pangan,
    values_from = konsumsi
  ) |> 
  as.data.frame() |> 
  column_to_rownames(var = "prov")

## AKU ----
aku_2018 <- prcomp(dat_2018)
dat_aku_2018 <- aku_2018$x |> 
  as.data.frame() |> 
  rownames_to_column(var = "prov") |> 
  as_tibble()

## Visualisasi AKU ----
p_18 <- dat_aku_2018 |> 
  mutate(
    label = if_else(
      PC1 >= 80, prov, ""
    )
  ) |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point(
    size = 2,
    alpha = .6
  ) + 
  geom_text_repel(
    aes(label = label)
  ) + 
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Tahun 2018"
  )

# Tahun 2019 ----
## Memproses data ----
dat_2019 <- dat_bersih |> 
  filter(tahun == 2019) |> 
  select(-tahun) |> 
  pivot_wider(
    names_from = bahan_pangan,
    values_from = konsumsi
  ) |> 
  as.data.frame() |> 
  column_to_rownames(var = "prov") |> 
  drop_na()

## AKU ----
aku_2019 <- prcomp(dat_2019)
dat_aku_2019 <- aku_2019$x |> 
  as.data.frame() |> 
  rownames_to_column(var = "prov") |> 
  as_tibble()

## Visualisasi AKU ----
p_19 <- dat_aku_2019 |> 
  mutate(
    label = if_else(
      PC1 >= 80, prov, ""
    )
  ) |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point(
    size = 2,
    alpha = .6
  ) + 
  geom_text_repel(
    aes(label = label)
  ) + 
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Tahun 2019"
  )

# Tahun 2020 ----
## Memproses data ----
dat_2020 <- dat_bersih |> 
  filter(tahun == 2020) |> 
  select(-tahun) |> 
  pivot_wider(
    names_from = bahan_pangan,
    values_from = konsumsi
  ) |> 
  as.data.frame() |> 
  column_to_rownames(var = "prov") |> 
  drop_na()

## AKU ----
aku_2020 <- prcomp(dat_2020)
dat_aku_2020 <- aku_2020$x |> 
  as.data.frame() |> 
  rownames_to_column(var = "prov") |> 
  as_tibble()

## Visualisasi AKU ----
p_20 <- dat_aku_2020 |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point(
    size = 2,
    alpha = .6
  ) + 
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Tahun 2020"
  )

# Tahun 2021 ----
## Memproses data ----
dat_2021 <- dat_bersih |> 
  filter(tahun == 2021) |> 
  select(-tahun) |> 
  pivot_wider(
    names_from = bahan_pangan,
    values_from = konsumsi
  ) |> 
  as.data.frame() |> 
  column_to_rownames(var = "prov") |> 
  drop_na()

## AKU ----
aku_2021 <- prcomp(dat_2021)
dat_aku_2021 <- aku_2021$x |> 
  as.data.frame() |> 
  rownames_to_column(var = "prov") |> 
  as_tibble()

## Visualisasi AKU ----
p_21 <- dat_aku_2021 |> 
  mutate(
    label = if_else(
      PC1 >= 80, prov, ""
    )
  ) |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point(
    size = 2,
    alpha = .6
  ) + 
  geom_text_repel(
    aes(label = label)
  ) + 
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Tahun 2021"
  )

# Tahun 2022 ----
## Memproses data ----
dat_2022 <- dat_bersih |> 
  filter(tahun == 2022) |> 
  select(-tahun) |> 
  pivot_wider(
    names_from = bahan_pangan,
    values_from = konsumsi
  ) |> 
  as.data.frame() |> 
  column_to_rownames(var = "prov") |> 
  drop_na()

## AKU ----
aku_2022 <- prcomp(dat_2022)
dat_aku_2022 <- aku_2022$x |> 
  as.data.frame() |> 
  rownames_to_column(var = "prov") |> 
  as_tibble()

## Visualisasi AKU ----
p_22 <- dat_aku_2022 |> 
  mutate(
    label = if_else(
      PC1 >= 80, prov, ""
    )
  ) |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point(
    size = 2,
    alpha = .6
  ) + 
  geom_text_repel(
    aes(label = label)
  ) + 
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Tahun 2022"
  )

# Tahun 2023 ----
## Memproses data ----
dat_2023 <- dat_bersih |> 
  filter(tahun == 2023) |> 
  select(-tahun) |> 
  pivot_wider(
    names_from = bahan_pangan,
    values_from = konsumsi
  ) |> 
  as.data.frame() |> 
  column_to_rownames(var = "prov") |> 
  drop_na()

## AKU ----
aku_2023 <- prcomp(dat_2023)
dat_aku_2023 <- aku_2023$x |> 
  as.data.frame() |> 
  rownames_to_column(var = "prov") |> 
  as_tibble()

## Visualisasi AKU ----
p_23 <- dat_aku_2023 |> 
  mutate(
    label = if_else(
      PC1 <= -15, prov, ""
    )
  ) |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point(
    size = 2,
    alpha = .6
  ) + 
  geom_text_repel(
    aes(label = label)
  ) + 
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Tahun 2023"
  )

# Tahun 2024 ----
## Memproses data ----
dat_2024 <- dat_bersih |> 
  filter(tahun == 2024) |> 
  select(-tahun) |> 
  pivot_wider(
    names_from = bahan_pangan,
    values_from = konsumsi
  ) |> 
  as.data.frame() |> 
  column_to_rownames(var = "prov") |> 
  drop_na()

## AKU ----
aku_2024 <- prcomp(dat_2024)
dat_aku_2024 <- aku_2024$x |> 
  as.data.frame() |> 
  rownames_to_column(var = "prov") |> 
  as_tibble()

## Visualisasi AKU ----
p_24 <- dat_aku_2024 |> 
  mutate(
    label = if_else(
      PC1 <= -50, prov, ""
    )
  ) |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point(
    size = 2,
    alpha = .6
  ) + 
  geom_text_repel(
    aes(label = label)
  ) + 
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Tahun 2024"
  )

# Gabung plot ----
plot_grid(
  p_18, p_19, p_20, p_21,
  p_22, p_23, p_24,
  ncol = 2
)

# Lebih lanjut ke 2022 ----
dat_aku_2022 |> 
  mutate(
    label = if_else(
      PC1 >= 10, prov, ""
    )
  ) |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point(
    size = 2,
    alpha = .6
  ) + 
  geom_text_repel(
    aes(label = label)
  ) + 
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Tahun 2022"
  )

dat_aku_2022_g <- dat_aku_2022 |> 
  mutate(
    grup = if_else(
      PC1 >= 80, 0, 1
    )
  )
dat_aku_2022_p <- dat_aku_2022_g |> 
  group_by(grup) |> 
  summarise(
    PC1_p = mean(PC1),
    PC2_p = mean(PC2),
    .groups = "drop"
  )
dat_aku_2022_g <- dat_aku_2022_g |> 
  mutate(
    dist = sqrt((PC1 + 3.78)^2 + (PC2 - 0.447)^2)
  ) |> 
  group_by(grup) |> 
  slice_min(order_by = dist, n = 4)

dat_aku_2022_g |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point(
    size = 2,
    alpha = .6
  ) + 
  geom_text_repel(
    aes(label = prov)
  ) + 
  coord_cartesian(
    xlim = c(-40, 160)
  ) + 
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Tahun 2022"
  )

p_col_2022 <- dat_bersih |> 
  filter(
    tahun == 2022,
    prov %in% dat_aku_2022_g$prov
  ) |> 
  ggplot(aes(y = bahan_pangan, x = konsumsi)) + 
  geom_col(
    aes(fill = konsumsi)
  ) + 
  theme_minimal() + 
  theme(
    axis.title.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold")
  ) + 
  facet_wrap(~prov, ncol = 5) + 
  labs(
    x = "Konsumsi Pangan (kg/kap/tahun)",
    title = "Rata-rata Konsumsi per Jenis Pangan Tahun 2024",
    caption = "Data: Badan Pangan Nasional"
  )

pctg_pc1 <- aku_2022$sdev[1] |> 
  round(2)
pctg_pc2 <- aku_2022$sdev[2] |> 
  round(2)

dat_aku_2022_lab <- dat_aku_2022 |> 
  mutate(
    grup = if_else(
      prov %in% dat_aku_2022_g$prov, 1, 0
    ),
    label = if_else(
      grup == 1, prov, ""
    )
  )
p_ku_2022 <- dat_aku_2022_lab |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point(
    aes(colour = as_factor(grup)),
    shape = 1,
    size = 3,
    stroke = 1.5
  ) + 
  geom_text_repel(
    aes(label = label),
    max.overlaps = 50
  ) + 
  coord_cartesian(
    xlim = c(-60, 140)
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    x = paste0("KU1 (", pctg_pc1, "%)"),
    y = paste0("KU2 (", pctg_pc2, "%)"),
    title = "Analisis Komponen Utama (AKU)",
    subtitle = "Data rata-rata konsumsi per jenis pangan penduduk Indonesia tahun 2022 yang\ndiproyeksikan pada bidang komponen utama (KU1 vs. KU2)"
  )

plot_grid(
  p_ku_2022, p_col_2022,
  ncol = 1
)

## Terskala ----
aku_2022n <- prcomp(
  dat_2022,
  scale. = TRUE
)
fviz_eig(aku_2022n)
fviz_pca_var(
  aku_2022n,
  col.var = "contrib",
  repel = TRUE,
  col.circle = "black"
)
aku_2022$center

dat_aku_2022n <- aku_2022n$x |> 
  as.data.frame() |> 
  rownames_to_column(var = "prov") |> 
  as_tibble()
dat_aku_2022n |> 
  mutate(
    label = if_else(
      PC1 <= -3, prov, ""
    )
  ) |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point() + 
  geom_text_repel(
    aes(label = label)
  )

# Analisis Akumulatif ----
dat_akum <- dat_bersih |> 
  mutate(
    tahun_prov = paste(tahun, prov, sep = "_")
  ) |> 
  select(
    tahun_prov, bahan_pangan, konsumsi
  ) |> 
  drop_na()

dat_akum_wide <- dat_akum |> 
  pivot_wider(
    names_from = bahan_pangan,
    values_from = konsumsi
  ) |> 
  as.data.frame() |> 
  column_to_rownames(var = "tahun_prov") |> 
  drop_na()

hasil_aku_akum <- prcomp(dat_akum_wide, scale. = TRUE)
dat_aku_akum <- hasil_aku_akum$x |> 
  as.data.frame() |> 
  rownames_to_column(var = "tahun_prov") |> 
  as_tibble() |> 
  separate(
    tahun_prov,
    into = c("tahun", "prov"),
    sep = "_"
  ) |> 
  mutate(
    tahun_prov = paste(tahun, prov, sep = "_"),
    label = if_else(
      PC1 >=3 | PC2 >=2, tahun_prov, ""
    ),
    grup = if_else(
      PC1 >=3 | PC2 >=2, 1, 0
    )
  )


## Visualisasi PCA ----
dat_aku_akum |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point(
    aes(col = as_factor(grup)),
    size = 2,
    alpha = .6
  ) + 
  geom_text_repel(
    aes(label = label),
    max.overlaps = 50
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Analisis Komponen Utama",
    subtitle = "Data rata-rata konsumsi per jenis pangan penduduk Indonesia (2018-2024)\nyang diproyeksikan pada bidang komponen utama 1 dan 2",
    caption = "Sumber data: Badan Pangan Nasional"
  )

fviz_pca_var(
  hasil_aku_akum,
  repel = TRUE
)
