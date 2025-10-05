library(tidyverse)
library(ggrepel)
library(ggsci)
library(scales)
library(cowplot)

# Impor dan transformasi data ----
url_data <- "https://raw.githubusercontent.com/jelajahstatid/jelajahstatid.github.io/refs/heads/main/pos/2025-10-analisis-komponen-utama/aset/1744360375.csv"

dat_pangan <- read_csv(
  file = url_data,
  col_names = TRUE
)

dat_pangan <- dat_pangan |> 
  rename(
    tahun = Tahun,
    prov = Provinsi,
    bahan_pangan = `Kelompok Bahan Pangan`,
    konsumsi = Konsumsi_Pangan
  ) |> 
  select(
    tahun, prov, bahan_pangan,
    Komoditas, konsumsi
  ) |> 
  group_by(
    tahun, prov, bahan_pangan
  ) |> 
  summarise(
    konsumsi = max(konsumsi),
    .groups = "drop"
  )

dat_pangan_lebar <- dat_pangan |> 
  pivot_wider(
    names_from = bahan_pangan,
    values_from = konsumsi
  ) |> 
  mutate(
    tahun_prov = paste(tahun, prov, sep = "_")
  ) |> 
  select(-tahun, -prov) |> 
  as.data.frame() |> 
  column_to_rownames(var = "tahun_prov") |> 
  drop_na()

# Analisis komponen utama (AKU) ----
hasil_aku <- prcomp(dat_pangan_lebar, scale. = TRUE)
dat_aku <- hasil_aku$x |> 
  as.data.frame() |> 
  rownames_to_column(var = "tahun_prov") |> 
  as_tibble() |> 
  separate(tahun_prov, into = c("tahun", "prov"), sep = "_")
dat_aku_rot <- hasil_aku$rotation |> 
  as.data.frame() |> 
  rownames_to_column(var = "bahan_pangan") |> 
  as_tibble() |> 
  mutate(
    d2 = sqrt(PC1^2 + PC2^2)
  )
ringkasan_aku <- summary(hasil_aku)
var_aku <- ringkasan_aku$importance |> 
  t() |> 
  as.data.frame() |> 
  rownames_to_column(var = "PC") |> 
  select(PC, `Proportion of Variance`) |> 
  rename(
    prop_var = `Proportion of Variance`,
    KU = PC
  ) |> 
  mutate(
    KU = paste0("KU", row_number())
  )
pst_pc1 <- round(ringkasan_aku$importance[2, "PC1"] * 100, 2)
pst_pc2 <- round(ringkasan_aku$importance[2, "PC2"] * 100, 2)

# Visualisasi AKU ----
## Proyeksi data pada PC1 dan PC2 ----
p1 <- dat_aku |> 
  mutate(tahun = as_factor(tahun)) |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 1
  ) + 
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 1
  ) + 
  geom_point(
    size = 1,
    shape = 1,
    stroke = 2,
    col = "#164863"
  ) + 
  theme_minimal() + 
  labs(
    x = paste0("KU1 (", pst_pc1, "%)"),
    y = paste0("KU2 (", pst_pc2, "%)")
  )

## Bobot setiap variabel ----
p2 <- dat_aku_rot |> 
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_segment(
    aes(
      x = 0, y = 0,
      xend = PC1, yend = PC2,
      col = d2
    ),
    linewidth = 1,
    arrow = arrow(length = unit(0.3, "cm"))
  ) + 
  annotate(
    "path",
    x = 0 + 1*cos(seq(0, 2*pi,length.out = 100)),
    y = 0 + 1*sin(seq(0, 2*pi,length.out = 100)),
    alpha = .3
  ) + 
  scale_color_gradient(
    low = "#9BBEC8",
    high = "#164863"
  ) + 
  coord_fixed(
    xlim = c(-1, 1),
    ylim = c(-1, 1)
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title = element_blank()
  )

## Variansi yang dijelaskan setiap komponen utama ----
p3 <- var_aku |> 
  ggplot(aes(x = KU, y = prop_var)) + 
  geom_col(
    aes(fill = prop_var)
  ) + 
  geom_line(
    aes(group = 1),
    col = "black",
    linewidth = 1
  ) + 
  geom_point(
    col = "black",
    size = 2,
    shape = 1,
    stroke = 2
  ) + 
  scale_y_continuous(labels = percent_format()) + 
  scale_fill_gradient(
    low = "#9BBEC8",
    high = "#164863"
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none",
    axis.title = element_blank()
  )

plot_grid(
  plot_grid(p3, p2, ncol = 2),
  p1,
  ncol = 1,
  rel_heights = c(1, 1.5)
)
