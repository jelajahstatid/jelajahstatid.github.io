library(tidyverse)
library(ggstream)
library(camcorder)
library(ggtext)

# Mempersiapkan data ----
pendapatan_as_riaa <- read_csv(
  "pos/2025-01-pendapatan-musik/aset/pendapatan_as_riaa.csv",
  show_col_types = FALSE
) |> 
  drop_na()
pendapatan_grup <- pendapatan_as_riaa |> 
  group_by(grup, tahun) |> 
  summarise(
    total_pendapatan = sum(pendapatan, na.rm = TRUE),
    total_pendapatan_infl = sum(pendapatan_infl, na.rm = TRUE),
    .groups = "drop"
  )

# Rekam ----
gg_record(
  dir = "pos/2025-01-pendapatan-musik/aset/rekaman/",
  device = "png",
  width = 800,
  height = 600, 
  units = "px",
  dpi = 72,
  bg = "white"
)

# Visualisasi data ----

# Warna
palette_function <- colorRampPalette(c("#164863", "#9BBEC8"))
palet_warna <- palette_function(5)


# Grafik
pendapatan_grup |> 
  mutate(
    grup = factor(grup, levels = c("Vinyl", "Tape", "Disc", "Download",
                                   "Streaming", "Other"))
  ) |> 
  ggplot(
    aes(
      x = tahun,
      y = total_pendapatan_infl,
      fill = grup
    )
  ) + 
  geom_col(
    position = "stack"
  ) + 
  geom_hline(
    yintercept = 0,
    linewidth = 1.5
  ) + 
  guides(
    fill = guide_legend(ncol = 6)
  ) + 
  theme_minimal(base_size = 24) + 
  scale_fill_manual(
    name = "",
    values = c(
      "Vinyl" = "#9BBEC8",
      "Tape" = "#79A0AE",
      "Disc" = "#588395",
      "Download" = "#37657C",
      "Streaming" = "#164863",
      "Other" = "black"
    )
  ) + 
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Pendapatan Industri Musik di AS",
    subtitle = "Disesuaikan dengan Inflasi, Nilai Dolar AS 2003",
    y = "Pendapatan (juta dolar AS)",
    caption = "Data: Recording Industry Association of America (RIAA)"
  )

# Stop rekaman ----
gg_stop_recording()

