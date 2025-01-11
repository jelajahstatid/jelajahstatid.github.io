library(tidyverse)
library(plotly)

# Impor data ----
pendapatan_as_riaa <- read_csv(
  "pos/2025-01-pendapatan-musik/aset/pendapatan_as_riaa.csv",
  show_col_types = FALSE
)

# Visualisasi data {ggplot2} ----

# Pemetaan warna
warna_diagram <- c(
  "LP/EP" = "#2A5784",
  "Vinyl Single" = "#43719F",
  "8 - Track" = "#5B8DB8",
  "Cassette" = "#7AAAD0",
  "Cassette Single" = "#9BC7E4",
  "Other Tapes" = "#BADDF1",
  "Kiosk" = "#E1575A",
  "CD" = "#EE7423",
  "CD Single" = "#F59D3D",
  "SACD" = "#FFC686",
  "DVD Audio" = "#9D7760",
  "Music Video (Physical)" = "#F1CF63",
  "Download Album" = "#7C4D79",
  "Download Single" = "#9B6A97",
  "Ringtones & Ringbacks" = "#BE89AC",
  "Download Music Video" = "#D5A5C4",
  "Other Digital" = "#EFC9E6",
  "Synchronization" = "#BBB1AC",
  "Paid Subscription" = "#24693D",
  "On-Demand Streaming (Ad-Supported)" = "#398949",
  "Other Ad-Supported Streaming" = "#61AA57",
  "SoundExchange Distributions" = "#7DC470",
  "Limited Tier Paid Subscription" = "#B4E0A7"
)
# Urutan level
urutan_format <- c(
  # Vinyl
  "LP/EP", "Vinyl Single",
  # Tape
  "8 - Track", "Cassette", "Cassette Single", "Other Tapes",
  # Disc and others
  "Kiosk", "CD", "CD Single", "SACD",
  "DVD Audio", "Music Video (Physical)",
  # Download
  "Download Album", "Download Single",
  "Ringtones & Ringbacks", "Download Music Video",
  "Other Digital", "Synchronization",
  # Streaming
  "Paid Subscription",
  "On-Demand Streaming (Ad-Supported)",
  "Other Ad-Supported Streaming",
  "SoundExchange Distributions",
  "Limited Tier Paid Subscription"
)
# Plot
p <- pendapatan_as_riaa |> 
  filter(
    grup %in% c("Tape", "Disc")
  ) |> 
  group_by(grup, tahun) |> 
  summarise(
    sum = sum(pendapatan_infl),
    .groups = "drop"
  ) |> 
  ggplot(
    aes(x = tahun, y = sum)
  ) + 
  geom_line(aes(group = grup)) + 
  geom_point()
ggplotly(p)
plot_musik <- pendapatan_as_riaa |> 
  mutate(
    format = factor(
      format,
      levels = urutan_format
    )
  ) |> 
  ggplot(
    aes(x = tahun, y = pendapatan, fill = format)
  ) + 
  geom_bar(
    position = "stack",
    stat = "identity"
  ) + 
  scale_fill_manual(
    name = "Format",
    values = warna_diagram
  ) + 
  guides(
    fill = guide_legend(ncol = 4)
  ) + 
  theme_minimal() + 
  theme(
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.spacing.y = unit(.025, "cm"),
    plot.title = element_text(
      face = "bold"
    )
  ) + 
  labs(
    title = "Pendapatan Industri Musik di AS Berdasarkan Format",
    subtitle = "Disesuaikan dengan Inflasi, Nilai Dolar Tahun 2023",
    y = "Pendapatan\n(juta dolar AS)",
    caption = "Data: Recording Industry Association of America (RIAA)"
  )

# Visualisasi {plotly}

ggplotly(plot_musik) |> 
  layout(
    legend = list(
      orientation = "v"
    )
  )

