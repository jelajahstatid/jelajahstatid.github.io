library(tuneR)
library(seewave)
library(tidyverse)
library(ggstream)
library(camcorder)
library(ggtext)

# Membaca data mp3 ----
file_path <- "pos/2025-01-siniar-ikhtisar/aset/siniar_jelajahstatid.mp3"
audio <- readMP3(file_path)

# Rekam ----
gg_record(
  dir = "pos/2025-01-siniar-ikhtisar/aset/rekaman/",
  device = "png",
  width = 800,
  height = 600, 
  units = "px",
  dpi = 72,
  bg = "white"
)

# Visualisasi gelombang suara ----
audio_rate <- oscillo(
  audio,
  f = audio@samp.rate,
  fastdisp = TRUE,
  plot = FALSE
)
colnames(audio_rate) <- "amplitude"
audio_rate_data <- audio_rate |> 
  as_tibble()

# Total audio length in seconds
audio_length_seconds <- 15 * 60 + 33  # 933 seconds

# Number of rows in the data
num_rows <- nrow(audio_rate_data)

# Time increment per row
time_increment <- audio_length_seconds / num_rows

# Add the 'time' column
audio_rate_data <- audio_rate_data |> 
  mutate(
    time = seq(0, audio_length_seconds, length.out = num_rows)
  )

# View the updated data
tail(audio_rate_data)

# Plot dengan ggplot2
audio_rate_data |> 
  ggplot(aes(x = time, y = amplitude)) + 
  geom_line(
    aes(colour = amplitude),
    linewidth = 1,
    show.legend = FALSE
  ) + 
  scale_colour_gradient(
    low = "#427D9D",
    high = "#164863"
  ) + 
  theme_minimal(base_size = 24) + 
  theme(
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Gelombang Audio Siniar",
    subtitle = "Perubahan amplitudo terhadap waktu selama durasi siniar",
    x = "Waktu (detik)",
    y = "Amplitudo"
  )

# Plot the spectrogram (optional)
spectro(audio, f = audio@samp.rate)

# Stop rekaman ----
gg_stop_recording()
