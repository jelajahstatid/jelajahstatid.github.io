library(magick)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scatterplot3d)
library(cowplot)

# Load and scale
my_image <- image_read(
  "pos/2025-09-klaterisasi-k-rerata/aset/karuna_donau.png"
) |> 
  image_scale("256x256!")

# Extract raw pixel data
raw_data <- image_data(my_image)

# Image dimensions
img_width  <- dim(raw_data)[2]
img_height <- dim(raw_data)[3]

# Convert to dataframe with flipped y_coord
rgb_df <- as.data.frame.table(raw_data, responseName = "value") |>
  mutate(
    channel = as.integer(Var1),
    x_coord = as.integer(Var2),
    y_coord = as.integer(Var3),
    value   = as.integer(value) # convert hex-string to integer
  ) |>
  select(channel, x_coord, y_coord, value) |>
  pivot_wider(
    names_from = channel,
    values_from = value,
    names_prefix = "ch"
  ) |>
  transmute(
    x_coord,
    # Flip y so that 1 = bottom row instead of top row
    y_coord = img_height - y_coord + 1,
    R = ch1,
    G = ch2,
    B = ch3,
    R_norm = R / 255,
    G_norm = G / 255,
    B_norm = B / 255
  )

head(rgb_df)

# Plot distinct colors ----
# Extract unique normalized colors
colors_df <- rgb_df |> 
  distinct(R_norm, G_norm, B_norm)
# Add hex color for plotting
colors_df <- colors_df |> 
  mutate(color_hex = rgb(R_norm, G_norm, B_norm))

# Static 3D scatterplot
scatterplot3d(
  x = colors_df$R_norm,
  y = colors_df$G_norm,
  z = colors_df$B_norm,
  color = colors_df$color_hex,
  pch = 16,            # solid circle
  cex.symbols = 1,     # size of points
  xlab = "Red",
  ylab = "Green",
  zlab = "Blue",
  main = "Unique Colors in RGB Space"
)

# Plot the image ----
rgb_df |> 
  ggplot(
    aes(x = x_coord, y = y_coord, fill = rgb(R_norm, G_norm, B_norm))
  ) +
  geom_tile() +
  scale_fill_identity() +
  coord_equal() +
  theme_void()

# Downsampling ----
# Choose step size (e.g., 4 = keep every 4th pixel)
step <- 4  

rgb_df_small <- rgb_df |> 
  filter(x_coord %% step == 0,
         y_coord %% step == 0)

rgb_df_small |> 
  ggplot(
    aes(x = x_coord, y = y_coord, fill = rgb(R_norm, G_norm, B_norm))
  ) +
  geom_tile() +
  scale_fill_identity() +
  coord_equal() +
  theme_void() +
  ggtitle(paste("Downsampled image (every", step, "pixels)"))

# Clustering ----
# Select normalized RGB for clustering
rgb_matrix <- rgb_df|> 
  select(R_norm, G_norm, B_norm)|> 
  as.matrix()

# Run k-means with k = 5
set.seed(123)  # for reproducibility
k <- 4
kmeans_res <- kmeans(rgb_matrix, centers = k)

# Extract cluster assignments and centroids
rgb_df_clustered <- rgb_df |> 
  mutate(
    cluster = kmeans_res$cluster,
    centroid_R = kmeans_res$centers[cluster, "R_norm"],
    centroid_G = kmeans_res$centers[cluster, "G_norm"],
    centroid_B = kmeans_res$centers[cluster, "B_norm"]
  ) |> 
  mutate(
    color_hex = rgb(R_norm, G_norm, B_norm),
    centroid_hex = rgb(centroid_R, centroid_G, centroid_B)
  )



# Check result
head(rgb_df_clustered)

# Plot original and clustered image ----
# Original image plot
original_plot <- rgb_df |> 
  ggplot(
    aes(x = x_coord, y = y_coord, fill = rgb(R_norm, G_norm, B_norm))
  ) +
  geom_tile() +
  scale_fill_identity() +
  coord_equal() + 
  theme(
    axis.title = element_blank()
  )

clustered_plot <- ggplot(rgb_df_clustered, aes(x = x_coord, y = y_coord, fill = centroid_hex)) +
  geom_tile() +
  scale_fill_identity() +
  coord_equal() + 
  theme(
    axis.title = element_blank()
  )

# Combine side-by-side
combined_plot <- cowplot::plot_grid(original_plot, clustered_plot, ncol = 2)
