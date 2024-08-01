# Gambar fitur ----

# data_pulau dari index.qmd
data_linear <- data_pulau |> 
  mutate(
    nama = "Linear, RMSE = 4,68",
    est = 3.662650 + 0.002549 * luas_pulau,
    rmse = 4.68
  )
data_pangkat <- data_pulau |> 
  mutate(
    nama = "Pangkat, RMSE = 2,26",
    est = a_pangkat * luas_pulau^z_pangkat,
    rmse = 2.26
  )
data_log <- data_pulau |> 
  mutate(
    nama = "Logaritma, RMSE = 2,18",
    est = fitted.values(model_log),
    rmse = 2.18
  )
data_weibull <- data_pulau |> 
  mutate(
    nama = "Weibull, RMSE = 1,76",
    est = fitted.values(model_weibull),
    rmse = 1.76
  )
data_gambar <- bind_rows(
  data_weibull,
  data_log,
  data_pangkat,
  data_linear
)

data_gambar |> 
  mutate(
    nama = fct_reorder(nama, -rmse)
  ) |> 
  ggplot(
    aes(x = luas_pulau, y = banyak_spesies)
  ) + 
  # Linear
  geom_segment(
    data = filter(data_gambar, nama == "Linear, RMSE = 4,68"),
    aes(x = luas_pulau, y = banyak_spesies, yend = est),
    color = "#427D9D"
  ) + 
  # Pangkat
  geom_segment(
    data = filter(data_gambar, nama == "Pangkat, RMSE = 2,26"),
    aes(x = luas_pulau, y = banyak_spesies, yend = est),
    color = "#427D9D"
  ) + 
  # Logaritma
  geom_segment(
    data = filter(data_gambar, nama == "Logaritma, RMSE = 2,18"),
    aes(x = luas_pulau, y = banyak_spesies, yend = est),
    color = "#427D9D"
  ) + 
  # Weibull
  geom_segment(
    data = filter(data_gambar, nama == "Weibull, RMSE = 1,76"),
    aes(x = luas_pulau, y = banyak_spesies, yend = est),
    color = "#427D9D"
  ) + 
  geom_point(
    size = 3,
    color = "#9BBEC8"
  ) + 
  # Linear
  geom_function(
    data = filter(data_gambar, nama == "Linear, RMSE = 4,68"),
    fun = function(x) 3.662650 + 0.002549 * x,
    color = "#164863",
    linewidth = 2
  ) + 
  # Pangkat
  geom_function(
    data = transform(data_gambar, nama = "Pangkat, RMSE = 2,26"),
    fun = function(x) a_pangkat * x^z_pangkat,
    color = "#164863",
    linewidth = 2
  ) + 
  # Logaritma
  geom_function(
    data = filter(data_gambar, nama == "Logaritma, RMSE = 2,18"),
    fun = function(x) c_log + z_log * log10(x),
    color = "#164863",
    linewidth = 2
  ) + 
  # Weibull
  geom_function(
    data = filter(data_gambar, nama == "Weibull, RMSE = 1,76"),
    fun = function(x) d_weibull * (1 - exp(-z_weibull * x^f_weibull)),
    color = "#164863",
    linewidth = 2
  ) + 
  facet_wrap(vars(nama), ncol = 2) + 
  theme_minimal(base_size = 18) + 
  theme(
    axis.title.x = element_markdown(),
    plot.title = element_text(face = "bold")
  ) + 
  labs(
    title = "Luas Pulau vs. Banyak Spesies",
    subtitle = "Pemodelan linear dan nonlinear terhadap\nhubungan antara luas pulau dan banyaknya spesies",
    x = "Luas pulau (m^2^)",
    y = "Banyak spesies",
    caption = "Data: Schrader dkk. / Biodiversity Data Journal"
  )
