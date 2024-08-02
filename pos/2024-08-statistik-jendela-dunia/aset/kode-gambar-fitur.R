library(ggtext)

gapminder |> 
  filter(year == 2007) |> 
  group_by(continent) |> 
  summarise(
    n = n(),
    .groups = "drop"
  ) |> 
  arrange(-n)

gambar_fitur <- gapminder |> 
  filter(
    year %in% c(1992, 1997, 2002, 2007)
  ) |> 
  ggplot(
    aes(x = gdpPercap, y = lifeExp)
  ) + 
  geom_point(
    aes(color = continent, size = pop),
    alpha = .6,
    show.legend = FALSE
  ) + 
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    linewidth = 1,
    alpha = .1,
    color = "black"
  ) + 
  scale_size(
    range = c(1, 20),
    guide = "none"
  ) + 
  scale_color_manual(
    name = "Benua",
    values = c(
      "Africa" = "#164863",
      "Asia" = "#427D9D",
      "Europe" = "#427D9D",
      "Americas" = "#9BBEC8",
      "Oceania" = "#9BBEC8"
    )
  ) + 
  facet_wrap(
    vars(year),
    ncol = 2
  ) + 
  theme_minimal(base_size = 18) + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text = element_blank()
  ) + 
  labs(
    title = "PDB per Kapita vs. Angka Harapan Hidup",
    subtitle = "Hubungan antara PDB per kapita dan angka harapan hidup\npada tahun 1992, 1997, 2002, dan 2007",
    x = "PDB per kapita",
    y = "Angka harapan hidup",
    caption = "Data: Jenny Bryan dkk. / Github"
  )

print(gambar_fitur)
