library(shiny)
library(tidyverse)
load(url("https://github.com/jennybc/gapminder/raw/main/data/gapminder.rdata"))

ui <- fluidPage(
  titlePanel("PDB per Kapita vs. Angka Harapan Hidup"),
  sliderInput(
    inputId = "tahun",
    label = "Tahun",
    min = 1952,
    max = 2007,
    value = 2007,
    step = 5,
    sep = ""
  ),
  plotOutput("diagram_pencar")
)

server <- function(input, output, session) {
  output$diagram_pencar <- renderPlot({
    tahun_int <- input$tahun
    
    gapminder |> 
      filter(year == tahun_int) |> 
      ggplot(
        aes(x = gdpPercap, y = lifeExp)
      ) + 
      geom_point(
        aes(color = continent, size = pop),
        alpha = .6
      ) + 
      scale_size(
        range = c(5, 30),
        guide = "none"
      ) + 
      scale_color_viridis_d(name = "Benua") + 
      theme_minimal(base_size = 18) + 
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold")
      ) + 
      xlim(c(200, 60000)) + 
      ylim(c(20, 90)) + 
      labs(
        x = "PDB per kapita",
        y = "Angka harapan hidup",
        caption = "Data: Jenny Bryan dkk. / Github"
      )
    
  })
}

shinyApp(ui = ui, server = server)
