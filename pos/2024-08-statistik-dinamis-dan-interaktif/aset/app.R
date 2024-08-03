library(shiny)
load(url("https://github.com/jennybc/gapminder/raw/main/data/gapminder.rdata"))

ui <- fluidPage(
  sliderInput(
    inputId = "tahun",
    label = "Tahun",
    min = 1952,
    max = 2007,
    value = 2007,
    step = 5
  ),
  plotOutput("diagram_pencar")
)

server <- function(input, output, session) {
  output$diagram_pencar <- renderPlot({
    tahun_int <- input$tahun
    
    gapminder |> 
      filter(year == tahun_int) |> 
      ggplot(aes(x = gdpPercap, y = lifeExp)) + 
      geom_point()
    
  })
}

shinyApp(ui = ui, server = server)
