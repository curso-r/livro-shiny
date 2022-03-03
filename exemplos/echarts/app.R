library(shiny)
library(echarts4r)

vars <- ggplot2::txhousing |>
  dplyr::select(where(is.numeric), -year, -month, -date) |>
  names()

cidades <- unique(ggplot2::txhousing$city)

ui <- fluidPage(
  titlePanel("echarts"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "cidades",
        "Selecione as cidades",
        multiple = TRUE,
        choices = cidades,
        selected = cidades[1]
      ),
      selectInput(
        "serie",
        "Selecione a série",
        choices = vars,
        selected = vars[1]
      )
    ),
    mainPanel(
      echarts4r::echarts4rOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  output$plot <- echarts4r::renderEcharts4r({
    ggplot2::txhousing |>
      dplyr::filter(city %in% input$cidades) |>
      dplyr::mutate(year = as.character(year)) |>
      dplyr::group_by(city, year) |>
      dplyr::summarise(avg_serie = mean(.data[[input$serie]], na.rm = TRUE)) |>
      e_charts(x = year) |>
      e_line(serie = avg_serie) |>
      e_tooltip()
  })
}

shinyApp(ui, server)
