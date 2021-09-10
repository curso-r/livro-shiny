library(shiny)

ui <- fluidPage(
  "Histograma da distribuição normal",
  sliderInput(
    inputId = "num",
    label = "Selecione o tamanho da amostra",
    min = 1,
    max = 1000,
    value = 100
  ),
  plotOutput(outputId = "hist"),
  textOutput(outputId = "media")
)

server <- function(input, output, session) {

  amostra <- reactive({
    sample(1:10, input$num, replace = TRUE)
  })

  output$hist <- renderPlot({
    barplot(table(amostra()))
  })

  output$media <- renderText({
    contagem <- sort(table(amostra()), decreasing = TRUE)
    mais_frequente <- names(contagem[1])
    glue::glue("O número mais sorteado foi {mais_frequente}.")
  })

}

shinyApp(ui, server)
