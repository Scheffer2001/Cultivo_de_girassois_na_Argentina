library(shiny)

shinyServer(function(input, output, session) {
  FILTERED_DATA <- reactive({
    filter_data(tb, LOCAL = input$LOCAL, EMPRESA = input$EMPRESA)
  })
  FILTERED_DATA2 <- reactive({
    filter_data2(tb, LOCAL = input$LOCAL)
  })
  FILTERED_DATA3 <- reactive(({
    filter_data3(tb, EMPRESA = input$EMPRESA)
  }))
  output$BOXPLOT <- renderPlotly({
    tb_sel <- FILTERED_DATA()
    plot_boxplot(tb_sel, plotly = TRUE)
  })
  output$BARPLOT <- renderPlotly({
    tb_sel <- FILTERED_DATA2()
    plot_barplot(tb_sel, plotly = TRUE)
  })
  output$CORRPLOT <- renderPlotly({
    tb_sel <- FILTERED_DATA()
    plot_corrplot(tb_sel, plotly = TRUE)
  })
  output$LEAFLET <- renderLeaflet({
    tb_sel <- FILTERED_DATA3()
    req(nrow(tb_sel) > 0)
    plot_country_map(tb_sel)
  })
  output$CATTER <- renderPlotly({
    tb_sel <- FILTERED_DATA2()
    plot_catterpillar(tb_sel, plotly = TRUE)
  })
})
