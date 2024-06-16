library(shiny)
library(leaflet)
library(shinyWidgets)
library(shinythemes)

local_choices <- sort(unique(local_empresa$aba))
empresa_choices <- sort(unique(local_empresa$empresa))

fluidPage(
  theme = shinytheme("sandstone"),
  tags$head(tags$style(
    HTML(
      "
      body {
        background-color: #e5e0f7;
      }
      .map-title {
        font-family: 'Arial', sans-serif;
        font-size: 18px;
        font-weight: bold;
        background-color: white;
        padding: 5px 10px;
        border-radius: 5px;
        box-shadow: 0 0 10px rgba(152, 141, 194, 0.5);
      }
      .custom-popup {
        font-family: 'Arial', sans-serif;
        font-size: 14px;
        background-color: white;
        border: 2px solid #555;
        padding: 10px;
        border-radius: 5px;
        box-shadow: 0 0 5px rgba(152, 141, 194, 0.5);
        max-width: 300px;
      }
      .main-container {
        background-color: #e5e0f7;
        padding: 20px;
      }
      .title-panel {
        font-family: 'Arial', sans-serif;
        font-size: 24px;
        font-weight: bold;
        color: #f2a641;
      }
    "
    )
  )),
  titlePanel(div(class = "title-panel", "Cultivo de girassol na Argentina")),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      pickerInput(
        inputId = "LOCAL",
        label = "Local",
        choices = local_choices,
        selected = "Anguil",
        multiple = TRUE,
        options = list(`actions-box` = TRUE, style = "btn-primary")
      ),
      pickerInput(
        inputId = "EMPRESA",
        label = "Empresa",
        choices = unique(empresa_choices),
        selected = "ACA",
        multiple = TRUE,
        options = list(`actions-box` = TRUE, style = "btn-primary")
      )
    ),
    mainPanel(
      div(
        class = "main-container",
        fluidRow(
          column(width = 6, plotlyOutput("BOXPLOT")),
          column(width = 6, plotlyOutput("BARPLOT"))
        ),
        fluidRow(
          column(width = 6, plotlyOutput("CORRPLOT")),
          column(width = 6, plotlyOutput("CATTER"))
        ),
        leafletOutput("LEAFLET")
      )
    )
  )
)
