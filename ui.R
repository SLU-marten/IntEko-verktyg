library("shiny")
library("fresh")
library("bslib")
library("terra")
library("leaflet")
library("DT")
library("bslib")
library("htmltools")
library("data.table")
library("readxl")
library("shinyWidgets")
library("ggplot2")

shinyUI(
  navbarPage(
    title = "Ekosystemtjänster",
    theme = main_theme,
    # Skapar UI för fliken med kartöversikt
    tabPanel(title = "Kartöversikt",
             layout_column_wrap(
               width = NULL, heights_equal = "row",
               style = css(grid_template_columns = "300px 3fr"),
               card(min_height = "calc(100vh - 110px)",
                    card_header("Kartlager", class = "bg-primary"),
                    card_body(dataTableOutput(outputId = "layerListTable"),  class = "p-0")),
               card(card_header("Karta", class = "bg-primary"),
                    card_body(leafletOutput(outputId = "layerMap"), class = "p-0")))),
    # Skapar UI för fliken med bä
    tabPanel("Beräkna",
             layout_column_wrap(
               width = 0.5, heights_equal = "row",
               card(min_height = "calc(50vh - 110px)",
                    card_header("Välj ekosystemkomponenter", class = "bg-primary"),
                    card_body(
                      virtualSelectInput(
                        "pickEC", NULL, multiple = T, showValueAsTags = TRUE,
                        choices = list(
                          # "Shore habitat" = layerlist[1:2],
                          "Benthic vegetation" = layerlist[3:7],
                          "Benthic fauna" = layerlist[8:9],
                          "Fish" = layerlist[c(10:12, 14:18)],
                          "Marine mammals" = layerlist[19:20])))),
               card(card_header("Välj ekosystemtjänster", class = "bg-primary"),
                    card_body(
                      virtualSelectInput(
                        "pickES", NULL, multiple = T, showValueAsTags = TRUE,
                        choices = list(
                          "Regulating" = ekoServiceList[1:5],
                          "Supporting" = ekoServiceList[7],
                          "Provisioning" = ekoServiceList[8:14],
                          "Cultural" = ekoServiceList[15:17]))))),
             layout_column_wrap(
               width = 0.5, heights_equal = "row",
               card(min_height = "calc(50vh - 110px)", full_screen = TRUE,
                    card_header("Matris", class = "bg-primary"),
                    card_body(sliderInput("matMax", "Maxvärde på matris", min = 0, max = 3, value = 3, step = 0.1),
                              dataTableOutput(outputId = "matris"),  class = "p-0")),
               card(card_header("Resultat", class = "bg-primary d-flex justify-content-between",
                                checkboxInput("binary", "Normalisera?", FALSE)), 
                    full_screen = TRUE,
                    card_body(leafletOutput(outputId = "resultMap"), class = "p-0"))),
             fluidRow(column(12, align = "center", 
                             actionButton("calc", "Beräkna"),
                             downloadButton("downloadMap", "Download map"))),
             br()
    )
  )
)
