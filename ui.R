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
library("bsicons")
library("rintrojs")

shinyUI(
  page_navbar(
    title = "Ekosystemtjänster",
    theme = main_theme,
    fillable_mobile = TRUE,
    id = "menu",
    introjsUI(),
    #### Skapar UI för fliken med kartöversikt ####
    nav_panel(
      title = "Kartöversikt",
      div(
        id = "map_all",
        layout_column_wrap(
          width = NULL, heights_equal = "row", fill = F,
          style = css(grid_template_columns = "300px 3fr"),
          # Box med lista över alla ekosystemkomponenter 
          div(
            id = "map_EC",
            card(
              min_height = "calc(100vh - 110px)",
              card_header("Kartlager", class = "bg-primary"),
              card_body(dataTableOutput(outputId = "layerListTable"),  class = "p-0")
            )
          ),
          div(
            id = "map_map",
            card(
              card_header("Karta", class = "bg-primary"),
              card_body(leafletOutput(outputId = "layerMap"),class = "p-0")
            )
          )
        )
      )
    ),
    # Skapar UI för fliken med beräkning
    nav_panel(
      title = "Beräkna",
      # Boxar för övre raden
      div(
        id = "calc_all",
        layout_column_wrap(
          width = 0.5, heights_equal = "row", fill = F,
          # Box för val av ekosystemkomponenter
          div(
            id = "calc_EC",
            card(
              fill = F,
              card_header("Välj ekosystemkomponenter", class = "bg-primary"),
              card_body(
                virtualSelectInput(
                  html = T, inline = T, "pickEC", NULL, multiple = TRUE, showValueAsTags = TRUE,
                  choices = list(
                    # "Shore habitat" = layerlist[1:2],
                    "Benthic vegetation" = layerlist[3:7],
                    "Benthic fauna" = layerlist[8:9],
                    "Fish" = layerlist[c(10:12, 14:18)],
                    "Marine mammals" = layerlist[19:20])
                )
              )
            )
          ),
          # Box för val av ekosystemtjänster
          div(
            id = "calc_ES",
            card(
              fill = F,
              card_header("Välj ekosystemtjänster", class = "bg-primary"),
              card_body(
                virtualSelectInput(
                  "pickES", NULL, multiple = T, showValueAsTags = TRUE,
                  choices = list(
                    "Regulating" = ekoServiceList[1:5],
                    "Supporting" = ekoServiceList[7],
                    "Provisioning" = ekoServiceList[8:14],
                    "Cultural" = ekoServiceList[15:17]
                  )
                )
              )
            )
          ),
          # ),
          # # Boxar för nedre raden
          # layout_column_wrap(
          #   width = 0.5, heights_equal = "row",
          # Box för känslighetsmatrisen
          div(
            id = "calc_matrix",
            card(
              fill = T,
              min_height = "calc(50vh - 110px)",
              full_screen = TRUE,
              card_header("Matris", class = "bg-primary d-flex justify-content-between",
                          popover(
                            title = "Input controls",
                            bs_icon("gear"),
                            sliderInput("matMax", "Maxvärde på matris", 
                                        min = 0, max = 3, value = 3, step = 0.1),
                            checkboxInput("binary", "Normalisera?", FALSE)
                          )
              ),
              card_body(dataTableOutput(outputId = "matris"),  class = "p-0")
            )
          ),
          # Box för resultat
          div(
            id = "calc_result",
            card(
              fill = T,
              min_height = "calc(50vh - 110px)",
              card_header("Resultat", class = "bg-primary"), 
              full_screen = TRUE,
              card_body(leafletOutput(outputId = "resultMap"), class = "p-0")
            )
          )
        )
      ),
      # Knappar för beräkning 
      fluidRow(
        column(
          width = 12, align = "center", 
          div(
            id = "calc_buttons",
            actionButton("calc", "Beräkna"),
            downloadButton("downloadMap", "Ladda ned karta")
          )
        )
      ),
      br()
    ),
    nav_spacer(),
    bslib::nav_item(
      tags$button(
        id = "help",
        class = "btn action-button",
        icon("question"),
        style = "color: #fff; border-color: #fff; border-radius: 100%;
        padding-left: 10px; padding-right: 10px; padding-top: 5px; padding-bottom: 5px;"
      )
    )
  )
)