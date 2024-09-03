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

server <- shinyServer(function(input, output, session){
  tags$style(HTML('<meta name="viewport" content="width=800">'))
  #### Skapar reaktiv variabel ####
  vals <- reactiveValues(selectedRow = 1, matris = wm_0123)
  #### ________________________ ####
  #### KARTÖVERSIKT ####
  #### . . . Lista över kartlager ####
  output$layerListTable <- renderDT({
    datatable(data.frame(Lager = layerlist),
              rownames = F, colnames = NULL, escape = F,
              fillContainer = TRUE, style = "bootstrap", 
              selection = list(mode = "single", target="row", selected = isolate(vals$selectedRow)),
              options = list(pageLength = 100, dom = 't', searching = FALSE, headerCallback = JS(headerCallback)))
  })
  #### . . . Välj kartlager och uppdatera karta ####
  observeEvent(input$layerListTable_rows_selected,{
    # browser()
    vals$selectedRow <- input$layerListTable_rows_selected
    title <- tags$div(tag.map.title, HTML(layerlist[input$layerListTable_rows_selected]))  
    selRast <- tryCatch(rst[[names(layerlist2[input$layerListTable_rows_selected])]],
                        error = function(e){
                          r_fail <- rst[[1]] * NA
                          names(r_fail) <- names(layerlist2[input$layerListTable_rows_selected])
                          r_fail
                          })
    leafletProxy("layerMap") %>% clearImages() %>% clearControls() %>% setView(18.8, 60, zoom = 7) %>% 
      addRasterImage(selRast, colors = pal, project = F, opacity = 0.8) %>% 
      addLegend("bottomright", pal = pal, title = "Legend", opacity = 0.95, values = as.integer(values(selRast))) %>% 
      addControl(title, position = "topleft", className="map-title")
  })
  
  #### . . . Leaflet-karta ####
  output$layerMap <- renderLeaflet(leaflet() %>% addProviderTiles(providers$Stadia.AlidadeSmooth))
  
  #### ________________________ ####
  #### BERÄKNA ####
  #### . . . Matris####
  output$matris <- renderDataTable({
    mat <- cbind(wm_0123, "rad" = 0)
    
    selRow <- which(rownames(mat) %in% (input$pickEC %>%  stringr::str_split_i(" ", 1)))
    selCol <- which(colnames(mat) %in% (input$pickES %>%  stringr::str_split_i(" ", 1)))
    mat[selRow, "rad"] <- 1
    
    # browser()
    
    datatable(mat, editable = TRUE, fillContainer = TRUE, escape = F,
              selection = list(mode = "none"),
              options = list(pageLength = 100, dom = 't', searching = F, ordering = F, 
                             columnDefs = list(list(className = 'dt-center', targets = c(0:21)),
                                               list(visible = FALSE, targets=c('rad'))))) %>% 
      formatStyle(0, target = 'row', lineHeight = '70%') %>% 
      formatStyle('rad', target = 'row', backgroundColor = styleEqual(c(1, 0), c('#ff00ff50', "white")))  %>% 
      formatStyle(selCol, backgroundColor = 'transparent')
  })
  
  # Uppdaterar matris när användare gör ändringar
  observeEvent(input$matris_cell_edit, {
    row  <- input$matris_cell_edit$row
    clmn <- input$matris_cell_edit$col
    vals$matris[row, clmn] <- input$matris_cell_edit$value
  })
  
  #### . . . Leaflet-karta ####
  output$resultMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stadia.AlidadeSmooth) %>%
      setView(18.8, 60, zoom = 7)
  })
  
  #### . . . Beräkna-knapp ####
  observeEvent(input$calc, {
    r_tot <- list()    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Beräknar...", value = 0)
    for(i in 1:length(input$pickEC)){
      r_es <- list()
      for(j in 1:length(input$pickES)){
        progress$inc(1 / (length(input$pickEC) * length(input$pickES)), detail = paste((i - 1) * length(input$pickES) + j, "av", length(input$pickEC) * length(input$pickES)))
        EC <- input$pickEC[i] %>%  stringr::str_split_i(" ", 1)
        ES <- input$pickES[j] %>%  stringr::str_split_i(" ", 1)
        V <- vals$matris[EC, ES] / 3 * input$matMax
        r <-  rst[EC] * V
        r_es[[j]] <- r
      }
      r_tot[[i]] <- rast(r_es) %>% sum(na.rm = T)
      gc()
    }
    r_sum <- rast(r_tot) %>% sum(na.rm = T)
    rm(r_tot)
    gc()
    if(input$binary){r_sum <- r_sum / max(values(r_sum), na.rm = T)}
    vals$rst <- r_sum
    title <- tags$div(tag.map.title, HTML("Resultat"))  
    pal2 <- colorNumeric(c("#2c7bb6", "#ffffbf", "#d7191c"), values(r_sum), na.color = "transparent")
    leafletProxy("resultMap") %>% clearImages() %>% clearControls() %>% setView(18.8, 60, zoom = 7) %>% 
      addRasterImage(r_sum, colors = pal2, project = F, opacity = 0.8) %>% 
      addLegend("bottomright", pal = pal2, title = "Legend", opacity = 0.95, values = values(r_sum))
  })
  #### . . . Ladda ned-knapp ####
  output$downloadMap <- downloadHandler(
    filename = paste0("instEko.png"),
    content = function(file){
      r <- terra::project(vals$rst, baltic, method = "near")
      rast_df <- dplyr::arrange(na.omit(raster::as.data.frame(r , xy = TRUE)), desc(names(r )))
      rast_df$brks <- rast_df[, 3]
      
      rasterClass <- classInt::classIntervals(rast_df$brks, n = 5, style = "equal")
      brks <- rasterClass$brks
      lbl <- c()
      for (i in 1:length(brks) - 1) {
        lbl[i] <- paste(round(brks[i], 2), "-", round(brks[i + 1], 2))
      }
      rast_df$brks <- cut(rast_df[, 3], breaks = brks, labels = lbl)
      sluColRed$na.value <- "#f7f6e9"
      p <- ggplot() +
        geom_sf(data = baltic, col = "lightgrey") +
        geom_tile(data = rast_df, aes_string(x = "x", y = "y", fill = "brks")) +
        sluColRed +
        coord_sf(xlim = c(16.5,21), ylim = c(58.5,61.8)) +
        guides(fill=guide_legend(title="Legend")) +
        xlab("Longitude") + ylab("Latitude") +
        theme_classic()
      ggsave(file, plot = p)
    })
  
  #### Hur-knapp ####
  observeEvent(input$help, {
    introText <- if(input$menu == "Kartöversikt") introText_map else introText_calc
    introSteps <- if(input$menu == "Kartöversikt") introSteps_map else introSteps_calc
    introjs(  
      session,
      options = list(
        "nextLabel" = "Nästa",
        "prevLabel" = "Tillbaka",
        "showProgress" = "true",
        "overlayOpacity" = 0.7,
        "steps" = data.frame(element = introSteps, intro = introText)
      )
    )
  })
  
})