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

# Laddar in lista över lager
layerlist <- read.delim("data/layerlist.txt", header = F)$V1
layerlist2 <- setNames(layerlist, layerlist %>% stringr::str_split_i(" ", 1))
ekoServiceList <- read.delim("data/EkoService.txt", header = F)$V1
ekoServiceList2 <- setNames(ekoServiceList, ekoServiceList %>%  stringr::str_split_i(" ", 1))
# Färgtema på appen
main_theme <- bs_theme(bg = "#EEE", fg = "#222", primary = "#007681",
                       "navbar-light-bg" = "#007681 !important",
                       "navbar-light-active-color" = "#FFF !important",
                       "navbar-light-brand-color" = "#FFF !important",
                       "nav-link-color" = "#FFF !important",
                       "nav-link-hover-color" = "#CCC !important",
                       "navbar-toggler-font-size" = "1rem") |> 
  bs_add_rules(
    list(
      ".bslib-card .card-body {background: #FFF;}",
      ".table.dataTable tbody td.active, .table.dataTable tbody tr.active td {
        background-color: #337ab7; color: #000 !important; font-weight: bolder;}"
    )
  )
# Tar bort utrymme över lagertabellen
headerCallback <- c("function(thead, data, start, end, display){  $('th', thead).css('display', 'none'); $('.dataTables_scrollHead').remove();}")

# Laddar in raster
# Rasterfilen är stackad och sparad i en rds-fil. Kod nedan för att göra en ny stack av alla tif-filer i mappen data/Kartor
  # rstPath <- list.files("data/Kartor", pattern = "*.tif", full.names = T)
  # rst <- rast(rstPath[c(11:17, 1:10)]) %>% projectRasterForLeaflet("ngb") %>% round(0)
  # names(rst) <- names(layerlist2[-c(1,2,13)])
  # mask <- rast("data/mask_rst.tif") %>% projectRasterForLeaflet("ngb") %>% round(0)
  # rst <- rst * mask
  # saveRDS(rst, "rst.rds")
rst <- readRDS("rst.rds")

# Färgschema till kartorna
pal <- colorFactor(c("#333333", "#ff0000"), c(0,1), na.color = "transparent")

# Kod för att anpassa karttext m.m. efter fönsterstorlek
# tag.map.title <- tags$style(HTML(".leaflet-control.map-title {transform: translate(30px,20%); position: fixed !important; right: calc(-280px + 50%); padding-left: 10px;
#                                  padding-right: 10px; background: rgba(255,255,255,0.95); font-weight: bold; font-size: calc(5px + 2vw); border: solid; border-radius: 10px;}
#                                  .leaflet .legend i{width: 2.5vh; height: 2.5vh; border-radius: 5px;}
#                                  .leaflet .legend {line-height: 3vh; font-size: 2vh; border: solid #007681; color: #007681; border-radius: 10px;}
#                                  .bslib-card, .tab-content, .tab-pane, .card-body {overflow: visible !important;}
#                                  thead, tbody, tfoot, tr, td, th {border-color: inherit;border-style: solid;border-width: 0;}
#                                  .table{border-bottom-width: 0px;}"))
# Hämtar matris
  # w <- read_xlsx("data/Linking_EST_to_EC_to_maps_sept_2023.xlsx", sheet="EC_EST_matrix", range="D2:Q19") %>%  setDT() %>% .[!is.na(KOD), ,]
  # w <- read.csv2("data/Matrix.csv")
  # rownames(w) <- w$X
  # wm_0123 <- w[,-1]
  # saveRDS(wm_0123, "wm_0123.rds")
wm_0123 <- readRDS("wm_0123.rds")

# Laddar in bakgrundskarta och funktion för att använda SLU-färger
baltic <- sf::st_read("data/balticMap/BalticNation.shp") 
sluColRed <- readRDS("data/sluColRed.rds")

# Introtexter till rintrojs
introText_map <- c("<p><strong> Kartöversikt </p></strong>
               <p>Här lägger vi in lite information om den här sidan...</p>
               <p>ABC 123...</p>",
               '<p><strong> Lista över ekosystemkomponenter </p></strong>
               <p>Här visas en lista över de ekosystemkomponenter som ingår i beräkningarna. </p>
               <p>Klicka på en ekosystemkomponent för att visa kartan över den.</p>',
               '<p><strong> Karta över ekosystemkomponenter </p></strong>
               <p>En karta över vald ekosystemkomponent visas här </p>
               <p>ABC 123...</p>')
introText_calc <- c("<p><strong> Beräkningsinformation </p></strong>
               <p>Här lägger vi in lite information om hur den här sidan används...</p>
               <p>ABC 123...</p>",
               "<p><strong> Ekosystemkomponenter </p></strong>
               <p>Välj vilka ekosystemkomponenter som ska ingå i analysen här.</p>
               <p>ABC 123...</p>",
               "<p><strong> Ekosystemtjänster </p></strong>
               <p>Välj vilka ekosystemtjänster som ska ingå i analysen här. </p>
               <p>ABC 123...</p>",
               "<p><strong> Bidragsmatris </p></strong>
               <p>Matris över ekosystemskomonenternas påverkan på ekosystemtjänsterna </p>
               <p>Värden för de valda ekosystemkomponenterna och -tjänsterna markeras i matrisen </p>
               <p>ABC 123...</p>",
               "<p><strong> Resultat </p></strong>
               <p>Resultatet av analysen visas här</p>
               <p>ABC 123...</p>",
               '<p><strong> Knappar </p></strong>
               <p>När de önskade ekosystemkomponenterna och tjänsterna är valda trycker man beräkna för att påbörja analysen</p>
               <p>För att ladda ned en bildfil av analysresultatet trycker man på "Ladda ned karta"</p>
               <p>ABC 123...</p>')
introSteps_map <- c("#map_all", "#map_EC", "#map_map")
introSteps_calc <- c("#calc_all", "#calc_EC", "#calc_ES", "#calc_matrix", "#calc_result", "#calc_buttons")
