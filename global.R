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
main_theme <- bs_theme(version = version_default(), bg = "#FFFFFF", fg = "#007681", primary = "#007681","navbar-bg" = "#007681")
# Tar bort utrymme över lagertabellen
headerCallback <- c("function(thead, data, start, end, display){  $('th', thead).css('display', 'none'); $('.dataTables_scrollHead').remove();}")

# Laddar in raster
# rstPath <- list.files("data/Kartor", pattern = "*.tif", full.names = T)
# rst <- rast(rstPath[c(11:17, 1:10)]) %>% projectRasterForLeaflet("ngb") %>% round(0)
# names(rst) <- names(layerlist2[-c(1,2,13)])
# mask <- rast("data/mask_rst.tif") %>% projectRasterForLeaflet("ngb") %>% round(0)
# rst <- rst * mask
# saveRDS(rst, "rst.rds")
rst <- readRDS("rst.rds")
pal <- colorFactor(c("#333333", "#ff0000"), c(0,1), na.color = "transparent")
tag.map.title <- tags$style(HTML(".leaflet-control.map-title {transform: translate(30px,20%); position: fixed !important; right: calc(-280px + 50%); padding-left: 10px; 
                                 padding-right: 10px; background: rgba(255,255,255,0.95); font-weight: bold; font-size: calc(5px + 2vw); border: solid; border-radius: 10px;}
                                 .leaflet .legend i{width: 2.5vh; height: 2.5vh; border-radius: 5px;}
                                 .leaflet .legend {line-height: 3vh; font-size: 2vh; border: solid #007681; color: #007681; border-radius: 10px;}
                                 .bslib-card, .tab-content, .tab-pane, .card-body {overflow: visible !important;}
                                 thead, tbody, tfoot, tr, td, th {border-color: inherit;border-style: solid;border-width: 0;}
                                 .table{border-bottom-width: 0px;}"))
# Hämtar matris
# w <- read_xlsx("data/Linking_EST_to_EC_to_maps_sept_2023.xlsx", sheet="EC_EST_matrix", range="D2:Q19") %>%  setDT() %>% .[!is.na(KOD), ,]
w <- read.csv2("data/Matrix.csv")
rownames(w) <- w$X
wm_0123 <- w[,-1]
# wm_0123 <- as.matrix(w, rownames.force= w$KOD)[, 2:ncol(w)] %>%  as.integer() %>% 
#   matrix(nrow = nrow(w), ncol = ncol(w) - 1, byrow = F, dimnames=list(w$KOD, colnames(w)[2:ncol(w)]))
# wm_01 <- ifelse(wm_0123 >= 1, 1, 0)

baltic <- sf::st_read("data/balticMap/BalticNation.shp") 
sluColRed <- readRDS("data/sluColRed.rds")
