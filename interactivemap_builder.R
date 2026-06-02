library(leaflet)
library(leafem)
library(htmltools)
library(terra)
library(sf)
library(tidyverse)
library(mapview)

# Leer insumos y poner todo en 4326
im <- rast("Data/Imagen 2000 a 2013.tif")|>
  project("EPSG:4326")
im2 <- rast("Data/Imagen 2013 a 2024.tif")|>
  project("EPSG:4326")

ext(im)

# Arreglar las imágenes RGB y pasarlas a gris
im[im[[4]] == 0] <- NA
im2[im2[[4]] == 0] <- NA


im <- app(im[[1:3]], fun ="sum")
im <- as.factor(im)
im2 <- app(im2[[1:3]], fun ="sum")
im2 <- as.factor(im2)
im <- classify(im, 
               matrix(c(0, 147, NA,
                        147, 250, 2,
                        250, 315, 3,
                        315, 386, 4,
                        386, 474, 5,
                        474, 504, 6,
                        504, 547, 7,
                        547, 635, 8,
                        635, 758, 9,
                        758, 760, 10), 
                      ncol=3, 
                      byrow=TRUE))
im <- as.factor(im)
im2 <- classify(im2, 
               matrix(c(0, 147, NA,
                        147, 250, 2,
                        250, 315, 3,
                        315, 386, 4,
                        386, 474, 5,
                        474, 504, 6,
                        504, 547, 7,
                        547, 635, 8,
                        635, 758, 9,
                        758, 760, 10), 
                      ncol=3, 
                      byrow=TRUE))
im2 <- as.factor(im2)

mapview(im)
# Assuming 'r' is your RGB SpatRaster with RGB channels set
# grayscale_raster <- colorize(im[[1:3]], to="col", grays=TRUE)   

poly <- st_read("Data/Causas de cus de 2000 a 2013.gpkg") |>
  st_transform(4326) 
poly2 <- st_read("Data/Causas de cus de 2013 a 2024.gpkg") |>
  st_transform(4326) 

unique(poly$Group)

mapview(im)

# Crear leaflet
mapa <- leaflet::leaflet()

# Agregar rasters y leyenda
colores <- c(#"black",
  "orange","#42ab0d", "#f71f2f","orchid",
             "purple", "gray60", "yellow", "blue", "white")
paleta <- leaflet::colorFactor(colores,
                               terra::values(im),
                               na.color = "#FFFFFF00")
cobs  <- terra::levels(im)[[1]] |>
  mutate(across(sum, ~case_when(
                                     #.x == 1 ~ "Cambio1",
                                     .x == 2 ~ "Bosque a zona agrícola",
                                     .x == 3 ~ "Bosque permanencia",
                                     .x == 4 ~ "Zona agrícola a bosque",
                                     .x == 5 ~ "Sin vegetación a zona agrícola",
                                     .x == 6 ~ "Zona agrícola a sin vegetación",
                                     .x == 7 ~ "Bosque a sin vegetación",
                                     .x == 8 ~ "Zona agrícola permanencia",
                                     .x == 9 ~ "Sin vegetación a bosque",
                                     .x == 10 ~ "Sin vegetación permanencia"))) |>
  group_by(ID) |>
  distinct(sum) |>
  ungroup()

# cobs <- tibble(
#   ID = seq(1,6,1),
#   sum = c("Agricultura", "Bosque", "Sin vegetación", "Cambio1", "Cambio2", "Deforestación")
# )

# colores <- c("#ffef36", "#42ab0d", "#fdfdff", "#c232e8", "#26741f", "#f71f2f")
# paleta <- leaflet::colorFactor(colores,
#                                terra::values(im),
#                                na.color = "#FFFFFF00")

mapa <- mapa %>% 
  leaflet::addRasterImage(
    raster::raster(im), 
    colors = paleta, 
    opacity = 0.9,  
    layerId = "Cambios 2000 a 2013",
    group = "Cambios 2000 a 2013"
  ) |>
  leaflet::addRasterImage(
    raster::raster(im2), 
    colors = paleta, 
    opacity = 0.9,  
    layerId = "Cambios 2013 a 2024",
    group = "Cambios 2013 a 2024"
  ) |>
  # leaflet::addRasterImage(raster::raster(im), 
  #                         colors = paleta, 
  #                         opacity = 0.9,  
  #                         group = "Cambios",
  #                         layerId = "Cambios") %>%
  leaflet::addLegend("bottomleft", 
                     pal = paleta, 
                     values = cobs$ID,
                     title = "Cambios",
                     labFormat =  leaflet::labelFormat(
                       transform = function(x) {
                         cobs %>%
                           dplyr::filter(ID == x) %>%
                           dplyr::pull(!!rlang::sym("sum"))
                       })
  )
mapa
# mapa

# Agregar polígonos
colores <- c("#969696","#a6cee3","#bd0026", "#ffff33","#984ea3","#4daf4a","#a65628", "#252525")
pal <- leaflet::colorFactor(colores,
                            domain = poly$Causa,
                            levels = levels(poly$Causa),
                            na.color = "#FFFFFF00")

# Polígonos
mapa <- mapa %>% 
  leaflet::addMarkers(data = poly,
                       # stroke = TRUE, 
                       # smoothFactor = 0.5, 
                       # opacity = 1,
                       # fillOpacity = 0.9,
                       # fillColor = ~pal(Causa),
                       # weight = 0.5,
                       # color = ~pal(Group),
                       group = "Causas de cambios",
                       popup = ~poly$Causa.espe)

# mapa
# Agregar mapa base
mapas_base <- c("Esri.WorldTopoMap", "Esri.WorldImagery")

for(provider in mapas_base) {
  mapa <- mapa %>% 
    leaflet::addProviderTiles(provider, 
                              group = provider)
}

mapa <- mapa %>%
  leaflet::addLayersControl(overlayGroups = c("Causas de cambios", "Cambios 2000 a 2013", "Cambios 2013 a 2024"),
                            baseGroups = mapas_base,
                            options = leaflet::layersControlOptions(collapsed = FALSE,
                                                                    hideSingleBase = TRUE)) %>%
  leaflet::addMiniMap(tiles = mapas_base[[1]], 
                      toggleDisplay = TRUE,
                      position = "bottomleft") 

mapa <- mapa %>%
  # Actualizar zoom en mini map conforme te muevas en el mapa principal
  htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }") %>% 
  leaflet::addEasyButtonBar(
    # leaflet::easyButton(
    #   icon = "fa-crosshairs", title = "Ubícame",
    #   onClick = leaflet::JS("function(btn, map){ map.locate({setView: true});}")),
    leaflet::easyButton(
      icon = "fa-globe", 
      title = "Zoom a México",
      onClick = leaflet::JS("function(btn, map){ map.fitBounds([
                                        [", 16.92588, ",", -97.63843, "], ",
                            "[", 17.06979, ",", -97.46379, "]
                                        ]); }"))) %>%
  # Agregar botón de opacidad de las capas
  # leaflet::addControl(html = "<input id=\"OpacitySlide\" type=\"range\" min=\"0\" max=\"1\" step=\"0.1\" value=\"0.5\">") %>%
  leaflet::addScaleBar(position = "bottomright",
                       options = leaflet::scaleBarOptions(metric = TRUE,
                                                          imperial = FALSE)) %>%
  # Agregar cosas para que jale el botón de opacidad
  htmlwidgets::onRender(
    "function(el,x,data){
                     var map = this;
                     var evthandler = function(e){
                        var layers = map.layerManager.getVisibleGroups();
                        console.log('VisibleGroups: ', layers); 
                        console.log('Target value: ', +e.target.value);
                        layers.forEach(function(group) {
                          var layer = map.layerManager._byGroup[group];
                          console.log('currently processing: ', group);
                          Object.keys(layer).forEach(function(el){
                            if(layer[el] instanceof L.Polygon){;
                            console.log('Change opacity of: ', group, el);
                             layer[el].setStyle({fillOpacity:+e.target.value});
                            }
                          });
                          
                        })
                     };
              $('#OpacitySlide').mousedown(function () { map.dragging.disable(); });
              $('#OpacitySlide').mouseup(function () { map.dragging.enable(); });
              $('#OpacitySlide').on('input', evthandler)}
          ")
mapa

htmlwidgets::saveWidget(mapa, 
                        "Map/index.html",
                        selfcontained = TRUE)
