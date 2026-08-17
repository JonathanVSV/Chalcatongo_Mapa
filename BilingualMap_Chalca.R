# — Preprocess-----
library(leaflet)
library(sf)
library(htmltools)
library(htmlwidgets)
library(rnaturalearth)
library(leafem)
library(terra)
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

# Reclasificar capas
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

# Leer datos Español
poly <- st_read("Data/Causas de cus de 2000 a 2013.gpkg") |>
  st_transform(4326) |>
  mutate(across(Causa, ~ifelse(.x == "Evento desencadenante social \n", "Evento desencadenante social", .x))) |>
  mutate(across(Causa, ~as.factor(.x)))
poly2 <- st_read("Data/Causas de cus de 2013 a 2024.gpkg") |>
  st_transform(4326) |>
  mutate(across(Causa, ~ifelse(.x == "Evento desencadenante social \n", "Evento desencadenante social", .x))) |>
  mutate(across(Causa, ~as.factor(.x)))

# Mixteco
polyM <- st_read("Data/Causas de Cus de 2000-2013 y 2013-2024 en mixteco/Causas de Cus 2000 a 2013 en mixteco.gpkg") |>
  st_transform(4326) |>
  mutate(across(Causa, ~ifelse(.x == "Evento desencadenante social \n", "Evento desencadenante social", .x))) |>
  mutate(across(Causa, ~as.factor(.x)))
poly2M <- st_read("Data/Causas de Cus de 2000-2013 y 2013-2024 en mixteco/Causas de Cus 2000 a 2013 en mixteco.gpkg") |>
  st_transform(4326) |>
  mutate(across(Causa, ~ifelse(.x == "Evento desencadenante social \n", "Evento desencadenante social", .x))) |>
  mutate(across(Causa, ~as.factor(.x)))
cerros <- st_read("Data/Cerros o montes de Chalcatongo en mixteco/Cerros o montes de Chalcatongo en mixteco.gpkg")|>
  st_transform(4326) 

# Unir capas de dos idiomas
poly <- poly |>
  left_join(polyM |>
              st_drop_geometry(),
            by = "Número")
poly2 <- poly2 |>
  left_join(poly2M |>
              mutate(across(`Número`, ~as.numeric(.x))) |>
              st_drop_geometry(),
            by = "Número")

unique(poly$Group)

# mapview(im)

# Agregar rasters y leyenda
colores_rast <- c(#"black",
  "orange","#42ab0d", "#f71f2f","orchid",
  "purple", "gray60", "yellow", "blue", "white")
paleta <- leaflet::colorFactor(colores_rast,
                               terra::values(im),
                               na.color = "#FFFFFF00")

# Clasificar clases en dos idiomas
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
  ungroup() |>
  mutate(sum_translated = c("Nuu ka kaja yunu te ka chi'i itu", "Ñu'un yuuku, yuuku ",
                            "Nuu ñu'un yuuku, kuu nuu ka jituto", "Nuu ñu'un ja tuu kuu yuuku, kuu nuu ñu'un ja ko jituto",
                            "Nuu ñu'un nuu ka jituto tuu kuitɨ na yuuni yoó", "Ñu'un yuku ja tuu kuɨtɨ yunu iin nuu ñu'unga, chi kaa rɨɨi",
                            "Ñu'un ja yoo ñunkuun nuu ka jituto ta'an kuia", "Nuu ñu'un ja tuukuitɨ yuunu iin, kaa te'e ii, rɨɨii", 
                            "Ñu'un nuu tuu kuitɨ nuu yuunu kui yoo nuu ñu'ungua ndɨkɨu ni kaa sua chii kaa rɨɨii"))

# Agregar polígonos
colores <- c("#a6cee3","#bd0026", "#ffff33","#984ea3","#a65628")
pal <- leaflet::colorFactor(colores,
                            domain = poly$Causa,
                            levels = levels(poly$Causa),
                            na.color = "#FFFFFF00")

## Points
### Create customized markers
### Can create in several lists, that's why two lapply are used
### In this case we really only need one level
resul <- lapply(1:length(colores), function(j){
  leaflet::makeAwesomeIcon(
    icon = "circle",
    library = "fa",
    iconColor = colores[j],
    markerColor = "white",
    
  )
}) 

# Cast as awesome icon list
resul <- structure(resul, class = "leaflet_awesome_icon_set")

# Generar popups bilingues

popups <- paste0(
  '<div style="font-size:13px; min-width:170px;">',
  
  # English version
  '<div class="bi-es">',
  '<b style="font-size:14px;">', 'Causa de cambio', '</b><br>',
  '<b>Categor\u00EDa causa:</b> ',  poly$Causa.x,    '<br>',
  '<b>Causa:</b> ',     poly$Causa.espe.x,
  '</div>',
  
  # Spanish version
  '<div class="bi-mix">',
  '<b style="font-size:14px;">', 'Change driver', '</b><br>',
  '<b>Cause category:</b> ',  poly$Causa.y,    '<br>',
  '<b>Cause:</b> ',     poly$Causa.espe.y,
  '</div>',
  
  '</div>'
)

popups2 <- paste0(
  '<div style="font-size:13px; min-width:170px;">',
  
  # English version
  '<div class="bi-es">',
  '<b style="font-size:14px;">', 'Causa de cambio', '</b><br>',
  '<b>Categor\u00EDa causa:</b> ',  poly2$Causa.x,    '<br>',
  '<b>Causa:</b> ',     poly2$Causa.espe.x,
  '</div>',
  
  # Spanish version
  '<div class="bi-mix">',
  '<b style="font-size:14px;">', 'Change driver', '</b><br>',
  '<b>Cause category:</b> ',  poly2$Causa.y,    '<br>',
  '<b>Cause:</b> ',     poly2$Causa.espe.y,
  '</div>',
  
  '</div>'
)

# — Title (top-left) --------------------------------------------------------
title_ctrl <- tags$div(
  class = "leaflet-control",
  style = "background:white; padding:8px 14px; border-radius:6px;
           box-shadow:0 1px 5px rgba(0,0,0,.4); max-width:250px;",

  tags$div(
    class = "bi-es",
    tags$b("Causas de cambios detectados en el municipio de Chalcatongo"),
    tags$br(),
    tags$span(style = "font-size:11px; color:#666;",
              "Haz clic para conocer detalles")
  ),
  tags$div(
    class = "bi-mix",
    tags$b("Causes of changes in Chalcatongo"),
    tags$br(),
    tags$span(style = "font-size:11px; color:#666;",
              "Click to know details")
  )
)

# — Language toggle button (top-right) --------------------------------------
lang_btn <- tags$button(
  id      = "lang-btn",
  onclick = "toggleLang()",
  style   = "background:white; border:2px solid #444; border-radius:6px;
             padding:5px 13px; font-size:13px; font-weight:bold;
             cursor:pointer; box-shadow:0 1px 5px rgba(0,0,0,.4);",
  
  # Shows "Español" when in English mode → clicking switches to Spanish
  tags$div(class = "bi-es", "\U0001F310  Espa\u00f1ol"),
  # Shows "English" when in Spanish mode → clicking switches back
  tags$div(class = "bi-mix", "\U0001F310  \u00D1uu Savi")
)

# — Custom legend (bottom-right) --------------------------------------------
# Build colour swatches from the quantile breaks
q_breaks <-cobs$ID
q_colors <- colores_rast

swatch_rows <- mapply(function(col, leg_es, leg_mix) {
  tags$div(
    style = "display:flex; align-items:center; margin:3px 0;",
    tags$div(style = paste0(
      "width:20px; height:14px; background:", col, ";",
      "border:1px solid rgba(0,0,0,.2); border-radius:2px; margin-right:7px;"
    )),
    tags$span(class = "bi-es", leg_es),
    tags$span(class = "bi-mix", leg_mix)
  )
}, q_colors, cobs$sum, cobs$sum_translated,  # <- swap in your second-language column
SIMPLIFY = FALSE)

legend_ctrl <- tags$div(
  class = "leaflet-control",
  style = "background:white; padding:8px 12px; border-radius:6px;
           box-shadow:0 1px 5px rgba(0,0,0,.4); font-size:12px;",
  
  tags$b(tags$div(class = "bi-es", "Cambios")),
  tags$b(tags$div(class = "bi-mix", "ndu'u xa'a")),
  tagList(swatch_rows)
)


lang_css <- tags$style(HTML("

  /* ── Default state: English visible ─────────────────────────────────── */
  .bi-mix { display: none;  }
  .bi-es { display: block; }

  /* ── Spanish active: swap visibility ────────────────────────────────── */
  body.lang-mix .bi-es { display: none;  }
  body.lang-mix .bi-mix { display: block; }

  /* ── Button hover ───────────────────────────────────────────────────── */
  #lang-btn:hover { background: #f0f0f0 !important; }

"))

# Language js

lang_js <- tags$script(HTML("
  function toggleLang() {
    document.body.classList.toggle('lang-mix');
  }
"))

# Layers names
group_labels <- c(
  "Esri.WorldTopoMap" = "<span class='bi-es'>ESRI Topografía</span><span class='bi-mix'>Ndaa Iyo</span>",
  "Esri.WorldImagery"  = "<span class='bi-es'>ESRI RGB Satelital</span><span class='bi-mix'>Ini Ñuu</span>",
  "Causas de cambios 2000 a 2013" = "<span class='bi-es'>Causas de cambios 2000 a 2013</span><span class='bi-mix'>Non Kuu ja sa'a  ja ni na saama  kuia uxi xiko ji kuia oko uxi uni</span>",
  "Causas de cambios 2013 a 2024"= "<span class='bi-es'>Causas de cambios 2013 a 2024</span><span class='bi-mix'>Non kuu ja sa'a ja na saama ñu'un ñuuyo ja ni ku kuia oko uxi uni ji kuia oko oko kuun</span>",
  "Cambios 2000 a 2013"    = "<span class='bi-es'>Cambios 2000 a 2013</span><span class='bi-mix'>Tu'un ja ni na sama kuia uxi xiko ji kuia oko uxi uni </span>",
  "Cambios 2013 a 2024"    = "<span class='bi-es'>Cambios 2013 a 2024</span><span class='bi-mix'>Tu'un ja ni na sama kuia oko uxi uni ji kuia oko oko kuun</span>"
)

# — Crear leaflet -----
# Avoid zoom controls at the upperleft
mapa <- leaflet::leaflet(options = leafletOptions(zoomControl = FALSE))

# Agregar mapa base
mapas_base <- c("Esri.WorldTopoMap", "Esri.WorldImagery")

for(provider in mapas_base) {
  mapa <- mapa %>% 
    leaflet::addProviderTiles(provider, 
                              group = provider)
}

mapa <- mapa %>% 
  leaflet::addRasterImage(
    raster::raster(im), 
    colors = paleta, 
    opacity = 0.9,  
    # layerId = "Cambios 2000 a 2013",
    group = "Cambios 2000 a 2013"
  ) |>
  leaflet::addRasterImage(
    raster::raster(im2), 
    colors = paleta, 
    opacity = 0.9,  
    # layerId = "Cambios 2013 a 2024",
    group = "Cambios 2013 a 2024"
  ) 

mapa


## Add points
mapa <- mapa %>% 
  leaflet::addAwesomeMarkers(data = poly, 
                             icon = resul,
                             popup = popups,
                             group = "Causas de cambios 2000 a 2013") %>% 
  leaflet::addAwesomeMarkers(data = poly2, 
                             icon = resul,
                             popup = popups2,
                             group = "Causas de cambios 2013 a 2024")

mapa


mapa <- mapa %>%
  addControl(title_ctrl,  position = "topleft") %>%
  addControl(lang_btn,    position = "topright") %>%
  addControl(legend_ctrl, position = "bottomright") %>%
  leaflet::addLayersControl(overlayGroups = c("Causas de cambios 2000 a 2013", 
                                              "Causas de cambios 2013 a 2024",
                                              "Cambios 2000 a 2013", 
                                              "Cambios 2013 a 2024"),
                            baseGroups = mapas_base,
                            options = leaflet::layersControlOptions(collapsed = FALSE,
                                                                    hideSingleBase = TRUE)) %>%
  onRender(sprintf("
    function(el, x) {
      var labels = %s;
      var spans = el.querySelectorAll(
        '.leaflet-control-layers-base span, .leaflet-control-layers-overlays span'
      );
      spans.forEach(function(span) {
        var text = span.textContent.trim();
        if (labels[text]) {
          span.innerHTML = labels[text];
        }
      });
    }
  ", jsonlite::toJSON(as.list(group_labels), auto_unbox = TRUE))) %>%
  leaflet::addMiniMap(tiles = mapas_base[[1]], 
                      toggleDisplay = TRUE,
                      position = "bottomleft") 


mapa


mapa <- mapa %>%
  htmlwidgets::onRender("
    function(el, x) {
      L.control.zoom({ position: 'topleft' }).addTo(this);
    }
  ") %>%
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
    leaflet::easyButton(
      icon = "fa-crosshairs", title = "Ubícame",
      onClick = leaflet::JS("function(btn, map){ map.locate({setView: true});}")),
    leaflet::easyButton(
      icon = "fa-globe", 
      title = "Zoom a Chalcatongo",
      onClick = leaflet::JS("function(btn, map){ map.fitBounds([
                                        [", 16.92588, ",", -97.63843, "], ",
                            "[", 17.06979, ",", -97.46379, "]
                                        ]); }"))) %>%
  # Agregar botón de opacidad de las capas
  # leaflet::addControl(html = "<input id=\"OpacitySlide\" type=\"range\" min=\"0\" max=\"1\" step=\"0.1\" value=\"0.5\">") %>%
  leaflet::addScaleBar(position = "bottomleft",
                       options = leaflet::scaleBarOptions(metric = TRUE,
                                                          imperial = FALSE)) # %>%
  # Agregar cosas para que jale el botón de opacidad
  # htmlwidgets::onRender(
  #   "function(el,x,data){
  #                    var map = this;
  #                    var evthandler = function(e){
  #                       var layers = map.layerManager.getVisibleGroups();
  #                       console.log('VisibleGroups: ', layers); 
  #                       console.log('Target value: ', +e.target.value);
  #                       layers.forEach(function(group) {
  #                         var layer = map.layerManager._byGroup[group];
  #                         console.log('currently processing: ', group);
  #                         Object.keys(layer).forEach(function(el){
  #                           if(layer[el] instanceof L.Polygon){;
  #                           console.log('Change opacity of: ', group, el);
  #                            layer[el].setStyle({fillOpacity:+e.target.value});
  #                           }
  #                         });
  #                         
  #                       })
  #                    };
  #             $('#OpacitySlide').mousedown(function () { map.dragging.disable(); });
  #             $('#OpacitySlide').mouseup(function () { map.dragging.enable(); });
  #             $('#OpacitySlide').on('input', evthandler)}
  #         ")

# prependContent puts CSS in <head> (before the widget renders)
# appendContent puts JS at the end of <body> (after DOM is ready)

mapa <- mapa %>%
  htmlwidgets::prependContent(lang_css) %>%
  htmlwidgets::appendContent(lang_js)

mapa

htmlwidgets::saveWidget(mapa, 
                        "Map/index.html",
                        selfcontained = TRUE)
