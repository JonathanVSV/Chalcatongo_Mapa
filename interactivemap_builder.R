# Preprocess-----
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

# Arreglar las imágenes RGB y pasarlas a gris
im[im[[4]] == 0] <- NA
im2[im2[[4]] == 0] <- NA

# Reclasificar capas
rcl <- matrix(c(0, 147, NA,
                147, 250, 2,
                250, 315, 3,
                315, 386, 4,
                386, 474, 5,
                474, 504, 6,
                504, 547, 7,
                547, 635, 8,
                635, 758, 9,
                758, 760, 10),
              ncol = 3,
              byrow = TRUE)

im  <- app(im[[1:3]],  fun = "sum") |> classify(rcl)
im2 <- app(im2[[1:3]], fun = "sum") |> classify(rcl)

# Tablas de clases y causas -----
# Tipos de cambio
clases <- tibble(
  ID = 2:10,
  es = c("Bosque a zona agrícola",
         "Bosque permanencia",
         "Zona agrícola a bosque",
         "Sin vegetación a zona agrícola",
         "Zona agrícola a sin vegetación",
         "Bosque a sin vegetación",
         "Zona agrícola permanencia",
         "Sin vegetación a bosque",
         "Sin vegetación permanencia"),
  mix = c("Nuu ka kaja yunu te ka chi'i itu",
          "Ñu'un yuuku, yuuku ",
          "Nuu ñu'un yuuku, kuu nuu ka jituto",
          "Nuu ñu'un ja tuu kuu yuuku, kuu nuu ñu'un ja ko jituto",
          "Nuu ñu'un nuu ka jituto tuu kuitɨ na yuuni yoó",
          "Ñu'un yuku ja tuu kuɨtɨ yunu iin nuu ñu'unga, chi kaa rɨɨi",
          "Ñu'un ja yoo ñunkuun nuu ka jituto ta'an kuia",
          "Nuu ñu'un ja tuukuitɨ yuunu iin, kaa te'e ii, rɨɨii",
          "Ñu'un nuu tuu kuitɨ nuu yuunu kui yoo nuu ñu'ungua ndɨkɨu ni kaa sua chii kaa rɨɨii"),
  color = c("orange", "#42ab0d", "#f71f2f", "orchid",
            "purple", "gray60", "yellow", "blue", "white")
)

# Opacidad de las capas de cambios
opacidad_cambios <- 0.9

# Pasar colores de R 
a_hex <- function(x) grDevices::rgb(t(grDevices::col2rgb(x)), maxColorValue = 255)
clases$color <- a_hex(clases$color)

# Causas de cambio 
colores_causas <- c(
  "Económico"                    = "#a6cee3",
  "Evento desencadenante social" = "#bd0026",
  "Expansión agrícola"           = "#ffff33",
  "Extensión de infraestructura" = "#984ea3",
  "Revegetación natural"         = "#a65628",
  "Factor demográfico"           = "#f781bf",
  "Valores y creencias"          = "#377eb8"
)

# Puntos: causas de cambio -----
leer_causas <- function(ruta) {
  st_read(ruta, quiet = TRUE) |>
    st_transform(4326) |>
    mutate(Causa = str_squish(Causa))
}

# Español
poly  <- leer_causas("Data/Causas de cus de 2000 a 2013.gpkg")
poly2 <- leer_causas("Data/Causas de cus de 2013 a 2024.gpkg")

# Mixteco
polyM  <- leer_causas("Data/Causas de Cus de 2000-2013 y 2013-2024 en mixteco/Causas de Cus 2000 a 2013 en mixteco.gpkg")
poly2M <- leer_causas("Data/Causas de Cus de 2000-2013 y 2013-2024 en mixteco/Causas de Cus 2013 a 2024 en mixteco.gpkg")

# Unir capas de dos idiomas (Causa.x = español, Causa.y = mixteco)
poly <- poly |>
  left_join(polyM |>
              st_drop_geometry(),
            by = "Número")
poly2 <- poly2 |>
  mutate(across(`Número`, ~as.numeric(.x))) |>
  left_join(poly2M |>
              mutate(across(`Número`, ~as.numeric(.x))) |>
              st_drop_geometry(),
            by = "Número")

# Nombres en mixteco de cada causa
causas_mix <- bind_rows(st_drop_geometry(poly)  |> select(Causa.x, Causa.y),
                        st_drop_geometry(poly2) |> select(Causa.x, Causa.y)) |>
  distinct(Causa.x, Causa.y) |>
  filter(!is.na(Causa.y)) |>
  group_by(Causa.x) |>
  slice(1) |>
  ungroup() |>
  deframe()

# Cerros
cerros <- st_read("Data/Cerros o montes de Chalcatongo en mixteco/Cerros o montes de Chalcatongo en mixteco.gpkg")|>
  st_transform(4326) |>
  separate(paraje,
           into = c("es","mix"),
           sep = ' "') |>
  mutate(across(mix, ~str_remove_all(.x, '"')))

# Agregar ícono de cerros
myIcons <- icons(
  iconUrl = "img/peak.png",
  iconWidth = 15,
  iconHeight = 15,
  iconAnchorX = 7,
  iconAnchorY = 7,
  className = "Cerros"
)

# Popups bilingües -----
# Causas de cambios
popup_causa <- function(datos) {
  paste0(
    '<div style="font-size:13px; min-width:170px;">',
    '<div class="bi-es">',
    '<b style="font-size:14px;">', 'Causa de cambio', '</b><br>',
    datos$Causa.espe.x,
    '</div>',
    '<div class="bi-mix">',
    '<b style="font-size:14px;">', "Non kuu ja ni sa'a ja ni na saama", '</b><br>',
    datos$Causa.espe.y,
    '</div>',
    '</div>'
  )
}
# Cerros
popups_cerros <- paste0(
  '<div style="font-size:13px; min-width:170px;">',
  '<div class="bi-es">',
  '<b style="font-size:14px;">', 'Cerro', '</b><br>',
  cerros$es,
  '</div>',
  '<div class="bi-mix">',
  '<b style="font-size:14px;">', 'Yuku ', '</b><br>',
  cerros$mix,
  '</div>',
  '</div>'
)

# Periodos -----
# Generar prendido/apagado comunes para cada periodo
periodos <- list(
  list(id = "p1",
       es = "2000 a 2013",
       mix = "Kuia uxi xiko ji kuia oko uxi uni",
       raster = im,
       puntos = poly,
       visible = TRUE),
  list(id = "p2",
       es = "2013 a 2024",
       mix = "Kuia oko uxi uni ji kuia oko oko kuun",
       raster = im2,
       puntos = poly2,
       visible = FALSE)   # se enciende desde el control de capas
)

subsecciones <- list(
  cambio = list(es = "Cambios",         mix = "Ni na saama"),
  causa  = list(es = "Causas de cambio", mix = "Non kuu ja ni sa'a ja ni na saama")
)

# Etiqueta HTML bilingüe para el control de capas (con muestra de color)
etiqueta <- function(es, mix, color = NULL, forma = "cuadro") {
  sw <- if (is.null(color)) "" else
    sprintf("<i class='sw sw-%s' style='background:%s'></i>", forma, color)
  paste0(sw,
         "<span class='txt'>",
         "<span class='bi-es'>", htmlEscape(es), "</span>",
         "<span class='bi-mix'>", htmlEscape(mix), "</span>",
         "</span>")
}

# Marcador pequeño tipo pin (SVG): blanco con un círculo del color de la causa
tam_marcador <- 25   # ancho en px (el original de awesome markers mide 35)

icono_pin <- function(color, ancho = tam_marcador) {
  alto <- round(ancho * 34 / 24)
  svg <- paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' width='24' height='34' viewBox='0 0 24 34'>",
    "<path d='M12 1C5.9 1 1 5.9 1 12c0 8.3 11 21 11 21s11-12.7 11-21C23 5.9 18.1 1 12 1z' ",
    "fill='white' stroke='#555' stroke-width='1.3'/>",
    "<circle cx='12' cy='12' r='6.5' fill='", color,
    "' stroke='rgba(0,0,0,.35)' stroke-width='1'/></svg>")
  leaflet::makeIcon(
    iconUrl = base64enc::dataURI(charToRaw(svg), mime = "image/svg+xml"),
    iconWidth = ancho, iconHeight = alto,
    iconAnchorX = ancho / 2, iconAnchorY = alto,
    popupAnchorX = 0, popupAnchorY = -alto
  )
}

# Etiqueta de la capa de cambios: nombre + leyenda de las clases debajo
etiqueta_cambios <- function(es, mix) {
  filas <- paste0(
    "<div class='leg-row'>",
    "<i class='sw sw-cuadro' style='background:", clases$color, "'></i>",
    "<span><span class='bi-es'>", htmlEscape(clases$es), "</span>",
    "<span class='bi-mix'>", htmlEscape(clases$mix), "</span></span>",
    "</div>",
    collapse = "")
  paste0("<span class='txt'>",
         "<span class='bi-es'>", htmlEscape(es), "</span>",
         "<span class='bi-mix'>", htmlEscape(mix), "</span>",
         "<div class='mini-leg'>", filas, "</div>",
         "</span>")
}

# Registro de capas: id del grupo = etiqueta, periodo y subsección
capas <- tibble(id = character(), label = character(),
                periodo = character(), seccion = character())

# Crear leaflet -----
mapa <- leaflet::leaflet(options = leafletOptions(zoomControl = FALSE))

# Mapas base
mapas_base <- c("Esri.WorldTopoMap", "Esri.WorldImagery")
for (provider in mapas_base) {
  mapa <- mapa |>
    leaflet::addProviderTiles(provider, group = provider)
}

# ciclo por periodos
for (p in periodos) {
  
  ## Raster: una sola capa con todos los tipos de cambio del periodo
  r <- terra::as.int(p$raster)
  gid <- paste0("cambio_", p$id)

  mapa <- mapa |>
    leaflet::addRasterImage(
      raster::raster(r),
      colors = leaflet::colorFactor(clases$color, domain = clases$ID,
                                    na.color = "#FFFFFF00"),
      method = "ngb",
      opacity = opacidad_cambios,
      group = gid
    )

  capas <- add_row(capas, id = gid,
                   label = etiqueta_cambios(subsecciones$cambio$es,
                                            subsecciones$cambio$mix),
                   periodo = p$id, seccion = "cambio")

  ## Puntos: una capa por causa de cambio, color según la causa
  for (k in seq_along(colores_causas)) {
    causa <- names(colores_causas)[k]
    sub <- p$puntos |> filter(Causa.x == causa)
    if (nrow(sub) == 0) next
    
    gid <- paste0("causa_", p$id, "_", k)
    icono <- icono_pin(colores_causas[[k]])
    
    mapa <- mapa |>
      leaflet::addMarkers(data = sub,
                          icon = icono,
                          popup = popup_causa(sub),
                          group = gid)
    
    mix <- causas_mix[causa]
    if (is.na(mix)) mix <- causa
    
    capas <- add_row(capas, id = gid,
                     label = etiqueta(causa, mix, colores_causas[[k]], "circulo"),
                     periodo = p$id, seccion = "causa")
  }
}

# Cerros
mapa <- mapa |>
  leaflet::addMarkers(data = cerros,
                      popup = popups_cerros,
                      group = "Cerros",
                      icon = myIcons)

# Ocultar al inicio los periodos con visible = FALSE
ocultas <- capas$id[capas$periodo %in%
                      map_chr(keep(periodos, ~!.x$visible), "id")]
if (length(ocultas) > 0) mapa <- mapa |> leaflet::hideGroup(ocultas)

# Datos para el control de capas agrupado -----
etiquetas <- c(
  setNames(capas$label, capas$id),
  "Esri.WorldTopoMap" = etiqueta("ESRI Topografía", "ESRI Topografía"),
  "Esri.WorldImagery" = etiqueta("ESRI RGB Satelital", "ESRI RGB Satelital"),
  "Cerros" = "<img src='img/peak.png' class='sw-img'>" |>
    paste0(etiqueta("Cerros", "Yuku"))
)

datos_control <- list(
  labels = as.list(etiquetas),
  sections = setNames(
    pmap(capas[, c("periodo", "seccion")], ~list(p = ..1, s = ..2)),
    capas$id),
  periods = setNames(
    map(periodos, ~paste0("<span class='bi-es'>", .x$es, "</span>",
                          "<span class='bi-mix'>", .x$mix, "</span>")),
    map_chr(periodos, "id")),
  subs = map(subsecciones["causa"], ~paste0("<span class='bi-es'>", .x$es, "</span>",
                                   "<span class='bi-mix'>", .x$mix, "</span>"))
)

# Agregar cerros en los controles de capas
peak_uri <- base64enc::dataURI(file = "img/peak.png", mime = "image/png")
datos_control$labels$Cerros <- sub("img/peak.png", peak_uri,
                                   datos_control$labels$Cerros, fixed = TRUE)

# JS: etiquetas bilingües con muestras de color, encabezados por periodo
# y subsección con casilla para prender/apagar todo el grupo
js_control <- r"---(
function(el, x, data) {
  var map = this;
  var ctrl = map.currentLayersControl;
  var headers = [];   // {box: checkbox, inputs: [...]}

  function inputOf(span) {
    var row = span.parentNode;
    return row ? row.querySelector('input') : null;
  }

  // Si falta alguna capa por prender, prende todas; si no, apaga todas
  function setAll(inputs, on) {
    inputs.forEach(function(i) { if (i.checked !== on) i.click(); });
  }

  // Estado de las casillas de grupo
  function refreshHeaders() {
    headers.forEach(function(h) {
      var n = h.inputs.filter(function(i) { return i.checked; }).length;
      h.box.checked = n > 0 && n === h.inputs.length;
      h.box.indeterminate = n > 0 && n < h.inputs.length;
    });
  }

  function makeHeader(html, cls, inputs) {
    var lab = document.createElement('label');
    lab.className = 'lyr-head ' + cls;
    var row = document.createElement('div');
    var box = document.createElement('input');
    box.type = 'checkbox';
    box.className = 'leaflet-control-layers-selector lyr-group-box';
    var txt = document.createElement('span');
    txt.className = 'lyr-head-txt';
    txt.innerHTML = html;
    row.appendChild(box); row.appendChild(txt); lab.appendChild(row);
    box.addEventListener('click', function(e) {
      e.stopPropagation();
      setAll(inputs, box.checked);
      refreshHeaders();
    });
    headers.push({box: box, inputs: inputs});
    return lab;
  }

  function decorate() {
    el.querySelectorAll('.lyr-head').forEach(function(h) { h.remove(); });
    headers = [];

    var spans = el.querySelectorAll(
      '.leaflet-control-layers-base label span, .leaflet-control-layers-overlays label span'
    );
    var rows = [];
    spans.forEach(function(span) {
      if (span.closest('.txt') || span.classList.contains('txt')) return;
      var id = span.getAttribute('data-gid') || span.textContent.trim();
      if (!data.labels[id]) return;
      span.setAttribute('data-gid', id);
      span.classList.add('lyr');
      span.innerHTML = data.labels[id];
      var sec = data.sections[id];
      if (sec) {
        span.closest('label').classList.add('lyr-in');
        rows.push({id: id, sec: sec, label: span.closest('label'), input: inputOf(span)});
      }
    });

    // Agrupar inputs por periodo y por periodo+subsección
    var byP = {}, byPS = {};
    rows.forEach(function(r) {
      (byP[r.sec.p] = byP[r.sec.p] || []).push(r.input);
      var k = r.sec.p + '|' + r.sec.s;
      (byPS[k] = byPS[k] || []).push(r.input);
    });

    var lastP = null, lastS = null;
    rows.forEach(function(r) {
      var parent = r.label.parentNode;
      if (r.sec.p !== lastP) {
        parent.insertBefore(makeHeader(data.periods[r.sec.p], 'lyr-head-p', byP[r.sec.p]), r.label);
        lastP = r.sec.p; lastS = null;
      }
      if (r.sec.s !== lastS) {
        var k = r.sec.p + '|' + r.sec.s;
        if (data.subs[r.sec.s]) {
          parent.insertBefore(makeHeader(data.subs[r.sec.s], 'lyr-head-s', byPS[k]), r.label);
        }
        lastS = r.sec.s;
      }
    });
    refreshHeaders();
  }

  // Actualizar casillas de grupo cuando se prende/apaga una capa suelta
  map.on('overlayadd overlayremove', refreshHeaders);
  el.addEventListener('change', function() { setTimeout(refreshHeaders, 0); });

  // Leaflet reconstruye la lista al cambiar capas desde fuera: re-decorar
  if (ctrl && ctrl._update) {
    var orig = ctrl._update;
    ctrl._update = function() {
      var res = orig.apply(this, arguments);
      decorate();
      return res;
    };
  }
  decorate();
}
)---"

# Botón de idioma y estilos -----
lang_btn <- tags$button(
  id      = "lang-btn",
  onclick = "toggleLang()",
  style   = "background:white; border:2px solid #444; border-radius:6px;
             padding:5px 13px; font-size:13px; font-weight:bold;
             cursor:pointer; box-shadow:0 1px 5px rgba(0,0,0,.4);",
  tags$div(class = "bi-es", "\U0001F310  Español"),
  tags$div(class = "bi-mix", "\U0001F310  Tu’un Savi")
)

# Título del mapa
titulo <- tags$div(
  id = "map-title",
  tags$div(class = "bi-es", "Mapeo del cambio de cubierta y uso de suelo para el atlas interactivo del municipio de Chalcatongo de Hidalgo, Tlaxiaco, Oaxaca"),
  # Falta la traducción al tu'un savi; por el momento dejar el título en español
  tags$div(class = "bi-mix", "Tutu nuu kastuu jaa na sama jaa yoo nuu ñu'un ñuuyo Ñuu Ndeya, Ndi jinuu, Nundua")
)

# Logos. Incrustar logos institucionales 
logos <- c("Data/logo_unicha.png", "Data/logo_unam.png", "Data/logo_ciga.png")
logos_ctrl <- tags$div(
  id = "map-logos",
  lapply(logos, function(f) {
    tags$img(src = base64enc::dataURI(file = f, mime = "image/png"))
  })
)

lang_css <- tags$style(HTML("
  /* ── Idioma ─────────────────────────────────────────────────────────── */
  .bi-mix { display: none;  }
  .bi-es  { display: block; }
  body.lang-mix .bi-es  { display: none;  }
  body.lang-mix .bi-mix { display: block; }
  #lang-btn:hover { background: #f0f0f0 !important; }

  /* ── Logos ──────────────────────────────────────────────────────────── */
  #map-logos {
    position: fixed; bottom: 40px; right: 10px;
    z-index: 1001; display: flex; align-items: center; gap: 14px;
    background: rgba(255,255,255,.92); padding: 6px 14px; border-radius: 6px;
    box-shadow: 0 1px 5px rgba(0,0,0,.4);
  }
  #map-logos img { height: 52px; width: auto; display: block; }

  /* ── Título ─────────────────────────────────────────────────────────── */
  #map-title {
    position: fixed; top: 10px; left: 50%; transform: translateX(-50%);
    z-index: 1001; text-align: center;
    width: max-content; max-width: calc(100vw - 700px); min-width: 280px;
    background: rgba(255,255,255,.92); padding: 8px 16px; border-radius: 6px;
    box-shadow: 0 1px 5px rgba(0,0,0,.4);
    font-family: 'Helvetica Neue', Arial, Helvetica, sans-serif;
    font-size: 15px; font-weight: bold; line-height: 1.3; color: #333;
  }

  /* ── Control de capas agrupado ──────────────────────────────────────── */
  .leaflet-control-layers-expanded {
    max-height: calc(100vh - 230px);   /* deja espacio para los logos */
    overflow-y: auto;
    max-width: 320px;
  }
  .leaflet-control-layers label > div {
    display: flex; align-items: center;
  }
  .leaflet-control-layers label span.lyr {
    display: flex; align-items: center; flex: 1 1 auto; min-width: 0;
  }
  .leaflet-control-layers label span.txt { line-height: 1.2; }
  .leaflet-control-layers label.lyr-in { margin-left: 14px; }
  .sw {
    display: inline-block; flex: 0 0 auto;
    width: 14px; height: 12px; margin: 0 6px 0 3px;
    border: 1px solid rgba(0,0,0,.35); border-radius: 2px;
  }
  .sw-circulo { width: 12px; height: 12px; border-radius: 50%; }
  .sw-img { width: 14px; height: 14px; margin: 0 6px 0 3px; flex: 0 0 auto; }
  .leaflet-control-layers label.lyr-head > div { display: flex; align-items: center; }
  .leaflet-control-layers label > div:has(.mini-leg) { align-items: flex-start; }
  .leaflet-control-layers label > div:has(.mini-leg) > input { margin-top: 2px; }
  .mini-leg { margin: 3px 0 4px; font-size: 11px; font-weight: normal; }
  .leg-row { display: flex; align-items: center; margin: 1px 0; line-height: 1.15; }
  .leg-row .sw { width: 12px; height: 10px; margin-left: 0; }
  .lyr-head { cursor: pointer; user-select: none; }
  .lyr-head-txt { margin-left: 4px; }
  .lyr-head-p {
    font-weight: bold; font-size: 13px;
    margin: 8px 0 2px; padding-top: 6px; border-top: 1px solid #ccc;
  }
  .lyr-head-s {
    font-style: italic; font-size: 12px; color: #444;
    margin: 4px 0 2px 6px;
  }
"))

lang_js <- tags$script(HTML("
  function toggleLang() {
    document.body.classList.toggle('lang-mix');
  }
"))

# Controles -----
mapa <- mapa |>
  addControl(lang_btn, position = "topright") |>
  leaflet::addLayersControl(
    overlayGroups = c("Cerros", capas$id),
    baseGroups = mapas_base,
    options = leaflet::layersControlOptions(collapsed = FALSE,
                                            hideSingleBase = TRUE)) |>
  htmlwidgets::onRender(js_control, data = datos_control) |>
  leaflet::addMiniMap(tiles = mapas_base[[1]],
                      toggleDisplay = TRUE,
                      position = "bottomleft")

mapa <- mapa |>
  htmlwidgets::onRender("
    function(el, x) {
      L.control.zoom({ position: 'topleft' }).addTo(this);
    }
  ") |>
  # Actualizar mini mapa al cambiar el mapa base
  htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }") |>
  leaflet::addEasyButtonBar(
    leaflet::easyButton(
      icon = "fa-crosshairs", title = "Ubícame",
      onClick = leaflet::JS("function(btn, map){ map.locate({setView: true});}")),
    leaflet::easyButton(
      icon = "fa-globe",
      title = "Zoom a Chalcatongo",
      onClick = leaflet::JS("function(btn, map){ map.fitBounds([
                                        [16.92588, -97.63843],
                                        [17.06979, -97.46379]
                                        ]); }"))) |>
  leaflet::addScaleBar(position = "bottomleft",
                       options = leaflet::scaleBarOptions(metric = TRUE,
                                                          imperial = FALSE))

# prependContent pone el CSS antes del widget; appendContent el JS al final
mapa <- mapa |>
  htmlwidgets::prependContent(lang_css, titulo, logos_ctrl) |>
  htmlwidgets::appendContent(lang_js)

mapa

# Exportar a html----
if (!dir.exists("Map")) {
  dir.create("Map", recursive = TRUE)
}
htmlwidgets::saveWidget(mapa,
                        "Map/index.html",
                        selfcontained = TRUE)
