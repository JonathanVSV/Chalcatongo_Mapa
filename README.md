# Chalcatongo_Mapa

Mapeo del cambio de cubierta y uso de suelo para el atlas interactivo del municipio de Chalcatongo de Hidalgo, Tlaxiaco, Oaxaca en  los periodos 2000 - 2013, 2013 - 2024.

## Descripción del trabajo

Información obtenida mediante un enfoque postclasificatorio. Las clasificaciones de 2000, 2013 y 2024 se obtuvieron mediante una clasificación supervisada, usando random forest, sobre mosaicos anuales sin nubes de imágenes Landsat. Toda la cartografía fue generada por Fernando Jiménez.

## Descripción del repositorio

La carpeta **Data** contiene los datos espaciales utilizados para generar el mapa interactivo, es decir, las causas de los cambios obtenidas mediante un ejercicio de mapeo participativo con varias agencias de la zona de estudio, así como las clasificaciones de cambios y permanencias en los dos periodos evaluados (2000-2013 y 2013-2024). También contiene los logos utilizados en el mapa y el símbolo de los cerros.

El archivo **index** contiene el archivo html del mapa interactivo.

La carpeta **index_files** contiene archivos auxiliares generados por leaflet para mostrar de manera adecuada el mapa interactivo.

El archivo **interactivemap_builder** contiene el código de R con el que se construyó el mapa de Leaflet. 

## Mapa interactivo

Consulta del mapa interactivo: [https://jonathanvsv.github.io/Chalcatongo_Mapa/](https://jonathanvsv.github.io/Chalcatongo_Mapa/)