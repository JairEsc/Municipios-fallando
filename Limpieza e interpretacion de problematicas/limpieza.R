data_inicial=read.csv("../Indicadores Municipales de Tendencia Negativa.xlsx - Hoja1.csv")
copia_colnames=colnames(data_inicial)
for(name_i in 2:length(colnames(data_inicial))){
  name=colnames(data_inicial)[name_i]
  if(nchar(name)>4){
    nombre_actual=name
  }else{
    copia_colnames[name_i]=nombre_actual
  }
}
colnames(data_inicial)=paste0(copia_colnames,"--",data_inicial[1,])
data_inicial=data_inicial[-1,]
colnames(data_inicial)[1]="CVEGEO"
source("../../../ASUS Gamer Jair/codigos/puras_librerias.R")
library(sf)
municipios=read_sf("../../../Reutilizables/Cartografia/conjunto_de_datos/13mun.shp") |> st_transform(st_crs("EPSG:4326"))
Encoding(municipios$NOMGEO) ="latin1"
municipios=municipios[order(municipios$CVEGEO),]
data_inicial=data_inicial |> 
  dplyr::mutate(`Pobreza.y.carencias.2010.2020--Índice de rezago social\n2010-2020`=
                  ifelse(`Pobreza.y.carencias.2010.2020--Índice de rezago social\n2010-2020`=='Alto',"1",""))
data_inicial=data_inicial |> 
  dplyr::mutate(`Pobreza.y.carencias.2010.2020--índice de marginación\n2010-2020`=
                  ifelse(`Pobreza.y.carencias.2010.2020--índice de marginación\n2010-2020`=='Muy Alto',"2",ifelse(`Pobreza.y.carencias.2010.2020--índice de marginación\n2010-2020`=='Alto',"1","")))

data_inicial$`Salud--Personal médico por cada mil habitantes`=gsub(pattern = "\\.",replacement = "",data_inicial$`Salud--Personal médico por cada mil habitantes`)
data_inicial$`Medio.Ambiente--Toneladas de basura generadas por cada mil habitantes`=gsub(pattern = "\\,",replacement = "",data_inicial$`Medio.Ambiente--Toneladas de basura generadas por cada mil habitantes`)


### Función para general la paleta de colores de cada tema ###
generarPaleta=function(tema=1,base=data_inicial){
  posibles_temas=unique(copia_colnames)[3:9]
  tema_seleccionado=posibles_temas[tema]
  #print(tema_seleccionado)
  base_filtrada=data_inicial |> 
    dplyr::select(CVEGEO,`Sector--Municipio`,dplyr::matches(paste0(tema_seleccionado,"--")))
  #print(colnames(base_filtrada))
  numero_de_indicadores=ncol(base_filtrada)-2
  base_filtrada_c_rank=base_filtrada 
  base_filtrada_c_rank[,3:ncol(base_filtrada_c_rank)]=lapply(
    base_filtrada_c_rank[,3:ncol(base_filtrada_c_rank)],
    FUN=\(x) {
      #print(x)
      z=rank(abs(x |> as.numeric()),na.last = "keep",ties.method = "average")
      max_z=max(z,na.rm = T)
      z=max_z-z+1
      z=84-z+1
      return(z)
      }
  )
  #return(base_filtrada_c_rank)
  base_filtrada_c_rank$unidimensional=base_filtrada_c_rank |> 
    dplyr::select(-c(CVEGEO,`Sector--Municipio`))|> rowSums(na.rm = T)
  #paleta_dado_tema=colorNumeric(palette = "Reds",domain =base_filtrada_c_rank$unidimensional[base_filtrada_c_rank$unidimensional>0],na.color = "#cdcdcd")  
  return(base_filtrada_c_rank$unidimensional)
}
corteDeColores=function(zz,corte=0.1){
  return(lapply(zz,FUN=\(x) {if(x/max(zz)>0.1 & x>0){
    return(x/max(zz))
    }else{
      if(x==0){
        return(0)
      }else{return(0.1)}
      
      }
    }) |> unlist())
}
generadorPopUp <- function(tema = 1, data_inicial = data_inicial) {
  posibles_temas <- unique(copia_colnames)[3:9]
  tema_seleccionado <- posibles_temas[tema]
  base_filtrada <- data_inicial |>
    dplyr::select(CVEGEO, `Sector--Municipio`, dplyr::matches(paste0(tema_seleccionado, "--")))
  # CSS para cada popup
  css_popup <- "<style>
    .card {
      background-color: #ffffff;
      border-radius: 12px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.2);
      padding: 16px;
      font-family: 'Segoe UI', Tahoma, sans-serif;
      max-width: 280px;
    }
    .card h2 {
      font-size: 1.1em;
      margin-bottom: 12px;
      color: #d4c19c;
    }
    .indicator {
      margin-bottom: 8px;
      padding: 6px;
      background-color: #f9fafc;
      border-left: 4px solid #621132;
      border-radius: 4px;
    }
    .indicator span {
      font-weight: bold;
      color: #555;
    }
  </style>"
  z <- vector("character", length = nrow(base_filtrada))
  for (i in seq_len(nrow(base_filtrada))) {
    municipio <- base_filtrada$`Sector--Municipio`[i]
    indicadores <- ""
    for (j in 3:ncol(base_filtrada)) {
      nombre <- names(base_filtrada)[j]
      valor <- base_filtrada[[j]][i]
      if (!is.na(valor) && valor != "") {
        indicadores <- paste0(indicadores,
                              '<div class="indicator"><span>',
                              gsub(paste0(tema_seleccionado, "--"), "", nombre),
                              '</span> ', '</div>')
      }
    }
    z[i] <- paste0(
      css_popup,
      '<div class="card">',
      '<h2>', municipio, '</h2>',
      ifelse(indicadores!='',paste0('<h2 style="color:#1d1d1b">', "Indicadores negativos en el municipio:", '</h2>'),""),
      indicadores,
      '</div>'
    )
  }
  return(z)
}

library(sf)
st_write(capa_de_regiones,"../../../../Jair/Reutilizables/shapes/regional.geojson",driver = "GeoJSON")
capa_de_regiones=sf::read_sf("../../../../Jair/Reutilizables/shapes/Regional_.shp")
library(leaflet.extras2)
library(leaflet.extras)
library(leaflet)
library(htmlwidgets)
library(dplyr)
library(leaflegend)
mapa_web=leaflet() |> 
  addTiles() |> 
  addPolygons(data=municipios |> as("Spatial"),group = "municipios_base",label = municipios$NOMGEO,,color = "white",fillColor = "gray",weight = 2,fillOpacity = 0.3) |> 
  addPolygons(data=merge(municipios |> dplyr::select(CVEGEO),data_inicial |> dplyr::select(CVEGEO,`Sector--Municipio`,`Desarrollo.económico--PIB per capita`,`Desarrollo.económico--PIB Turístico per capita`,`Desarrollo.económico--Empleos formales generados ante el Imss`),by='CVEGEO') 
              # |> dplyr::filter(`Desarrollo.económico--PIB per capita`!='' | `Desarrollo.económico--Empleos formales generados ante el Imss`!='' | `Desarrollo.económico--Empleos formales generados ante el Imss`!='' )
              ,group = "Desarrollo Económico",fillColor  = "red",fillOpacity = corteDeColores(generarPaleta(tema = 1)) ,color = "white", popup =gsub("<br>  <br>","",generadorPopUp(tema = 1,data_inicial = data_inicial))) |> 
  addPolygons(data=merge(municipios |> dplyr::select(CVEGEO),data_inicial |> dplyr::select(CVEGEO,`Pobreza.y.carencias.2010.2020--Rezago Educativo`:`Pobreza.y.carencias.2010.2020--Carencia por acceso a los servicios de salud`),by='CVEGEO') 
              # |> dplyr::filter_at(vars(-CVEGEO,-geometry), any_vars(.!=''))
              ,group = "Pobreza y Carencias",fillColor  = "red",fillOpacity = corteDeColores(generarPaleta(tema = 2)) 
              ,color = "white",popup =gsub("<br>  <br>","",generadorPopUp(tema = 2,data_inicial = data_inicial))) |> 
  addPolygons(data=merge(municipios |> dplyr::select(CVEGEO),data_inicial |> dplyr::select(CVEGEO,`Salud--Camas  por cada mil habitantes`:`Salud--Consultorios por cada mil habitantes`),by='CVEGEO') 
              # |> dplyr::filter_at(vars(-CVEGEO,-geometry), any_vars(.!=''))
              ,group = "Salud",fillColor  = "red",fillOpacity = corteDeColores(generarPaleta(tema = 3)) 
              ,color = "white",popup =gsub("<br>  <br>","",generadorPopUp(tema = 3,data_inicial = data_inicial))) |> 
  addPolygons(data=merge(municipios |> dplyr::select(CVEGEO),data_inicial |> dplyr::select(CVEGEO,`Educación--Grado promedio de escolaridad`:`Educación--% Analfabetismo`),by='CVEGEO') 
              # |> dplyr::filter_at(vars(-CVEGEO,-geometry), any_vars(.!=''))
              ,group = "Educación",fillColor  = "red",fillOpacity = corteDeColores(generarPaleta(tema = 4)) ,color = "white",popup =gsub("<br>  <br>","",generadorPopUp(tema = 4,data_inicial = data_inicial))) |> 
  addPolygons(data=merge(municipios |> dplyr::select(CVEGEO),data_inicial |> dplyr::select(CVEGEO,`Medio.Ambiente--Plantas Tratadoras de Aguas residuales por cada mil habitantes`:`Medio.Ambiente--Volumen tratado en las PTARs  (Millones m3)`),by='CVEGEO') 
              # |> dplyr::filter_at(vars(-CVEGEO,-geometry), any_vars(.!=''))
              ,group = "Medio Ambiente",fillColor  = "red",fillOpacity = corteDeColores(generarPaleta(tema = 5)) ,color = "white",popup =gsub("<br>  <br>","",generadorPopUp(tema = 5,data_inicial = data_inicial))) |> 
  addPolygons(data=merge(municipios |> dplyr::select(CVEGEO),data_inicial |> dplyr::select(CVEGEO,`Seguridad.Pública--Incidencia Delictiva del Fuero Común`:`Seguridad.Pública--Robo a casa habitación`),by='CVEGEO') 
              # |> dplyr::filter_at(vars(-CVEGEO,-geometry), any_vars(.!=''))
              ,group = "Seguridad Pública",fillColor  = "red",fillOpacity = corteDeColores(generarPaleta(tema = 6)) ,color = "white",popup =gsub("<br>  <br>","",generadorPopUp(tema = 6,data_inicial = data_inicial))) |> 
  addPolygons(data=merge(municipios |> dplyr::select(CVEGEO),data_inicial |> dplyr::select(CVEGEO,`Vivienda--Hacinamiento`:`Vivienda--% Viviendas con menor disponibilidad de computadora`),by='CVEGEO') 
              # |> dplyr::filter_at(vars(-CVEGEO,-geometry), any_vars(.!=''))
              ,group = "Vivienda",fillColor  = "red",fillOpacity = corteDeColores(generarPaleta(tema = 7)) ,color = "white",popup =gsub("<br>  <br>","",generadorPopUp(tema = 7,data_inicial = data_inicial))) |> 
  addSearchFeatures(targetGroups = "municipios_base",options = searchFeaturesOptions(zoom=12,openPopup=F,hideMarkerOnCollapse=T)) |> 
  addPolylines(data=capa_de_regiones |> sf::st_cast("LINESTRING") |> as("Spatial"),group = "regional_base",color = "black",weight = 2.5) |> 
  addLayersControl(baseGroups = c("Desarrollo Económico","Pobreza y Carencias","Salud","Educación","Medio Ambiente","Seguridad Pública","Vivienda"),options = layersControlOptions(collapsed = F,autoZIndex = T)) |> 
  onRender("
    function() {
      var map = this;
      function bringRegionalToFront() {
        map.eachLayer(function(layer) {
          if (layer.options && layer.options.group === 'regional_base') {
            if (layer.bringToFront) {
              layer.bringToFront();
            }
          }
        });
        
        let control_info=document.getElementsByClassName('info legend leaflet-control');
        control_info[0].style.backgroundColor='darkgray';
      }

      bringRegionalToFront();

      map.on('baselayerchange', function(e) {
        bringRegionalToFront();
      });
    }
  ") |> 
  addLegendImage(
    images = c("imagenes/blanco.png", "imagenes/negro.png"),
    labels = c("Municipios", "Regiones"),
    width = 30, height = 30,
    position = 'bottomleft',
    labelStyle = "font-size:14px"
  ) |> 
  addLegend(
    position = "bottomright",
    pal = colorFactor(palette = c("red","#ff7f7f"),reverse = T,domain = c("Alta Prioridad","Muy Alta Prioridad"), na.color = "#808080"),opacity = 1,
    values = c("Muy Alta Prioridad","Alta Prioridad","Sin prioridad")
  ) 

mapa_web |> htmlwidgets::saveWidget("problematicas_municipios.html",title = "Municipios en Prioridad")


