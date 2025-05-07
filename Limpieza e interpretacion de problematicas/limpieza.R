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
municipios=read_sf("../../../Reutilizables/Cartografia/conjunto_de_datos/13mun.shp") |> st_transform(st_crs("EPSG:4326"))
Encoding(municipios$NOMGEO) ="latin1"

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
  print(tema_seleccionado)
  base_filtrada=data_inicial |> 
    dplyr::select(CVEGEO,`Sector--Municipio`,dplyr::matches(paste0(tema_seleccionado,"--")))
  print(colnames(base_filtrada))
  numero_de_indicadores=ncol(base_filtrada)-2
  base_filtrada_c_rank=base_filtrada 
  base_filtrada_c_rank[,3:ncol(base_filtrada_c_rank)]=lapply(
    base_filtrada_c_rank[,3:ncol(base_filtrada_c_rank)],
    FUN=\(x) {
      print(x)
      z=rank(abs(x |> as.numeric()),na.last = "keep",ties.method = "average")
      max_z=max(z,na.rm = T)
      z=max_z-z+1
      z=84-z+1
      return(z)
      }
  )
  base_filtrada_c_rank$unidimensional=base_filtrada_c_rank |> 
    dplyr::select(-c(CVEGEO,`Sector--Municipio`))|> rowSums(na.rm = T)
  paleta_dado_tema=colorNumeric(palette = "Reds",domain =base_filtrada_c_rank$unidimensional[base_filtrada_c_rank$unidimensional>0],na.color = "#cdcdcd")  
  return(base_filtrada_c_rank$unidimensional)
}
corteDeColores=function(zz,corte=0.1){
  return(lapply(zz,FUN=\(x) {if(x/max(zz)>0.1){return(x/max(zz))}else{return(0.1)}}) |> unlist())
}
generadorPopUp=function(tema=1,data_inicial=data_inicial){
  posibles_temas=unique(copia_colnames)[3:9]
  tema_seleccionado=posibles_temas[tema]
  print(tema_seleccionado)
  base_filtrada=data_inicial |> 
    dplyr::select(CVEGEO,`Sector--Municipio`,dplyr::matches(paste0(tema_seleccionado,"--")))
  zz=list()
  zz=append(zz,)
   |> dplyr::select(-CVEGEO) |> 
    lapply(FUN=\(x){
      zz=append(zz,
                names(x)
                )
      print(names(x))
    })
  return(zz)
  #La idea es meter en una lista los datos que se mostrarán en cada renglón de la tarjeta.
  #E.g.
    #Municipio: mun_i
    #indice_1: valor_1
        #...
    #indice_n: valor_n
  
  # paste(
  #   "<b>Municipio: </b>", htmlEscape(turismo_shp$NOM_MUN), "<br>",
  #   ifelse(!is.na(turismo_shp$empresas_a), 
  #          paste0("<b>Empresas adheridas al código CCN: </b>",turismo_shp$empresas_a),
  #          ""),
  #   ifelse(!is.na(turismo_shp$incidentes_total), 
  #          paste0("<table style='border-collapse: collapse; width: 100%; font-size: 12px;'>",
  #                 "<tr><th style='border: 1px solid black; padding: 4px; text-align: center;'>Tipo de Delito</th>",
  #                 "<th style='border: 1px solid black; padding: 4px; text-align: center;'>Cantidad</th></tr>"),
  #          ""),
  #   
  #   
  #   # Para cada delito, solo lo mostramos si el valor es mayor a 0
  #   ifelse(turismo_shp$`Explotación de la prostitución ajena` > 0, 
  #          paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>Explotación de la prostitución ajena</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", turismo_shp$`Explotación de la prostitución ajena`, "</td></tr>"),
  #          ""),
  #   
  #   ifelse(turismo_shp$`Explotación laboral` > 0, 
  #          paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>Explotación laboral</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", turismo_shp$`Explotación laboral`, "</td></tr>"),
  #          ""),
  #   
  #   ifelse(turismo_shp$`Circulación de pornografía` > 0, 
  #          paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>Circulación de pornografía</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", turismo_shp$`Circulación de pornografía`, "</td></tr>"),
  #          ""),
  #   
  #   ifelse(turismo_shp$`Utilización de menores de 18 años en hechos delictivos` > 0, 
  #          paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>Utilización de menores de 18 años en hechos delictivos</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", turismo_shp$`Utilización de menores de 18 años en hechos delictivos`, "</td></tr>"),
  #          ""),
  #   
  #   ifelse(turismo_shp$`Otras formas de explotación sexual` > 0, 
  #          paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>Otras formas de explotación sexual</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", turismo_shp$`Otras formas de explotación sexual`, "</td></tr>"),
  #          ""),
  #   
  #   ifelse(turismo_shp$Esclavitud > 0, 
  #          paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>Esclavitud</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", turismo_shp$Esclavitud, "</td></tr>"),
  #          ""),
  #   ifelse(!is.na(turismo_shp$incidentes_total), 
  #          paste0("<tr><th style='border: 1px solid black; padding: 4px; text-align: center;'>Total</th><td style='border: 1px solid black; padding: 4px; text-align: center;'>", turismo_shp$incidentes_total, "</td></tr>"),
  #          ""),
  #   
  #   
  #   "</table>"
  # ) |>
  #   sapply(FUN = function(x) HTML(x), USE.NAMES = F) |>
  #   `class<-`("html") |> return()
  #labelOptions = labelOptions(textsize = 14, direction = "top")
}
generadorPopUp(tema = 1)
corteDeColores(generarPaleta(tema = 7))
zz=corteDeColores(generarPaleta(tema = 2))
zz
mapa_web=leaflet() |> 
  addTiles() |> 
  addPolygons(data=municipios |> as("Spatial"),group = "municipios_base",label = municipios$NOMGEO,popup = "QAAAA",color = "white",fillColor = "gray",weight = 2,fillOpacity = 0.3) |> 
  addPolygons(data=merge(municipios |> dplyr::select(CVEGEO),data_inicial |> dplyr::select(CVEGEO,`Sector--Municipio`,`Desarrollo.económico--PIB per capita`,`Desarrollo.económico--PIB Turístico per capita`,`Desarrollo.económico--Empleos formales generados ante el Imss`),by='CVEGEO') |> 
                dplyr::filter(`Desarrollo.económico--PIB per capita`!='' | `Desarrollo.económico--Empleos formales generados ante el Imss`!='' | `Desarrollo.económico--Empleos formales generados ante el Imss`!='' ),group = "Desarrollo Económico",fillColor  = "red",fillOpacity = corteDeColores(generarPaleta(tema = 1)) ,color = "white") |> 
  addPolygons(data=merge(municipios |> dplyr::select(CVEGEO),data_inicial |> dplyr::select(CVEGEO,`Pobreza.y.carencias.2010.2020--Rezago Educativo`:`Pobreza.y.carencias.2010.2020--Carencia por acceso a los servicios de salud`),by='CVEGEO') |> 
                dplyr::filter_at(vars(-CVEGEO,-geometry), any_vars(.!='')),group = "Pobreza y Carencias",fillColor  = "red",fillOpacity = corteDeColores(generarPaleta(tema = 2)) ,color = "white",) |> 
  addPolygons(data=merge(municipios |> dplyr::select(CVEGEO),data_inicial |> dplyr::select(CVEGEO,`Salud--Camas  por cada mil habitantes`:`Salud--Consultorios por cada mil habitantes`),by='CVEGEO') |> 
                dplyr::filter_at(vars(-CVEGEO,-geometry), any_vars(.!='')),group = "Salud",fillColor  = "red",fillOpacity = corteDeColores(generarPaleta(tema = 3)) ,color = "white",) |> 
  addPolygons(data=merge(municipios |> dplyr::select(CVEGEO),data_inicial |> dplyr::select(CVEGEO,`Educación--Grado promedio de escolaridad`:`Educación--% Analfabetismo`),by='CVEGEO') |> 
                dplyr::filter_at(vars(-CVEGEO,-geometry), any_vars(.!='')),group = "Educación",fillColor  = "red",fillOpacity = corteDeColores(generarPaleta(tema = 4)) ,color = "white",) |> 
  addPolygons(data=merge(municipios |> dplyr::select(CVEGEO),data_inicial |> dplyr::select(CVEGEO,`Medio.Ambiente--Plantas Tratadoras de Aguas residuales por cada mil habitantes`:`Medio.Ambiente--Volumen tratado en las PTARs  (Millones m3)`),by='CVEGEO') |> 
                dplyr::filter_at(vars(-CVEGEO,-geometry), any_vars(.!='')),group = "Medio Ambiente",fillColor  = "red",fillOpacity = corteDeColores(generarPaleta(tema = 5)) ,color = "white",) |> 
  addPolygons(data=merge(municipios |> dplyr::select(CVEGEO),data_inicial |> dplyr::select(CVEGEO,`Seguridad.Pública--Incidencia Delictiva del Fuero Común`:`Seguridad.Pública--Robo a casa habitación`),by='CVEGEO') |> 
                dplyr::filter_at(vars(-CVEGEO,-geometry), any_vars(.!='')),group = "Seguridad Pública",fillColor  = "red",fillOpacity = corteDeColores(generarPaleta(tema = 6)) ,color = "white") |> 
  addPolygons(data=merge(municipios |> dplyr::select(CVEGEO),data_inicial |> dplyr::select(CVEGEO,`Vivienda--Hacinamiento`:`Vivienda--% Viviendas con menor disponibilidad de computadora`),by='CVEGEO') |> 
                dplyr::filter_at(vars(-CVEGEO,-geometry), any_vars(.!='')),group = "Vivienda",fillColor  = "red",fillOpacity = corteDeColores(generarPaleta(tema = 7)) ,color = "white") |> 
  addSearchFeatures(targetGroups = "municipios_base",options = searchFeaturesOptions(zoom=12,openPopup=F,hideMarkerOnCollapse=T)) |> 
  
  addLayersControl(baseGroups = c("Desarrollo Económico","Pobreza y Carencias","Salud","Educación","Medio Ambiente","Seguridad Pública","Vivienda"),options = layersControlOptions(collapsed = F)) 
  

mapa_web

