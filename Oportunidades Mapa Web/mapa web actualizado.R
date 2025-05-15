shape_oportunidades=shape_oportunidades_montse
shape_oportunidades=shape_oportunidades |> 
  dplyr::select(-c(TP1:TP5))
shape_oportunidades=shape_oportunidades |> 
  dplyr::mutate(
    C1=ifelse(C1=='Agua y Crisis Hidrica',"Agua y Crisis Hídrica",C1),
    C2=ifelse(C2=='Agua y Crisis Hidrica',"Agua y Crisis Hídrica",C2),
    C3=ifelse(C3=='Agua y Crisis Hidrica',"Agua y Crisis Hídrica",C3),
    C4=ifelse(C4=='Agua y Crisis Hidrica',"Agua y Crisis Hídrica",C4),
    C5=ifelse(C5=='Agua y Crisis Hidrica',"Agua y Crisis Hídrica",C5),
    
    
    C1=ifelse(C1=='Gestion territorial y desarrollo urbano',"Gestión territorial y desarrollo urbano",C1),
    C2=ifelse(C2=='Gestion territorial y desarrollo urbano',"Gestión territorial y desarrollo urbano",C2),
    C3=ifelse(C3=='Gestion territorial y desarrollo urbano',"Gestión territorial y desarrollo urbano",C3),
    C4=ifelse(C4=='Gestion territorial y desarrollo urbano',"Gestión territorial y desarrollo urbano",C4),
    C5=ifelse(C5=='Gestion territorial y desarrollo urbano',"Gestión territorial y desarrollo urbano",C5),
    
    
    C1=ifelse(C1=='Problemas sociales',"Problemas Sociales",C1),
    C2=ifelse(C2=='Problemas sociales',"Problemas Sociales",C2),
    C3=ifelse(C3=='Problemas sociales',"Problemas Sociales",C3),
    C4=ifelse(C4=='Problemas sociales',"Problemas Sociales",C4),
    C5=ifelse(C5=='Problemas sociales',"Problemas Sociales",C5),
    
    
    C1=ifelse(C1=='Seguridad y corrupcion',"Seguridad y corrupción",C1),
    C2=ifelse(C2=='Seguridad y corrupcion',"Seguridad y corrupción",C2),
    C3=ifelse(C3=='Seguridad y corrupcion',"Seguridad y corrupción",C3),
    C4=ifelse(C4=='Seguridad y corrupcion',"Seguridad y corrupción",C4),
    C5=ifelse(C5=='Seguridad y corrupcion',"Seguridad y corrupción",C5),
    
    )

source("../../../ASUS Gamer Jair/codigos/puras_librerias.R")
shape_oportunidades=shape_oportunidades|>
  st_transform(st_crs("EPSG:4326"))
unique(c(shape_oportunidades$C1,shape_oportunidades$C2,shape_oportunidades$C3,shape_oportunidades$C4,shape_oportunidades$C5))|>as.factor()

categorias=unique(c(shape_oportunidades$C1,shape_oportunidades$C2,shape_oportunidades$C3,shape_oportunidades$C4,shape_oportunidades$C5))
paleta_categorias=colorFactor(palette = c("#11c1f5","#621132","#9cfe89","#d4c19c","grey","#2f7e21","purple","red","#9d2449","#3560f1"),domain =unique(c(shape_oportunidades$C1,shape_oportunidades$C2,shape_oportunidades$C3,shape_oportunidades$C4,shape_oportunidades$C5)),alpha = T
)
contorno=3
opacity_fill=0.5
paleta_categorias(unique(c(shape_oportunidades$C1,shape_oportunidades$C2,shape_oportunidades$C3,shape_oportunidades$C4,shape_oportunidades$C5))[1]
)
lapply(shape_oportunidades,class)
shape_oportunidades[,9:19]=lapply(shape_oportunidades[,9:19]|>st_drop_geometry(),FUN = function(x) stri_enc_toutf8(x))
stri_enc_toutf8(shape_oportunidades$P1)
mapa_oportunidades=leaflet()|>setView(-98.88704,20.47901,zoom = 09)|>addTiles(options = tileOptions(minZoom=8,opacity = 0.7))
mapa_oportunidades=mapa_oportunidades|>
  #addLabelOnlyMarkers(data = shape_oportunidades|>st_centroid()|>as("Spatial"), label =  ~as.character(shape_oportunidades$Municipio), 
  #                    labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))|>
  addPolygons(data=shape_oportunidades|>as("Spatial"),stroke = T,color ="#b38e5d",weight = contorno ,fillColor = paleta_categorias(shape_oportunidades$C1),fillOpacity = opacity_fill,
              group = "Prioridad 1",
              label = ~
                paste(
                  "<b>Municipio: </b>", htmlEscape(shape_oportunidades$Municipio), "<br>",
                  "<p style='text-align: center;'><b>Principales problemáticas<b/><p/>",
                  paste0("<table style='border-collapse: collapse; font-size: 12px;'>",
                         "<tr><th style='border: 1px solid black; padding: 4px; text-align: center;'>Prioridad</th>",
                         "<th style='border: 1px solid black; padding: 4px; text-align: center;'>Oportunidad</th></tr>"),
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'><b>1<b/></td><td style='border: 1px solid black; padding: 4px; text-align: center;'>","<b>", shape_oportunidades$P1,"<b/>", "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>2</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P2, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>3</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P3, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>4</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P4, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>5</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P5, "</td></tr>")                  
                  ,"</table>"
                ) |>
                sapply(FUN = function(x) HTML(x), USE.NAMES = F) |>
                `class<-`("html"),
              labelOptions = labelOptions(textsize = 14, direction = "top")
  )|>
  addPolygons(data=shape_oportunidades|>as("Spatial"),stroke = T,color ="#b38e5d",weight = contorno,fillColor = paleta_categorias(shape_oportunidades$C2),fillOpacity = opacity_fill,
              group = "Prioridad 2",
              label = ~
                paste(
                  "<b>Municipio: </b>", htmlEscape(shape_oportunidades$Municipio), "<br>",
                  "<p style='text-align: center;'><b>Principales problemáticas<b/><p/>",
                  paste0("<table style='border-collapse: collapse; font-size: 12px;'>",
                         "<tr><th style='border: 1px solid black; padding: 4px; text-align: center;'>Prioridad</th>",
                         "<th style='border: 1px solid black; padding: 4px; text-align: center;'>Oportunidad</th></tr>"),
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>1</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P1, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'><b>2<b/></td><td style='border: 1px solid black; padding: 4px; text-align: center;'>","<b>", shape_oportunidades$P2,"<b/>", "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>3</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P3, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>4</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P4, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>5</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P5, "</td></tr>")                  
                  ,"</table>"
                ) |>
                sapply(FUN = function(x) HTML(x), USE.NAMES = F) |>
                `class<-`("html"),
              labelOptions = labelOptions(textsize = 14, direction = "top")
  )|>
  addPolygons(data=shape_oportunidades|>as("Spatial"),stroke = T,color ="#b38e5d",weight = contorno ,fillColor = paleta_categorias(shape_oportunidades$C3),fillOpacity = opacity_fill,
              group = "Prioridad 3",
              label = ~
                paste(
                  "<b>Municipio: </b>", htmlEscape(shape_oportunidades$Municipio), "<br>",
                  "<p style='text-align: center;'><b>Principales problemáticas<b/><p/>",
                  paste0("<table style='border-collapse: collapse; font-size: 12px;'>",
                         "<tr><th style='border: 1px solid black; padding: 4px; text-align: center;'>Prioridad</th>",
                         "<th style='border: 1px solid black; padding: 4px; text-align: center;'>Oportunidad</th></tr>"),
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>1</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P1, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>2</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P2, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'><b>3<b/></td><td style='border: 1px solid black; padding: 4px; text-align: center;'>","<b>", shape_oportunidades$P3,"<b/>", "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>4</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P4, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>5</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P5, "</td></tr>")                  
                  ,"</table>"
                ) |>
                sapply(FUN = function(x) HTML(x), USE.NAMES = F) |>
                `class<-`("html"),
              labelOptions = labelOptions(textsize = 14, direction = "top")
  )|>
  addPolygons(data=shape_oportunidades|>as("Spatial"),stroke = T,color ="#b38e5d",weight = contorno ,fillColor = paleta_categorias(shape_oportunidades$C4),fillOpacity = opacity_fill,
              group = "Prioridad 4",
              label = ~
                paste(
                  "<b>Municipio: </b>", htmlEscape(shape_oportunidades$Municipio), "<br>",
                  "<p style='text-align: center;'><b>Principales problemáticas<b/><p/>",
                  paste0("<table style='border-collapse: collapse; font-size: 12px;'>",
                         "<tr><th style='border: 1px solid black; padding: 4px; text-align: center;'>Prioridad</th>",
                         "<th style='border: 1px solid black; padding: 4px; text-align: center;'>Oportunidad</th></tr>"),
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>1</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P1, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>2</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P2, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>3</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P3, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'><b>4<b/></td><td style='border: 1px solid black; padding: 4px; text-align: center;'>","<b>", shape_oportunidades$P4,"<b/>", "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>5</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P5, "</td></tr>")                  
                  ,"</table>"
                ) |>
                sapply(FUN = function(x) HTML(x), USE.NAMES = F) |>
                `class<-`("html"),
              labelOptions = labelOptions(textsize = 14, direction = "top")
  )|>
  addPolygons(data=shape_oportunidades|>as("Spatial"),stroke = T,color ="#b38e5d",weight = contorno ,fillColor = paleta_categorias(shape_oportunidades$C5),fillOpacity = opacity_fill,
              group = "Prioridad 5",
              label = ~
                paste(
                  "<b>Municipio: </b>", htmlEscape(shape_oportunidades$Municipio), "<br>",
                  "<p style='text-align: center;'><b>Principales problemáticas<b/><p/>",
                  paste0("<table style='border-collapse: collapse; font-size: 12px;'>",
                         "<tr><th style='border: 1px solid black; padding: 4px; text-align: center;'>Prioridad</th>",
                         "<th style='border: 1px solid black; padding: 4px; text-align: center;'>Oportunidad</th></tr>"),
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>1</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P1, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>2</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P2, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>3</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P3, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'>4</td><td style='border: 1px solid black; padding: 4px; text-align: center;'>", shape_oportunidades$P4, "</td></tr>"),                  
                  paste0("<tr><td style='border: 1px solid black; padding: 4px; text-align: center;'><b>5<b/></td><td style='border: 1px solid black; padding: 4px; text-align: center;'>","<b>", shape_oportunidades$P5,"<b/>", "</td></tr>")                  
                  ,"</table>"
                ) |>
                sapply(FUN = function(x) HTML(x), USE.NAMES = F) |>
                `class<-`("html"),
              labelOptions = labelOptions(textsize = 14, direction = "top")
  )|>addLayersControl(baseGroups = paste0("Prioridad ",1:5),options = layersControlOptions(collapsed=F))|>
  addLegend(position = "bottomright",pal = paleta_categorias,values = unique(c(shape_oportunidades$C1,shape_oportunidades$C2,shape_oportunidades$C3,shape_oportunidades$C4,shape_oportunidades$C5)),opacity = opacity_fill,title = "Simbología")|>
  htmlwidgets::onRender("
    function(el, x) {
      let map=this;
      

      // Control de capas. Lista de los elementos de addLayersControl
      var layersControl = document.getElementsByClassName('leaflet-control-layers-list')[0];

      // div para el grupo de capas.
      var container = document.createElement('div');
      container.style.marginTop = '10px';
      
      // Crear el checkbox principal
      var mainCheckboxLabel = document.createElement('label');//Es un label porque asi los crea leaflet
      var mainCheckbox = document.createElement('input');
      mainCheckbox.type = 'checkbox';
      mainCheckbox.checked=true;
      mainCheckbox.id = 'mainCheckbox';
      instruccion=document.createElement('span')
      instruccion.innerHTML='Problematicas por Prioridad'
      instruccion.style.fontSize='16px'
      instruccion.style.fontWeight='bold'
      instruccion_vacia=document.createElement('div')
      instruccion_vacia.style.height='10px';
      instruccion_vacia.innerHTML=' '
      mainCheckboxLabel.appendChild(mainCheckbox);
      mainCheckboxLabel.appendChild(document.createTextNode('Empresas Relacionadas con el turismo'));
      layersControl.insertBefore(instruccion_vacia, layersControl.firstChild)
      layersControl.insertBefore(instruccion, layersControl.firstChild)
    }")
mapa_oportunidades

saveWidget(mapa_oportunidades,"Oportunidades_Problemáticas_Hidalgo150525.html",selfcontained = T,title = "Oportunidades Hidalgo")
#se le podria agregar buscador

