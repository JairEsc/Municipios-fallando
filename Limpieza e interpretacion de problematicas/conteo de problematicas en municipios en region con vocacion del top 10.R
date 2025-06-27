regiones=regiones |> 
  dplyr::filter(!is.na(Municipio) & Municipio!='Estatal')

data_final=merge(data_inicial,regiones,by.x='Sector--Municipio',by.y = 'Municipio',all.x=T) |> 
  dplyr::relocate(Región,.after   = `Sector--Municipio`) 
data_final$`Sector--Municipio` |> unique()
#data_final$`Sector--Municipio`[data_final$`Sector--Municipio`=="San Agustrn Metzquititlán"]="San Agustín Metzquititlán"
#data_final$`Sector--Municipio`[data_final$`Sector--Municipio`=="Zapotlan de Júarez"]="Zapotlán de Júarez"
data_final=data_final |> 
  dplyr::select(-c(`INDEMUN--Bando de Policía y Gobierno`:`INDEMUN--Acciones del municipio para disminuir la vulnerabilidad ante el cambio climático`))
lista_regiones=list()
##Usaremos también los datos de ballasa para la creación de los rasters de vocación. 

ballasa_p_mun=read.csv("balassa_p_mun_c_region.csv",fileEncoding = "latin1")
Names=c("Minería.No.Metálicos","Minería.Metálicos","Industria.Química","Fabricación.de.equipo.electrónico.y.eléctrico",
        "Fabricación.de.carrocerías..y.equipo.de.transporte.etc","Fabricación.de.maquinaria","Fabricación.de.muebles",
        "Maquiladoras.y.textileras","Comunicaciones.y.transportes.paqueteria","Manufactura.metal.y.herreria",
        "Alimentos.y.bebidas","Explotacion.de.animales","Fabricación.de.material.de.construcción","Hospedaje",
        "Industria.del.papel","Banca.y.seguros")
data_final=merge(data_final,ballasa_p_mun |> dplyr::select(c(NOM_MUN,dplyr::matches(Names))),by.x='Sector--Municipio',by.y='NOM_MUN',all.x=T)
for (vocacion in Names) {
  print(paste("------------------- Vocacion:", vocacion, "-------------------"))
  
  data_final2 <- data_final |>
    dplyr::arrange(dplyr::desc(dplyr::across(vocacion)))|>
    dplyr::slice_head(n=5) |> 
    dplyr::select(-c(dplyr::matches(Names))) |> 
    dplyr::select(where(~!all(. == "" | is.na(. ))))
  
  conteo_por_columna <- data_final2 |>
    dplyr::summarise(dplyr::across(.fns = ~ sum(!is.na(.) ))) |>
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "nombre_columna",
      values_to = "conteo_valores_no_vacios"
    )
  
  # Inicializar la columna para la parte removida
  conteo_por_columna$prefijo_extraido <- NA_character_
  
  for (tema_seleccionado in unique(copia_colnames)[3:9]) { # Asegúrate de que los índices sean correctos para tus temas
    patron <- paste0(tema_seleccionado, "--")
    
    # Usamos stringr::str_detect para verificar si el patrón existe
    # y str_extract para extraerlo, y str_remove para limpiar el nombre
    rows_to_update <- stringr::str_detect(conteo_por_columna$nombre_columna, stringr::fixed(patron))
    
    # Solo actualiza si hay filas que coinciden
    if (any(rows_to_update)) {
      # Almacenar el prefijo antes de removerlo
      conteo_por_columna$prefijo_extraido[rows_to_update] <-
        stringr::str_squish(gsub("[0-9]","",gsub("\\."," ",gsub("--|-","",stringr::str_extract(conteo_por_columna$nombre_columna[rows_to_update], stringr::fixed(patron))))))
      
      # Remover el prefijo
      conteo_por_columna$nombre_columna[rows_to_update] <-
        stringr::str_remove(conteo_por_columna$nombre_columna[rows_to_update], stringr::fixed(patron))
    }
  }
  
  lista_regiones[[vocacion |> substr(1,31)]] <- conteo_por_columna |> dplyr::arrange(dplyr::desc(conteo_valores_no_vacios))
  print(conteo_por_columna)
}
openxlsx::write.xlsx(lista_regiones,"conteo_municipios_Con_Vocacion_c_problematica_de_indicadores.xlsx")
lista_regiones["Minería.Metálicos"] |> write.csv("MIN_MET_conteo_municipios_Con_Vocacion_c_problematica_de_indicadores.csv")
