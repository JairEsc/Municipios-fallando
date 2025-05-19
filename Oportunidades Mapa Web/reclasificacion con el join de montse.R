#Se cancela el fuzzyjoin y utilizamos el join que hizo montse

shape_oportunidades_montse=sf::read_sf("Problemas_a_nivel_municipal_montse.shp")




shape_oportunidades_montse$C1=
  shape_oportunidades_montse$TP1|> stringi::stri_extract(regex  = "Gestión territorial y desarrollo urbano|Medio Ambiente|Economía y Empleo|Infraestructura y Transporte|Seguridad y corrupcion|Seguridad y corrupción|Economia y Empleo|Agua y Crisis Hidrica|Agua y Crisis Hídrica|Educacion|Gestion territorial y desarrollo urbano|Problemas Sociales|Problemas sociales|Educación|Riesgos|Salud") 

shape_oportunidades_montse$C2=
  shape_oportunidades_montse$TP2 |> stringi::stri_extract(regex  = "Gestión territorial y desarrollo urbano|Medio Ambiente|Economía y Empleo|Infraestructura y Transporte|Seguridad y corrupcion|Seguridad y corrupción|Economia y Empleo|Agua y Crisis Hidrica|Agua y Crisis Hídrica|Educacion|Gestion territorial y desarrollo urbano|Problemas Sociales|Problemas sociales|Educación|Riesgos|Salud") 

shape_oportunidades_montse$C3=
  shape_oportunidades_montse$TP3 |> stringi::stri_extract(regex  = "Gestión territorial y desarrollo urbano|Medio Ambiente|Economía y Empleo|Infraestructura y Transporte|Seguridad y corrupcion|Seguridad y corrupción|Economia y Empleo|Agua y Crisis Hidrica|Agua y Crisis Hídrica|Educacion|Gestion territorial y desarrollo urbano|Problemas Sociales|Problemas sociales|Educación|Riesgos|Salud") 

shape_oportunidades_montse$C4=
  shape_oportunidades_montse$TP4 |> stringi::stri_extract(regex  = "Gestión territorial y desarrollo urbano|Medio Ambiente|Economía y Empleo|Infraestructura y Transporte|Seguridad y corrupcion|Seguridad y corrupción|Economia y Empleo|Agua y Crisis Hidrica|Agua y Crisis Hídrica|Educacion|Gestion territorial y desarrollo urbano|Problemas Sociales|Problemas sociales|Educación|Riesgos|Salud") 

shape_oportunidades_montse$C5=
  shape_oportunidades_montse$TP5 |> stringi::stri_extract(regex  = "Gestión territorial y desarrollo urbano|Medio Ambiente|Economía y Empleo|Infraestructura y Transporte|Seguridad y corrupcion|Seguridad y corrupción|Economia y Empleo|Agua y Crisis Hidrica|Agua y Crisis Hídrica|Educacion|Gestion territorial y desarrollo urbano|Problemas Sociales|Problemas sociales|Educación|Riesgos|Salud") 



shape_oportunidades_montse$TP1 |> stringi::stri_split_regex(pattern = "Gestión territorial y desarrollo urbano|Medio Ambiente|Economía|Infraestructura y Transporte|Seguridad y corrupcion|Seguridad y corrupción|Economia y Empleo|Agua y Crisis Hidrica|Agua y Crisis Hídrica|Educacion|Gestion territorial y desarrollo urbano|Problemas Sociales|Educación|Riesgos|Salud",n = 2) |> lapply(\(x) x[2]) |> unlist() |> stringr::str_squish()


shape_oportunidades_montse$P1=
  shape_oportunidades_montse$TP1 |> stringi::stri_split_regex(pattern = "Gestión territorial y desarrollo urbano|Medio Ambiente|Economía y Empleo|Infraestructura y Transporte|Seguridad y corrupcion|Seguridad y corrupción|Economia y Empleo|Agua y Crisis Hidrica|Agua y Crisis Hídrica|Educacion|Gestion territorial y desarrollo urbano|Problemas Sociales|Problemas sociales|Educación|Riesgos|Salud",n = 2) |> lapply(\(x) x[2]) |> unlist() |> stringr::str_squish()

shape_oportunidades_montse$P2=
  shape_oportunidades_montse$TP2 |> stringi::stri_split_regex(pattern = "Gestión territorial y desarrollo urbano|Medio Ambiente|Economía y Empleo|Infraestructura y Transporte|Seguridad y corrupcion|Seguridad y corrupción|Economia y Empleo|Agua y Crisis Hidrica|Agua y Crisis Hídrica|Educacion|Gestion territorial y desarrollo urbano|Problemas Sociales|Problemas sociales|Educación|Riesgos|Salud",n = 2) |> lapply(\(x) x[2]) |> unlist() |> stringr::str_squish()

shape_oportunidades_montse$P3=
  shape_oportunidades_montse$TP3 |> stringi::stri_split_regex(pattern = "Gestión territorial y desarrollo urbano|Medio Ambiente|Economía y Empleo|Infraestructura y Transporte|Seguridad y corrupcion|Seguridad y corrupción|Economia y Empleo|Agua y Crisis Hidrica|Agua y Crisis Hídrica|Educacion|Gestion territorial y desarrollo urbano|Problemas Sociales|Problemas sociales|Educación|Riesgos|Salud",n = 2) |> lapply(\(x) x[2]) |> unlist() |> stringr::str_squish()

shape_oportunidades_montse$P4=
  shape_oportunidades_montse$TP4 |> stringi::stri_split_regex(pattern = "Gestión territorial y desarrollo urbano|Medio Ambiente|Economía y Empleo|Infraestructura y Transporte|Seguridad y corrupcion|Seguridad y corrupción|Economia y Empleo|Agua y Crisis Hidrica|Agua y Crisis Hídrica|Educacion|Gestion territorial y desarrollo urbano|Problemas Sociales|Problemas sociales|Educación|Riesgos|Salud",n = 2) |> lapply(\(x) x[2]) |> unlist() |> stringr::str_squish()

shape_oportunidades_montse$P5=
  shape_oportunidades_montse$TP5 |> stringi::stri_split_regex(pattern = "Gestión territorial y desarrollo urbano|Medio Ambiente|Economía y Empleo|Infraestructura y Transporte|Seguridad y corrupcion|Seguridad y corrupción|Economia y Empleo|Agua y Crisis Hidrica|Agua y Crisis Hídrica|Educacion|Gestion territorial y desarrollo urbano|Problemas Sociales|Problemas sociales|Educación|Riesgos|Salud",n = 2) |> lapply(\(x) x[2]) |> unlist() |> stringr::str_squish()

shape_oportunidades_montse |> sf::write_sf("oportunidades_municipios_reclasificado.shp")

iconv(c(shape_oportunidades_montse$TP1,shape_oportunidades_montse$TP2,shape_oportunidades_montse$TP3, shape_oportunidades_montse$TP4, shape_oportunidades_montse$TP5),to = "ASCII//TRANSLIT") |> stringr::str_to_lower() |> unique()

#Conteo del listado de problemas 
shape_oportunidades_montse_pivoted=shape_oportunidades_montse |> sf::st_drop_geometry() |> 
  tidyr::pivot_longer(names_to = "listado",cols = TP1:TP5)
conteo_problemas=shape_oportunidades_montse_pivoted |> 
  dplyr::group_by(value) |> 
  dplyr::summarise(dplyr::n())
conteo_problemas |> write.csv("../../../Repositorios/Municipios fallando/Oportunidades Mapa Web/Listado de Problemas públicos reconteo.csv",fileEncoding = "latin1",row.names = F)
