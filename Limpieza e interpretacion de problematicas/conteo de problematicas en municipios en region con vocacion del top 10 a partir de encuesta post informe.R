#conteo de problemáticas de las percepciones a nivel region-vocacional desde la encuesta del informe
library(foreign)
cat_sav=read.spss("../../../Tareas/PostInforme 2024 aceptacion_problematicas/Base Hidalgo Noviembre 2024 Post Informe.SAV")
sav=foreign::read.spss("../../../Tareas/PostInforme 2024 aceptacion_problematicas/Base Hidalgo Noviembre 2024 Post Informe.SAV",to.data.frame = T)
#mujeres que tienen hijes están afiliadas al pan
variable_labels <- attr(sav, "variable.labels")
sav[is.na(sav)]='-'
variables=as.data.frame(matrix(c(colnames(sav),variable_labels),ncol=2,nrow=length(colnames(sav)),byrow = F))
#library(dplyr)
# Asigna las etiquetas como nombres de las columnas
colnames(sav) <- variable_labels
#colnames(sav)[345]="PESOF"
base_problemas_Aprobacion=sav |> 
  dplyr::select(MPO_INEGI,P10,P15,PESOF)

#base_problemas_Aprobacion=merge(base_problemas_Aprobacion,regiones,by.x='MPO_INEGI',by.y='Municipio',all.x=T)
#Este será por región vocacional.
base_problemas_Aprobacion=base_problemas_Aprobacion |> 
  dplyr::group_by(MPO_INEGI,P10) |> 
  #dplyr::mutate(Región=as.character(Región)) |> 
  dplyr::mutate(P10=as.character(P10)) |> 
  dplyr::summarise(suma_problematicas=sum(PESOF,na.rm = T))
municipios_problemas=base_problemas_Aprobacion |> tidyr::pivot_wider(names_from = P10,values_from = suma_problematicas)

lista_vocaciones_percepcion=list()
for (vocacion in Names) {
  print(paste("------------------- Vocacion:", vocacion, "-------------------"))
  
  data_final2 <- data_final |>
    dplyr::arrange(dplyr::desc(dplyr::across(vocacion)))|>
    dplyr::slice_head(n=5) |> 
    dplyr::select(-c(dplyr::matches(Names))) |> 
    dplyr::select(where(~!all(. == "" | is.na(. )))) |> 
    dplyr::select(`Sector--Municipio`)
  vocacion_municipios_problemas=municipios_problemas |> 
    dplyr::filter(MPO_INEGI%in% data_final2$`Sector--Municipio`)
  municipios_problemas_en_porc=vocacion_municipios_problemas[,-1] |> colSums(na.rm = T) |> as_tibble(rownames = NA)
  municipios_problemas_en_porc$name=rownames(municipios_problemas_en_porc)
  municipios_problemas_en_porc=municipios_problemas_en_porc |> 
    dplyr::relocate(name,.before = value)
  municipios_problemas_en_porc=municipios_problemas_en_porc |> 
    dplyr::mutate(en_porc=100*(value/sum(municipios_problemas_en_porc$value,na.rm = T)))
  #www=municipios_problemas_en_porc |> tidyr::pivot_longer(names_to = "Problematica",values_to = "Porcentaje",cols = AGUA:TODO)
  
  
  
  lista_vocaciones_percepcion[[vocacion |> substr(1,31)]] <-municipios_problemas_en_porc |> dplyr::arrange(dplyr::desc(value))
  #print(conteo_por_columna)
}
openxlsx::write.xlsx(lista_vocaciones_percepcion,"conteo_poblacion_mun_top10_p_vocacion_por_problematica_percepcion_encuesta_post_informe.xlsx")
openxlsx::write.xlsx(lista_vocaciones_percepcion,"conteo_poblacion_mun_top10_p_vocacion_por_problematica_percepcion_encuesta_post_informe.xlsx")









####Aprobacion 
sav$PESOF |> sum()
base_problemas_Aprobacion=sav |> 
  dplyr::select(MPO_INEGI,P10,P15,PESOF)

base_problemas_Aprobacion=merge(base_problemas_Aprobacion,regiones,by.x='MPO_INEGI',by.y='Municipio',all.x=T)

base_problemas_Aprobacion=base_problemas_Aprobacion |> 
  dplyr::group_by(Región,P15) |> 
  dplyr::mutate(Región=as.character(Región)) |> 
  dplyr::mutate(P15=as.character(P15)) |> 
  dplyr::summarise(suma_aprobacion=sum(PESOF,na.rm = T))
municipios_problemas=base_problemas_Aprobacion |> tidyr::pivot_wider(names_from = P15,values_from = suma_aprobacion)

rowSums(municipios_problemas |>dplyr::ungroup() |>  dplyr::select(Aprueba:`No sé`),na.rm = T)
municipios_problemas_en_porc=municipios_problemas
municipios_problemas_en_porc[,-1]=municipios_problemas |>dplyr::ungroup() |>  dplyr::select(Aprueba:`No sé`) |> lapply(FUN=\(x){
  100*x/rowSums(municipios_problemas |>dplyr::ungroup() |>  dplyr::select(Aprueba:`No sé`),na.rm = T)
})

municipios_problemas_en_porc |> write.csv("Jair/Tareas/PostInforme 2024 aceptacion_problematicas/aprobacion_p_region.csv",fileEncoding = "latin1",row.names = F)


www=municipios_problemas_en_porc |> tidyr::pivot_longer(names_to = "Aprobación",values_to = "Porcentaje",cols = Aprueba:`No sé`)
www=www |>dplyr::group_by(Región) |>  dplyr::arrange(desc(Porcentaje))
www=www |> dplyr::ungroup() |> dplyr::arrange(desc(Región))
www |> write.csv("Jair/Tareas/PostInforme 2024 aceptacion_problematicas/aprobacion_p_region_c_pivot.csv",fileEncoding = "latin1",row.names = F)
