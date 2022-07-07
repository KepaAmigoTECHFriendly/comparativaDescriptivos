#' @title Integra como telemetría al dispositivo Comparativa - Indicadores descriptivos los datos desciptivos (BBDD MITMA) del municipio demandado
#'
#' @description Integra como telemetría al dispositivo Comparativa - Indicadores descriptivos los datos desciptivos (BBDD MITMA) del municipio demandado
#'
#' @param ip_plataforma, id_dispositivo_comparativa, nombre_municipio
#'
#' @return json
#'
#' @examples  municipio_comparativa_descriptivos("http://82.223.66.83:8080","81006600-fc46-11ec-94d8-c1cee3606f2e","Barakaldo")
#'
#' @import httr
#' jsonlite
#' dplyr
#'
#' @export

municipio_comparativa_descriptivos <- function(ip_plataforma, id_dispositivo_comparativa, nombre_municipio){

  ip_plataforma <- sub("/.*/(.*?)/.*", "\\1", as.character(ip_plataforma))  # Captura hasta la tercera /
  id_dispositivo_comparativa <- as.character(id_dispositivo_comparativa)
  nombre_municipio <- as.character(nombre_municipio)

  # -----------------------------------------------------------------------------
  # DATA FRAME INDICADORES DESCRIPTIVOS
  # -----------------------------------------------------------------------------

  #df_descriptivos <- read.csv("/home/kepa/TECH friendly/PROYECTOS/Observatorio Agenda Urbana/SCRIPTS R/datos_descriptivos_Espana.csv", sep = ",")
  df_descriptivos <- read.csv(as.character(list.files(system.file('extdata', package = 'my_package'), full.names = TRUE)), sep = ",")
  df_descriptivos <- df_descriptivos[,-1]
  nombre_columnas <- "NombreMun,D.01. Variación de la población 2007 – 2017 (%),D.02.a. Superficie de cobertura artificial por municipio (%).,D.02.b. Superficie de cultivos por municipio (%).,D.03.a. Superficie municipal destinada a explotaciones agrarias y forestales (%).,D.03.b. Superficie destinada a explotaciones agrarias y forestales respecto al suelo urbano y urbanizable delimitado de la ciudad (%).,D.04. Superficie municipal de suelo no urbanizable (%).,D.05. Superficie de zonas verdes por cada 1.000 habitantes.,D.06. Densidad Urbana. Número de habitantes por hectárea de superficie de suelo urbano (hab./ha).,D.07. Superficie de suelo urbano mixto discontinuo sobre suelo urbano mixto total (%),D.08. Densidad de vivienda por superficie de suelo urbano (Viv/ha).,D.09. Compacidad urbana. Superficie construida total por superficie de suelo (m2t/m2s),D.10.a. Superficie construida de uso residencial por superficie de suelo (m2t/m2s),D.10.b. Superficie construida de uso residencial respecto al total de superficie construida (%).,D.ST.01. Densidad de viviendas previstas en las áreas de suelo de desarrollo  (Viv/ha).,D.ST.02. Porcentaje de áreas de suelo de desarrollo respecto al total del suelo urbano (%),D.ST.03. Suelo urbanizable delimitado respecto al total del suelo urbano (%),D.ST.04. Porcentaje de áreas de suelo en desarrollo de uso residencial respecto al total de suelo urbano (%).,D.ST.05. Porcentaje de áreas de suelo en desarrollo de uso actividades económica (industrial o terciario) respecto al total de suelo urbano (%),D.14. Porcentaje del parque edificatorio por municipio con una antigüedad anterior al año 2000 (%).,D.17.a. Superficie de infraestructuras de transporte (ha).,D.17.b. Porcentaje de superficie de infraestructuras de transporte respecto al término municipal (%),D.18.a. Vehículos domiciliados cada 1000 habitantes.,D.18.b. Porcentaje de Turismos (%),D.18.c. Porcentaje de motocicletas (%),D.22.a. Índice de envejecimiento de la población (%),D.22.b. Índice de senectud de la población (%),D.23. Porcentaje de población extranjera (%),D.24.a. Índice de dependencia total (%),D.24.b. Índice de dependencia infantil (%),D.24.c. Índice de dependencia de mayores (%),D.26.a. Trabajadores en sector agricultura (%).,D.26.b. Trabajadores en sector industria (%).,D.26.c. Trabajadores en sector construcción (%).,D.26.d. Trabajadores en sector servicios (%).,D.27.a. Establecimientos en sector agricultura (%).,D.27.b. Establecimientos en sector industria (%).,D.27.c. Establecimientos en sector construcción (%).,D.27.d. Establecimientos en sector servicios (%).,D.28.a. Porcentaje de parados total (%).,D.28.b. Porcentaje de parados entre 25 y 44 años (%),D.28.c. Proporción de paro femenino (%),D.29. Número de viviendas por cada 1.000 habitantes.,D.32. Variación del número de hogares 2001-2011 (%),D.33. Crecimiento del parque de vivienda 2001-2011 (%),D.34. Porcentaje de vivienda secundaria (%).,D.35. Porcentaje de vivienda vacía (%).,D.ST.06. Porcentaje de viviendas previstas en áreas de desarrollo respecto al parque de vivienda existente (%).,D.ST.07. Número de viviendas previstas en las áreas de desarrollo por cada 1.000 habitantes.,D.37. FIGURA DE PLANEAMIENTO URBANÍSTICO VIGENTE EN EL MUNICIPIO,D.38. FECHA DE LA FIGURA DE PLANEAMIENTO URBANÍSTICO VIGENTE EN EL MUNICIPIO."
  nombre_columnas <- unlist(strsplit(nombre_columnas,","))
  colnames(df_descriptivos) <- nombre_columnas

  df_descriptivos <- df_descriptivos[df_descriptivos$NombreMun == nombre_municipio,]  # Filtrado municipio
  #Paso a numérico
  for(i in 2:49){
    df_descriptivos[,i] <- as.numeric(gsub("[,]",".",df_descriptivos[,i]))

  }


  # -----------------------------------------------------------------------------
  # PETICIÓN TOKEN THB
  # -----------------------------------------------------------------------------
  url <- paste(ip_plataforma,"/api/auth/login",sep = "")
  cuerpo <- '{"username":"kepa@techfriendly.es","password":"kepatech"}'
  post <- httr::POST(url = url,
                     add_headers("Content-Type"="application/json","Accept"="application/json"),
                     body = cuerpo,
                     encode = "json",verbose()
  )

  resultado_peticion_token <- httr::content(post)
  auth_thb <- paste("Bearer",resultado_peticion_token$token)

  # -----------------------------------------------------------------------------
  # CARGA INDICADORES EN DISPOSITIVO: COMPARATIVA MUNICIPIO
  # -----------------------------------------------------------------------------

  json_datos <- jsonlite::toJSON(df_descriptivos,pretty=T)
  json_datos <- sub("[[]","",json_datos)
  json_datos <- sub("[]]","",json_datos)

  # GET token del dispositivo
  url <- paste(ip_plataforma,"/api/device/",id_dispositivo_comparativa,"/credentials",sep = "")
  get_token <- httr::GET(url = url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb))

  token <- jsonlite::fromJSON(rawToChar(get_token$content))
  token <- token$credentialsId

  url <- paste(ip_plataforma,"/api/v1/",token,"/telemetry",sep = "")

  # 1) Creacion atributos horarios
  #json_envio_plataforma <- paste('{"hora_subida_persianas":', 12,'}',sep = "")
  post <- httr::POST(url = url,
                     add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"=auth_thb),
                     body = json_datos,
                     verify= FALSE,
                     encode = "json",verbose()
  )

  print("Programa finalizado")

  return(json_datos)
}
