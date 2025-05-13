#------------------------------Importar Librerias-------------------------------
# install.packages(c("sf", "dplyr","readr", "tmap", "ggplot2"))
library(readr)    # Para leer CSV fácilmente
library(sf)        # Para leer y manejar shapefiles
library(dplyr)     # Para manipular datos
library(tmap)      # Para visualizar mapas
library(ggplot2)   # Para visualización general
library(tidyverse)
library(tools)
library(dplyr)
library(stringr)
library(purrr)
library(stringi)
library(car) #Verificar colinealidad
#--------------------------------Mortalidad (INEGI)--------------------------------------
Aguascalientes <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Aguascalientes.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Aguascalientes <- Aguascalientes %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Baja_California_Sur <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Baja%20California%20Sur.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Baja_California_Sur <- Baja_California_Sur %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Baja_California <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Baja%20California.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Baja_California <- Baja_California %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Campeche <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Campeche.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Campeche <- Campeche %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Chiapas <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Chiapas.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Chiapas <- Chiapas %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Chihuahua <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Chihuahua.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Chihuahua <- Chihuahua %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Ciudad_De_México <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Ciudad%20de%20Mexico.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Ciudad_De_México <- Ciudad_De_México %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Coahuila <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Coahuila%20de%20Zaragosa.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Coahuila <- Coahuila %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Colima <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Colima.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Colima <- Colima %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Durango <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Durango.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Durango <- Durango %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Guanajuato <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Guanajuato.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Guanajuato <- Guanajuato %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Guerrero <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Guerrero.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Guerrero <- Guerrero %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Hidalgo <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Hidalgo.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Hidalgo <- Hidalgo %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Jalisco <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Jalisco.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Jalisco <- Jalisco %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Michoacán <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Michoac%C3%A1n%20de%20Ocampo.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Michoacán <- Michoacán %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Morelos <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Morelos.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Morelos <- Morelos %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

México <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/M%C3%A9xico.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
México <- México %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Nayarit <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Nayarit.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Nayarit <- Nayarit %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Nuevo_León <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Nuevo%20Le%C3%B3n.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Nuevo_León <- Nuevo_León %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Oaxaca <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Oaxaca.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Oaxaca <- Oaxaca %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Puebla <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Puebla.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Puebla <- Puebla %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Querétaro <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Queretaro.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Querétaro <- Querétaro %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Quintana_Roo <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Quintana%20Roo.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Quintana_Roo <- Quintana_Roo %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

San_Luis_Potosí <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/San%20Luis%20Potos%C3%AD.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
San_Luis_Potosí <- San_Luis_Potosí %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Sinaloa <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Sinaloa.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Sinaloa <- Sinaloa %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Sonora <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Sonora.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Sonora <- Sonora %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Tabasco <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Tabasco.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Tabasco <- Tabasco %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Tamaulipas <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Tamaulipas.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Tamaulipas <- Tamaulipas %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Tlaxcala <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Tlaxcala.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Tlaxcala <- Tlaxcala %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Veracruz <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Varacruz%20Ignacio%20de%20la%20Llave.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Veracruz <- Veracruz %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Yucatán <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Yucat%C3%A1n.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Yucatán <- Yucatán %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))

Zacatecas <- read_csv('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Zacatecas.csv', skip_empty_rows = FALSE, locale = locale(encoding = "ISO-8859-1"))
Zacatecas <- Zacatecas %>%
  filter(if_any(everything(), ~ !is.na(.) & str_trim(.) != ""))
#--------------------Unir las tablas de Mortalidad--------------------------------
# Obtener todos los nombres de los objetos que son data.frames y tienen 4 columnas (como los que subiste)
nombres_entidades <- ls() %>%
  keep(~ is.data.frame(get(.x)) && ncol(get(.x)) == 4)
# Unir todos y agregar el nombre del objeto como Entidad_federativa
mortalidad <- map_df(nombres_entidades, function(nombre_obj) {
  df <- get(nombre_obj)
  nombre_limpio <- str_replace_all(nombre_obj, "_", " ")
  df$Entidad_federativa <- tools::toTitleCase(nombre_limpio)
  return(df)
})
#----------------------------------Limpiar datos----------------------------------
# Convertir Hombre y Mujer a formato largo
mortalidad <- mortalidad %>%
  pivot_longer(cols = c(Hombre, Mujer), names_to = "Sexo", values_to = "Defunciones")
mortalidad_limpia <- mortalidad %>%
  mutate(
    Grupo_edad = str_to_lower(str_trim(Grupo_edad)),
    Grupo_edad = str_remove(Grupo_edad, " años"), # quitar texto "años"
    Grupo_edad = str_remove(Grupo_edad, " año"), # quitar texto "año"
    Grupo_edad = str_replace_all(Grupo_edad, "menores de 1", "0-0"), # Homogeneizar
    Edad = case_when(
      Grupo_edad == "0-0" ~ 0,
      Grupo_edad == "1-4" ~ 2,
      Grupo_edad == "5-9" ~ 7,
      Grupo_edad == "10-14" ~ 12,
      Grupo_edad == "15-19" ~ 17,
      Grupo_edad == "20-24" ~ 22,
      Grupo_edad == "25-29" ~ 27,
      Grupo_edad == "30-34" ~ 32,
      Grupo_edad == "35-39" ~ 37,
      Grupo_edad == "40-44" ~ 42,
      Grupo_edad == "45-49" ~ 47,
      Grupo_edad == "50-54" ~ 52,
      Grupo_edad == "55-59" ~ 57,
      Grupo_edad == "60-64" ~ 62,
      Grupo_edad == "65-69" ~ 67,
      Grupo_edad == "70-74" ~ 72,
      Grupo_edad == "75-79" ~ 77,
      Grupo_edad == "80-84" ~ 82,
      Grupo_edad == "85 y más" ~ 87,
      TRUE ~ NA_real_
    )
  )

#-----------------------------Población (CONAPO)--------------------------------
poblacion <- read_csv("https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Poblacion%20a%20mitad%20de%20a%C3%B1o%201950-2070.csv", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
#---------------------------Limpiar Datos----------------------------------------
poblacion <- poblacion %>%
  rename(
    Año = AÑO,
    Entidad_federativa = ENTIDAD,
    Sexo = SEXO,
    Edad = EDAD,
    Poblacion = POBLACION
  ) %>%
  mutate(
    # Limpiar nombres de entidad
    Entidad_federativa = str_to_title(str_trim(Entidad_federativa)),
    # Corregir acentos si quedaron mal
    Entidad_federativa = str_replace_all(Entidad_federativa, "Republica Mexicana", "República Mexicana"),
    # Homogeneizar sexo
    Sexo = case_when(
      Sexo == "Hombres" ~ "Hombre",
      Sexo == "Mujeres" ~ "Mujer",
      TRUE ~ Sexo
    )
  ) %>%
  select(Entidad_federativa, Año, Sexo, Edad, Poblacion)
poblacion_limpia <- poblacion %>%
  filter(Entidad_federativa != "República Mexicana")
#----------------------------Hacer grupos como en Mortalidad----------------------
poblacion_quinquenal <- poblacion_limpia %>%
  mutate(
    Grupo_edad = case_when(
      Edad == 0 ~ "0-0",
      Edad %in% 1:4 ~ "1-4",
      Edad %in% 5:9 ~ "5-9",
      Edad %in% 10:14 ~ "10-14",
      Edad %in% 15:19 ~ "15-19",
      Edad %in% 20:24 ~ "20-24",
      Edad %in% 25:29 ~ "25-29",
      Edad %in% 30:34 ~ "30-34",
      Edad %in% 35:39 ~ "35-39",
      Edad %in% 40:44 ~ "40-44",
      Edad %in% 45:49 ~ "45-49",
      Edad %in% 50:54 ~ "50-54",
      Edad %in% 55:59 ~ "55-59",
      Edad %in% 60:64 ~ "60-64",
      Edad %in% 65:69 ~ "65-69",
      Edad %in% 70:74 ~ "70-74",
      Edad %in% 75:79 ~ "75-79",
      Edad %in% 80:84 ~ "80-84",
      Edad >= 85 ~ "85 y más",
      TRUE ~ NA_character_
    ),
    Edad = case_when(  # Edad representativa para que coincida con mortalidad_limpia
      Grupo_edad == "0-0" ~ 0,
      Grupo_edad == "1-4" ~ 2,
      Grupo_edad == "5-9" ~ 7,
      Grupo_edad == "10-14" ~ 12,
      Grupo_edad == "15-19" ~ 17,
      Grupo_edad == "20-24" ~ 22,
      Grupo_edad == "25-29" ~ 27,
      Grupo_edad == "30-34" ~ 32,
      Grupo_edad == "35-39" ~ 37,
      Grupo_edad == "40-44" ~ 42,
      Grupo_edad == "45-49" ~ 47,
      Grupo_edad == "50-54" ~ 52,
      Grupo_edad == "55-59" ~ 57,
      Grupo_edad == "60-64" ~ 62,
      Grupo_edad == "65-69" ~ 67,
      Grupo_edad == "70-74" ~ 72,
      Grupo_edad == "75-79" ~ 77,
      Grupo_edad == "80-84" ~ 82,
      Grupo_edad == "85 y más" ~ 87,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(Entidad_federativa, Año, Sexo, Grupo_edad, Edad) %>%
  summarise(Poblacion = sum(Poblacion), .groups = "drop")

#--------------------------------Unir Mortalidad y Poblacion----------------------
base_trm <- mortalidad_limpia %>%
  left_join(poblacion_quinquenal, by = c("Entidad_federativa", "Año", "Sexo", "Edad")) %>%
  mutate(Tasa_mortalidad = Defunciones / Poblacion)

#----------------------------Algunas Visualizaciones-------------------------------
summary(base_trm$Tasa_mortalidad)
#1. Gráfico de líneas por edad y entidad federativa
base_trm %>%
  filter(!is.na(Tasa_mortalidad)) %>%
  ggplot(aes(x = Edad, y = Tasa_mortalidad, color = Entidad_federativa)) +
  geom_line(alpha = 0.5) +
  labs(
    title = "Tasa de mortalidad por grupo de edad",
    x = "Edad representativa del grupo quinquenal",
    y = "Tasa de mortalidad",
    color = "Entidad federativa"
  ) +
  theme_minimal()
# 2. Grafico por entidad y año promedio (resumen general)
base_trm %>%
  group_by(Entidad_federativa, Año) %>%
  summarise(tasa_promedio = mean(Tasa_mortalidad, na.rm = TRUE)) %>%
  ggplot(aes(x = Año, y = tasa_promedio, color = Entidad_federativa)) +
  geom_line() +
  labs(
    title = "Tasa promedio de mortalidad por entidad a lo largo del tiempo",
    x = "Año",
    y = "Tasa promedio de mortalidad"
  ) +
  theme_minimal()
#--------------Visualizar shapefiles climáticos---------------------------------
leer_shapefile_zip <- function(url_zip, devolver_df = FALSE) {
  # 1. Descargar ZIP a archivo temporal
  archivo_temp <- tempfile(fileext = ".zip")
  download.file(url_zip, archivo_temp, mode = "wb")
  
  # 2. Crear carpeta temporal y descomprimir ZIP
  carpeta_temp <- tempfile()
  dir.create(carpeta_temp)
  unzip(archivo_temp, exdir = carpeta_temp)
  
  # 3. Buscar el .shp dentro del contenido descomprimido
  archivo_shp <- list.files(carpeta_temp, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  if (length(archivo_shp) == 0) stop("No se encontró ningún archivo .shp dentro del ZIP.")
  
  # 4. Leer shapefile como objeto 'sf'
  capa_sf <- st_read(archivo_shp[1], quiet = TRUE)
  
  # 5. Si se solicita como data.frame sin geometría
  if (devolver_df) {
    return(st_drop_geometry(capa_sf))
  } else {
    return(capa_sf)
  }
}




# Obtener como objeto 'sf' (para mapas)
su25_sf <- leer_shapefile_zip('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/tendencia_regional_su25.zip')
regiones_clima_sf <- leer_shapefile_zip('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/regiones_clima_itrf08.zip')
entidades_sf <- leer_shapefile_zip('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Entidades_Federativas.zip')
# Releer la columna con codificación corregida
entidades_sf$NOMGEO <- iconv(entidades_sf$NOMGEO, from = "latin1", to = "UTF-8")
tr20_sf <- leer_shapefile_zip('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/tendencia_regional_tr20.zip')
wsdi_sf <- leer_shapefile_zip('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/tendencia_regional_wsdi.zip')
r95p_sf <- leer_shapefile_zip('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/tendencia_regional_r95p.zip')
sdii_sf <- leer_shapefile_zip('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/tendencia_regional_sdii.zip')

# Obtener como data.frame sin geometría (para modelado)
su25_df <- leer_shapefile_zip('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/tendencia_regional_su25.zip', devolver_df = TRUE)
regiones_clima_df <- leer_shapefile_zip('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/regiones_clima_itrf08.zip', devolver_df =TRUE)
entidades_df <- leer_shapefile_zip('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Entidades_Federativas.zip', devolver_df = TRUE, locale = locale(encoding = "UTF-8"))
entidades_df$NOMGEO <- iconv(entidades_df$NOMGEO, from = "latin1", to = "UTF-8")
tr20_df <- leer_shapefile_zip('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/tendencia_regional_tr20.zip', devolver_df = TRUE)
wsdi_df <- leer_shapefile_zip('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/tendencia_regional_wsdi.zip', devolver_df = TRUE)
r95p_df <- leer_shapefile_zip('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/tendencia_regional_r95p.zip', devolver_df = TRUE)
sdii_df <- leer_shapefile_zip('https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/tendencia_regional_sdii.zip', devolver_df = TRUE)

#---------------Asignar una region climatica a cada entidad federativa---------
# Lista de índices que quieres procesar
indices <- c("su25", "tr20", "wsdi", "r95p", "sdii")

# Lista para guardar los resultados válidos
resultados_indices <- list()

for (indice in indices) {
  nombre_obj <- paste0(indice, "_sf")
  
  if (exists(nombre_obj)) {
    capa_sf <- get(nombre_obj)
    
    # Verifica que sea objeto sf y tenga la columna TEND_L
    if ("sf" %in% class(capa_sf) && "TEND_L" %in% names(capa_sf)) {
      message("✓ Procesando índice: ", indice)
      
      # Transformar proyección
      capa_sf <- st_transform(capa_sf, st_crs(entidades_sf))
      
      # Unión espacial
      entidades_con_clima <- st_join(entidades_sf, capa_sf["TEND_L"])
      
      # Resumen por entidad federativa
      resumen <- entidades_con_clima %>%
        group_by(CVE_ENT, NOMGEO) %>%
        summarise(valor = mean(TEND_L, na.rm = TRUE), .groups = "drop") %>%
        st_drop_geometry() %>%
        rename(Entidad_federativa = NOMGEO)
      
      # Renombrar columna 'valor' con el nombre del índice
      colnames(resumen)[colnames(resumen) == "valor"] <- indice
      
      resultados_indices[[indice]] <- resumen
    } else {
      warning("⚠️ El objeto '", nombre_obj, "' no es sf válido o le falta la columna TEND_L.")
    }
  } else {
    warning("⚠️ El objeto '", nombre_obj, "' no existe.")
  }
}

# Combinar todos los índices disponibles en un solo data.frame
clima_final <- reduce(resultados_indices, full_join, by = c("CVE_ENT", "Entidad_federativa"))

# Diccionario de equivalencias
equivalencias <- c(
  "Coahuila de Zaragoza" = "Coahuila",
  "Ciudad de México" = "Ciudad De México",
  "Michoacán de Ocampo" = "Michoacán",
  "Veracruz de Ignacio de la Llave" = "Veracruz"
)

# Reemplazar nombres en clima_final
clima_final <- clima_final %>%
  mutate(Entidad_federativa = recode(Entidad_federativa, !!!equivalencias))
#-------------------------Unir Clima a la Base para analizar-------------------------
# Unir clima_final con base_trm por Entidad_federativa
base_completa <- base_trm %>%
  left_join(clima_final, by = "Entidad_federativa")

summary(base_completa)  # o cualquier índice

#--------------------------Algunas visualizaciones de la Base Completa------------------
#Tendencia de tasa de mortalidad a lo largo del tiempo
ggplot(base_completa, aes(x = Año, y = Tasa_mortalidad, color = Sexo)) +
  geom_line(stat = "summary", fun = mean) +
  facet_wrap(~ Entidad_federativa) +
  labs(title = "Tasa de mortalidad media por año y sexo",
       y = "Tasa de mortalidad",
       x = "Año") +
  theme_minimal()

#Relación entre índice climático y tasa de mortalidad
ggplot(base_completa, aes(x = su25, y = Tasa_mortalidad)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre índice SU25 y tasa de mortalidad",
       x = "SU25 (días muy cálidos)",
       y = "Tasa de mortalidad") +
  theme_minimal()
#Boxplot por grupo de edad
ggplot(base_completa, aes(x = factor(Grupo_edad.x), y = Tasa_mortalidad)) +
  geom_boxplot() +
  labs(title = "Distribución de tasa de mortalidad por grupo de edad",
       x = "Grupo de edad",
       y = "Tasa de mortalidad") +
  theme_minimal()
#Mapa de calor por entidad y año
ggplot(base_completa, aes(x = Año, y = Entidad_federativa, fill = Tasa_mortalidad)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Mapa de calor: tasa de mortalidad por entidad y año",
       x = "Año", y = "Entidad") +
  theme_minimal()

ggplot(base_completa, aes(x = su25, y = Tasa_mortalidad, color = Grupo_edad.x)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Grupo_edad.x) +
  theme_minimal()
#--------------------------MODELO--------------------------
modelo_trm <- lm(log(Tasa_mortalidad + 1e-5) ~ su25 + Edad + Sexo + factor(Entidad_federativa), data = base_completa)
summary(modelo_trm)

#Modelo que quita variables con ayuda de R
modelo_step <- step(modelo_trm, direction = "both", trace = FALSE)
summary(modelo_step)

#Modelo con mas variables climaticas
modelo_ext <- lm(log(Tasa_mortalidad + 1e-5) ~ su25 + tr20 + wsdi + r95p + sdii + Edad + Sexo + factor(Entidad_federativa), data = base_completa)
summary(modelo_ext)
vif(modelo_ext) #Colinealidad verificar
alias(modelo_ext) #Detectar colinealidad

#Modelo solo con variables climaticas
modelo_clima <- lm(log(Tasa_mortalidad + 1e-5) ~ su25 + tr20 + wsdi + r95p + sdii + Edad + Sexo, data = base_completa)
summary(modelo_clima)
vif(modelo_clima)

modelo_clima2 <- lm(log(Tasa_mortalidad + 1e-5) ~ su25 + tr20 + Edad + Sexo, data = base_completa)
vif(modelo_clima2)
summary(modelo_clima2)

modelo_final <- lm(log(Tasa_mortalidad + 1e-5) ~ tr20 + wsdi + r95p + Edad + Sexo, data = base_completa)
summary(modelo_final)
vif(modelo_final)



# Interacción con edad
modelo_inter <- lm(log(Tasa_mortalidad + 1e-5) ~ su25 * Edad + factor(Entidad_federativa), data = base_completa)
summary(modelo_inter)

#Prediccion de la Prediccion vs Observado
base_completa$predicho <- predict(modelo_final)

ggplot(base_completa, aes(x = predicho, y = log(Tasa_mortalidad + 1e-5))) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Valores observados vs predichos (log Tasa de mortalidad)",
    x = "Predicción del modelo",
    y = "Valor observado"
  ) +
  theme_minimal()
#Tasa de mortalidad vs Edad por sexo
ggplot(base_completa, aes(x = Edad, y = Tasa_mortalidad, color = Sexo)) +
geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Tasa de mortalidad observada por edad y sexo",
    x = "Edad",
    y = "Tasa de mortalidad"
  ) +
  theme_minimal()
#Residuos
ggplot(base_completa, aes(x = predicho, y = residuals(modelo_final))) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuos vs Predicción", x = "Predicción", y = "Residuos")





