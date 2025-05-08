#------------------------------Importar Librerias-------------------------------
# install.packages(c("sf", "dplyr","readr", "tmap", "ggplot2"))
library(readr)    # Para leer CSV fácilmente
library(sf)        # Para leer y manejar shapefiles
library(dplyr)     # Para manipular datos
library(tmap)      # Para visualizar mapas
library(ggplot2)   # Para visualización general
library(tidyverse)
library(sf)
library(tools)
library(dplyr)
#--------------------------------Mortalidad--------------------------------------
mortalidad <- read_csv('https://raw.githubusercontent.com/JuanMi-Galan/Seminario-de-Investigaci-n/refs/heads/main/Mortalidad_01-Por%20Genero%20y%20Entdad%20Federativa.csv', col_names = FALSE)
# Crear nombres de columna combinando año + sexo
nombres_combinados <- paste0(mortalidad[2, -1], "_", mortalidad[1, -1])
nombres_finales <- c("Entidad_federativa", nombres_combinados)
# Asignar nombres nuevos y eliminar las primeras dos filas (que eran encabezados)
mortalidad <- mortalidad[-c(1,2), ]
colnames(mortalidad) <- nombres_finales
# Convertir a formato largo
mortalidad <- mortalidad %>%
  pivot_longer(
    cols = -Entidad_federativa,
    names_to = c("Año", "Sexo"),
    names_sep = "_",
    values_to = "Mortalidad"
  ) %>%
  mutate(
    Año = as.integer(Año),
    Mortalidad = as.numeric(Mortalidad)
  )
#-------------------------------Esperanza de Vida----------------------------------
esperanza_vida <- read_csv('https://raw.githubusercontent.com/JuanMi-Galan/Seminario-de-Investigaci-n/refs/heads/main/Esperanza%20de%20VIda-Por%20Genero%20y%20Entidad%20Federativa.csv', col_names = FALSE)
# Crear nombres de columna combinando año + sexo
nombres_combinados <- paste0(esperanza_vida[2, -1], "_", esperanza_vida[1, -1])
nombres_finales <- c("Entidad_federativa", nombres_combinados)
# Asignar nombres nuevos y eliminar las primeras dos filas (que eran encabezados)
esperanza_vida <- esperanza_vida[-c(1,2), ]
colnames(esperanza_vida) <- nombres_finales
# Convertir a formato largo
esperanza_vida <- esperanza_vida %>%
  pivot_longer(
    cols = -Entidad_federativa,
    names_to = c("Año", "Sexo"),
    names_sep = "_",
    values_to = "Esperanza_vida"
  ) %>%
  mutate(
    Año = as.integer(Año),
    Esperanza_vida = as.numeric(Esperanza_vida)
  )
#-----------------------------Mapa de Esperanza de Vida-------------------------
'''
mx_estados <- read("https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/dest2019gw.zip", quiet = TRUE)
# Arreglar nombres de entidades en tu dataset para que coincidan con el shapefile
unique(esperanza_vida$Entidad_federativa)
unique(mx_estados$name)
# Crear versión simplificada de esperanza de vida para un año y sexo específico
esperanza_estado <- esperanza_vida %>%
  filter(Año == "2020", Sexo == "Total") %>%
  mutate(Entidad = str_to_title(Entidad_federativa))  # Capitalizar como en el shapefile
# Unir shapefile con esperanza de vida
mx_mapa <- mx_estados %>%
  left_join(esperanza_estado, by = c("name" = "Entidad_federativa"))
'''
#--------------Visualizar shapefiles climáticos---------------------------------
url <- 'https://github.com/JuanMi-Galan/Seminario-de-Investigaci-n/raw/refs/heads/main/Entidades_Federativas.zip'
# Crear archivo temporal
temp <- tempfile()
download.file(url, temp)
# Descomprimir en directorio temporal
unzip(temp, exdir = tempdir())
# Buscar todos los archivos .shp (recursivamente)
archivo_shp <- list.files(tempdir(), pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
# Leer todos los shapefiles en una lista
lista_shapefiles <- lapply(archivo_shp, st_read)
# Asignar nombres a la lista usando los nombres de archivo (sin extensión)
nombres <- file_path_sans_ext(basename(archivo_shp))
names(lista_shapefiles) <- nombres
# Mostrar nombres de shapefiles disponibles
print(names(lista_shapefiles))
#-----------------------------Acceder a los shapefiles-------------------------
#lista_shapefiles$"tendencia_regional_su25"
#-----------------------------Graficar Tendencia Lineal-------------------------
# Crear carpeta para guardar gráficos
dir.create("graficas_tendencias", showWarnings = FALSE)

# Loop sobre los shapefiles
for (nombre in names(lista_shapefiles)) {
  shapefile <- lista_shapefiles[[nombre]]
  
  # Verifica si contiene la columna TEND_L
  if ("TEND_L" %in% colnames(shapefile)) {
    p <- ggplot(shapefile) +
      geom_sf(aes(fill = TEND_L)) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      theme_minimal() +
      labs(
        title = paste("Tendencia climática:", nombre),
        fill = "TEND_L"
      )
    print(p)
    
    # Guardar la imagen
    ggsave(filename = paste0("graficas_tendencias/", nombre, ".png"), plot = p, width = 8, height = 6)
  } else {
    message(paste("El archivo", nombre, "no tiene la columna TEND_L."))
  }
}
#----------------------Regiones Climaticas a Estados-----------------------------------
asignar_clima_a_estados <- function(nombre_tendencia, lista_shapefiles) {
  # Cargar shapefiles
  tendencia <- lista_shapefiles[[nombre_tendencia]]
  estados <- lista_shapefiles[["Entidades_Federativas"]]
  
  # Unificar sistema de coordenadas
  tendencia <- st_transform(tendencia, st_crs(estados))
  
  # Encontrar la posición de "AÑOS_REG"
  pos_anios <- which(names(tendencia) == "AÑOS_REG")
  
  # Obtener la columna climática 2 posiciones a la derecha
  columna_clima <- names(tendencia)[pos_anios + 2]
  
  # Unión espacial: asigna a cada estado el valor de la región climática
  estados_clima <- st_join(estados, tendencia[, c(columna_clima, "geometry")], 
                           left = TRUE, join = st_intersects)
  
  return(estados_clima)
}



su25_estado <- asignar_clima_a_estados("tendencia_regional_su25", lista_shapefiles)

# Ver resultado
head(su25_estado)

# Mapa simple
plot(su25_estado[colnames(su25_estado)[5]])  # Ajusta el índice si cambia el orden




