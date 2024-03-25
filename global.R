############################
## GLOBAL GALICIA_2024_08 ##
############################

library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(data.table)
library(htmltools)

library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)

library(mapSpain)
library(sf)
library(leaflet)

library(coalitions)

library(ggplot2)
library(echarts4r)
library(reactablefmtr)

# LOAD DATA
load("data/map_data_municipios_2024.Rda")
load("data/map_data_provincias_2024.Rda")
load("data/map_data_ccaa_2024.Rda")
# ---------------------------------
load("data/data_galicia_raw_2020.Rda")

# reactiveValues()
current_selection <- reactiveValues()
current_selection$provincia <- 'Coruña, A'
current_selection$municipio <- 'A Baña'


# partidos
partidos = as.vector(colnames(map_data_ccaa_2024)[12:22])
# logos
logos = c('image://bng_64x64.png', 'image://do_64x64.png', 'image://ecg_64x64.png', 'image://escanos_en_branco_64x64.png', 'image://pacma_64x64.png',
          'image://podemos_galicia_64x64.png', 'image://pp_64x64.png', 'image://psoe_psg_64x64.png', 'image://pum_j_64x64.png', 'image://sumar_galicia_64x64.png',
          'image://vox_64x64.png')
# colores
colores = c('#76b2e1', '#ffcf00', '#00803f', '#be823d', '#a0a000', '#9a6fff', '#0456a1', '#e10a18', '#f6c293', '#eb7399', '#01c11a')
# palette
palette_candidaturas <- colorFactor(
  palette = colores,
  levels = partidos
)

#####################
## REPARTO d'Hondt ##
#####################
# bind_cols... numero_diputados
map_data_provincias_2024 <- bind_cols(map_data_provincias_2024, numero_diputados = c(25, 14, 14, 22))
# Link... ESPANA... https://es.wikipedia.org/wiki/Elecciones_en_Espa%C3%B1a
# Link... AUTONOMICAS... https://es.wikipedia.org/wiki/Elecciones_auton%C3%B3micas_en_Espa%C3%B1a
escanos <- data.frame(partidos = partidos,
                      logos = logos,
                      colores = colores,
                         lapply(rownames(map_data_provincias_2024), function(x){
                           dHondt(as.data.frame(map_data_provincias_2024[x, 12:22]) %>%
                           select(1:11) %>%
                           as.numeric(),
                           partidos, n_seats = map_data_provincias_2024$numero_diputados[as.numeric(x)])
                           })) 
escanos$total <- rowSums(escanos[, 4:7], na.rm = TRUE)
colnames(escanos)[4:7] <- map_data_provincias_2024$provincia

#########################
## reactable 2020 data ##
#########################
# reactable_data_ccaa_2020
reactable_data_ccaa_2020 <- data_galicia_raw_2020 %>% 
  dplyr::group_by(ccaa) %>% 
  summarise_at(vars(BNG:VOX), sum, na.rm = TRUE) %>% 
  select(BNG:VOX) %>%  
  pivot_longer(cols = everything()) %>% 
  transmute(partido = name,
            '2020' = value)
# data_provincia_2020
reactable_data_provincia_2020 <- data_galicia_raw_2020 %>% 
  dplyr::group_by(provincia) %>% 
  summarise_at(vars(BNG:VOX), sum, na.rm = TRUE) %>% 
  select(provincia, BNG:VOX)
