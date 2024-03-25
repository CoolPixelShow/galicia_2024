############################
## SERVER GALICIA_2024_08 ##
############################

server <- function(input, output, session) {
  # SET VALUE BOXES FUNCTION
  set_value_boxes <- function(current_provincia, current_municipio){
    # value_box_provincia
    value_box_provincia <- data.frame(map_data_provincias_2024) %>% 
      dplyr::filter(provincia == current_provincia) %>% 
      transmute(votos_totales, votos_blancos, participacion = round(votos_validos / censo_total * 100, 2))
    output$provincia_data <- renderUI(HTML(paste0('Votos totales: ', format(value_box_provincia['votos_totales'], big.mark = ".", decimal.mark = ','), "<br/>",
                                                  'Votos en blanco: ', format(value_box_provincia['votos_blancos'], big.mark = ".", decimal.mark = ','), "<br/>",
                                                  'Participacion: ', value_box_provincia['participacion'],  " %")))
    # value_box_municipio
    value_box_municipio <- data.frame(map_data_municipios_2024) %>% 
      dplyr::filter(municipio == current_municipio) %>% 
      transmute(votos_totales, votos_blancos, participacion = round(votos_validos / censo_total * 100, 2))
    output$municipio_data <- renderUI(HTML(paste0('Votos totales: ', format(value_box_municipio['votos_totales'], big.mark = ".", decimal.mark = ','), "<br/>",
                                                  'Votos en blanco: ', format(value_box_municipio['votos_blancos'], big.mark = ".", decimal.mark = ','), "<br/>",
                                                  'Participacion: ', value_box_municipio['participacion'],  " %")))
  }
  # INITIAL VALUES
  set_value_boxes('Coruña, A', 'A Baña')
  # GET 'provincia' and 'municipio' FROM LEAFLET MAP CLICK
  # Link... https://stackoverflow.com/questions/34095499/how-can-i-pass-info-from-leaflet-popup-to-shiny-output
  # Link... https://rstudio.github.io/leaflet/articles/shiny.html
  observeEvent(input$mapa_ccaa_shape_click, {
    temp <- dplyr::filter(map_data_municipios_2024, LAU_CODE %in% input$mapa_ccaa_shape_click$id)
    current_selection$provincia <- temp$provincia
    current_selection$municipio <- temp$municipio
    set_value_boxes(current_selection$provincia, current_selection$municipio)
  })
  # VALUEBOX_CCAA
  output$boxPlot_00 <- renderEcharts4r({
    mas_votado_ccaa <- data.frame(partido = map_data_ccaa_2024$mas_votado,
                                  puesto = 1,
                                  # Link... https://www.r-bloggers.com/2017/03/which-function-in-r/
                                  logo = escanos$logos[which(escanos$partidos == map_data_ccaa_2024$mas_votado)])
    # showcase
    mas_votado_ccaa |> 
      e_charts(partido) |> 
      e_pictorial(puesto, symbol = logo, symbolSize = c(64, 64)) |> 
      e_legend(show = FALSE) |>
      e_x_axis(show = F, splitLine = list(show = F)) |>
      e_y_axis(show = F, splitLine=list(show = F)) |>
      e_title('MAS VOTADO', left = 'center', textStyle = list(fontSize = 12, color = "#95a5a6")) |>
      e_grid(top = '80%')
  })
  # valuebox_data
  output$ccaa_data <- renderUI(HTML(paste0('Votos totales: ', format(map_data_ccaa_2024$votos_totales, big.mark = ".", decimal.mark = ','), "<br/>",
                                           'Votos en blanco: ', format(map_data_ccaa_2024$votos_blancos, big.mark = ".", decimal.mark = ','), "<br/>",
                                           'Participacion: ', round(map_data_ccaa_2024$votos_validos / map_data_ccaa_2024$censo_total * 100, 2),  " %")))
  # output plot_00
  output$plot_00 <- renderEcharts4r({
    diputados_ccaa <- escanos %>% 
      dplyr::filter(total > 0) %>% 
      arrange(desc(total))
    # Link... https://rquer.netlify.app/infographics/
    diputados_ccaa |> 
      e_charts(partidos) |> 
      e_pictorial(total, symbol = logos, symbolSize = c(64, 64)) |> 
      e_labels(position = 'bottom', textStyle = list(fontSize = 18, fontWeight ='bold')) |>
      e_legend(show = FALSE) |>
      e_x_axis(show = F, splitLine = list(show = F)) |>
      e_y_axis(show = F, splitLine=list(show = F)) |>
      e_title(paste0('DIPUTADOS (total ', sum(map_data_provincias_2024$numero_diputados), ')')) |>
      e_grid(bottom = '20', left = '0%', right = '0%')
  })
  # output plot_01
  # Link... https://echarts.apache.org/en/option.html#series-pie.radius
  output$plot_01 <- renderEcharts4r({
    diputados_ccaa <- escanos %>% 
      dplyr::filter(total > 0) %>% 
      arrange(desc(total)) %>% 
      rbind(c('', '', 0, 0, 0, 0, 0, 75))
    # e_pie
    diputados_ccaa |> 
      e_charts(partidos) |> 
      e_pie(total, radius = c("20%", "75%"), center = c('50%', '100%'), top = '-120%', clockwise = TRUE, startAngle = 180, endAngle = 0, label = list(show = FALSE)) |>
      e_color(color = diputados_ccaa$colores) |>
      e_legend(show = TRUE, icons = list("rect")) |>
      e_tooltip(show = TRUE)
  })
  # output plot_02
  output$plot_02 <- renderReactable({
    # table_ccaa
    map_data_ccaa_2024 <- as.data.frame(map_data_ccaa_2024) %>% 
      select(BNG:VOX) %>% 
      pivot_longer(cols = everything()) %>% 
      transmute(partido = name,
                leyenda = "diamond",
                color = colores,
                '2024' = value) %>% 
      left_join(reactable_data_ccaa_2020) %>% # join_tables
      replace(is.na(.), 0) %>% 
      mutate('2020' = ifelse(`2020` == 0, 0, `2024` - `2020`))
    # reactable
    # Link... https://kcuilla.github.io/reactablefmtr/articles/icon_sets.html
    reactable(
      map_data_ccaa_2024,
      columns = list(partido = colDef(cell = icon_sets(map_data_ccaa_2024, icon_ref = "leyenda", icon_color_ref = "color", icon_position = "left")),
                     # leyenda = colDef(align = "center", width = 60, cell = icon_sets(map_data_ccaa_2024, icon_ref = "leyenda", icon_color_ref = "color", icon_position = "over"))
                     leyenda = colDef(show = F), color = colDef(show = F), # Link... https://stackoverflow.com/questions/69543680/r-reactable-hide-columns-by-default
                     '2024' = colDef(style = list(background = "rgba(0, 0, 0, 0.03)"), width = 70),
                     '2020' = colDef(cell = function(value) {if (value > 0) paste0("+", value) else value},
                                     style = function(value) {color <- if (value > 0) {"#008000"}
                                     else if (value < 0) {"#e00000"}
                                     list(fontWeight = 600, color = color)}, width = 70)),
      compact = TRUE,
      highlight = TRUE,
      pagination = FALSE,
      style = list(fontSize = "0.8rem"),
      defaultSorted = list('2024' = "desc")
      # theme = reactableTheme(headerStyle = list(background = '#2c3e50', color ='white', borderColor = '#555'))
    )
  })
  # output map_ccaa
  output$mapa_ccaa <- renderLeaflet({
    leaflet(map_data_municipios_2024,
            # Hide zoomControl
            # check this... https://stackoverflow.com/questions/36365897/r-leaflet-zoomcontrol-option
            options = leafletOptions(zoomControl = FALSE,
                                     # Control zoom increments
                                     # check this... https://stackoverflow.com/questions/56457592/how-can-i-zoom-in-finer-steps-in-leaflet
                                     zoomSnap = 0.10, scrollWheelZoom = FALSE, dragging = FALSE, doubleClickZoom = FALSE)) %>% 
      # Link... https://stackoverflow.com/questions/34095499/how-can-i-pass-info-from-leaflet-popup-to-shiny-output
      addPolygons(layerId = map_data_municipios_2024$LAU_CODE, smoothFactor = 0.1, fillColor = ~palette_candidaturas(map_data_municipios_2024$mas_votado),
                  stroke = TRUE, weight = 0.5, opacity = 0.8, color = "#222", dashArray = "", fillOpacity = 0.8,
                  highlightOptions = highlightOptions(weight = 3, color = '#dddddd', opacity = 1, dashArray = "", # Outline color and fill
                                                      # fillColor = current_selection$color[[4]], fillOpacity = 0.5, # Polygon fill color and fill
                                                      bringToFront = TRUE, sendToBack = TRUE),
                  label = map_data_municipios_2024$municipio) %>% 
      addPolylines(data = map_data_provincias_2024, weight = 3, opacity = 1, color = '#2c3e50', dashArray = "") %>% 
      # map_title... addControl
      addControl(tags$div(
        # interactive_tag_map_title
        # check this... https://stackoverflow.com/questions/49072510/r-add-title-to-leaflet-map
        # CSS "position" property
        # check this... https://www.w3schools.com/css/css_positioning.asp
        tags$style(HTML(".leaflet-control.map-title {position: relative !important; text-align: center; padding-left: 10px; padding-right: 10px; background: rgba(255,255,255,0.75); font-weight: bold; font-size: 24px;}")),
        HTML(paste0("<p style='color:#2c3e50;font-size:18px;'>18 febrero 2024</p>"))), position = "topleft", className="map-title")
  })
  # output map_provincia
  output$mapa_provincia_municipio <- renderLeaflet({
    map_data_provincias_2024 <- dplyr::filter(map_data_provincias_2024, provincia %in% current_selection$provincia)
    map_data_municipios_2024 <- dplyr::filter(map_data_municipios_2024, municipio %in% current_selection$municipio)
    leaflet(map_data_provincias_2024,
            # Hide zoomControl
            # check this... https://stackoverflow.com/questions/36365897/r-leaflet-zoomcontrol-option
            options = leafletOptions(zoomControl = FALSE, zoomSnap = 0.10)) %>%
      # Background
      # providers... check this... https://leaflet-extras.github.io/leaflet-providers/preview/
      addProviderTiles(providers$CartoDB.Positron, group = "Mapa (fondo)") %>%
      # Link... https://stackoverflow.com/questions/34095499/how-can-i-pass-info-from-leaflet-popup-to-shiny-output
      addPolygons(smoothFactor = 0.1, fillColor = ~palette_candidaturas(map_data_provincias_2024$mas_votado),
                  stroke = TRUE, weight = 2, opacity = 1, color = "#222", dashArray = "", fillOpacity = 0.8) %>% 
      addPolygons(data = map_data_municipios_2024, smoothFactor = 0.1, fillColor = ~palette_candidaturas(map_data_municipios_2024$mas_votado),
                  # stroke = TRUE, weight = 0.5, opacity = 1, color = "#222", dashArray = "", fillOpacity = 0.8)
                  stroke = TRUE, weight = 2, opacity = 1, color = "#dddddd", dashArray = "", fillOpacity = 0.8)
  })
  # VALUEBOX_PROVINCIA
  output$current_provincia <- renderText(current_selection$provincia)
  # output plot_03
  output$plot_03 <- renderEcharts4r({
    diputados_provincia <- escanos %>% 
      # get() to use the value of a variable
      # Link... https://stackoverflow.com/questions/67990556/how-do-i-use-get-in-r-to-access-a-variable-with-another-variable
      dplyr::filter(get(current_selection$provincia) > 0) %>% 
      arrange(desc(get(current_selection$provincia))) %>% 
      transmute(partidos, logos, colores, current_provincia = get(current_selection$provincia))
    # Link... https://rquer.netlify.app/infographics/
    diputados_provincia |> 
      e_charts(partidos) |> 
      e_pictorial(current_provincia, symbol = logos, symbolSize = c(48, 48)) |> 
      e_labels(position = 'bottom', textStyle = list(fontSize = 16, fontWeight ='bold')) |>
      e_legend(show = FALSE) |>
      e_x_axis(show = F, splitLine = list(show = F)) |>
      e_y_axis(show = F, splitLine=list(show = F)) |>
      e_title(paste0(sum(diputados_provincia$current_provincia), ' diputados a elegir')) |>
      e_grid(bottom = '20', left = '0%', right = '0%')
    })
  # output plot_04
  output$plot_04 <- renderReactable({
    # filter reactable_data_provincia_2020
    reactable_data_provincia_2020 <- reactable_data_provincia_2020 %>% 
      dplyr::filter(provincia %in% current_selection$provincia) %>%
      select(BNG:VOX) %>% 
      pivot_longer(cols = everything()) %>% 
      transmute(partido = name,
                '2020' = value)
    # table_provincia
    current_selection$table_provincia <- as.data.frame(map_data_provincias_2024) %>% 
      dplyr::filter(provincia %in% current_selection$provincia) %>%
      select(BNG:VOX) %>% 
      pivot_longer(cols = everything()) %>% 
      transmute(partido = name, '2024' = value) %>% 
      left_join(reactable_data_provincia_2020) %>% # join_tables
      replace(is.na(.), 0) %>% 
      mutate('2020' = ifelse(`2020` == 0, 0, `2024` - `2020`))
    # reactable
    reactable(
      current_selection$table_provincia,
      columns = list('2024' = colDef(style = list(background = "rgba(0, 0, 0, 0.03)"), width = 60),
                     '2020' = colDef(cell = function(value) {if (value > 0) paste0("+", value) else value},
                                     style = function(value) {color <- if (value > 0) {"#008000"}
                                     else if (value < 0) {"#e00000"}
                                     list(fontWeight = 600, color = color)}, width = 60)),
      compact = TRUE,
      wrap = FALSE,
      highlight = TRUE,
      pagination = FALSE,
      # defaultPageSize = 8,
      # paginationType = 'simple',
      style = list(fontSize = "0.8rem"),
      defaultSorted = list('2024' = "desc")
      # theme = reactableTheme(headerStyle = list(background = '#95a5a6', color ='white', borderColor = '#555'))
    )
  })
  # VALUEBOX_MUNICIPIO
  output$current_municipio <- renderText(current_selection$municipio)
  
  # output plot_05
  output$plot_05 <- renderReactable({
    # filter reactable_data_provincia_2020
    reactable_data_municipio_2020 <- data_galicia_raw_2020 %>% 
      dplyr::filter(municipio %in% current_selection$municipio) %>%
      select(BNG:VOX) %>% 
      pivot_longer(cols = everything()) %>% 
      transmute(partido = name,
                '2020' = value)
    # table_municipio
    current_selection$table_municipio <- as.data.frame(map_data_municipios_2024) %>% 
      dplyr::filter(municipio %in% current_selection$municipio) %>%
      select(BNG:VOX) %>% 
      pivot_longer(cols = everything()) %>% 
      transmute(partido = name, '2024' = value) %>% 
      left_join(reactable_data_municipio_2020) %>% # join_tables
      replace(is.na(.), 0) %>% 
      mutate('2020' = ifelse(`2020` == 0, 0, `2024` - `2020`))
    # reactable
    reactable(
      current_selection$table_municipio,
      columns = list('2024' = colDef(style = list(background = "rgba(0, 0, 0, 0.03)"), width = 60),
                     '2020' = colDef(cell = function(value) {if (value > 0) paste0("+", value) else value},
                                     style = function(value) {color <- if (value > 0) {"#008000"}
                                     else if (value < 0) {"#e00000"}
                                     list(fontWeight = 600, color = color)}, width = 60)),
      compact = TRUE,
      wrap = FALSE,
      highlight = TRUE,
      pagination = FALSE,
      style = list(fontSize = "0.8rem"),
      defaultSorted = list('2024' = "desc")
      # theme = reactableTheme(headerStyle = list(background = '#ecf0f1', color ='black', borderColor = '#555'))
    )
  })
}
