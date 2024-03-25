########################
## UI GALICIA_2024_08 ##
########################

ui <- page_fillable(
  # Link... https://stackoverflow.com/questions/77581592/best-way-to-change-bslib-value-box-title-text-size
  tags$head(tags$style(HTML('.bslib-value-box .value-box-title {font-size: 24px; font-style: italic;}'))),
  # BOOTSTRAP THEMES... Link... https://bootswatch.com/
  # THEMING... Link... https://rstudio.github.io/bslib/articles/theming/
  theme = bs_theme(version = 5, bootswatch = "flatly", `enable-rounded` = FALSE),
  title = "Galicia 2024",
  # Link... https://stackoverflow.com/questions/24705431/how-can-i-insert-an-image-into-the-navbar-on-a-shiny-navbarpage
  # LAYOUT
  layout_column_wrap(
    # Link... https://rstudio.github.io/bslib/articles/column-layout/index.html
    style = css(grid_template_columns = "2fr 1fr"),
    # -----------------------------
    card(
      full_screen = FALSE,
      height = '100',
      card_header('CCAA'),
      layout_column_wrap(
        style = css(grid_template_columns = "1fr 2fr"),
        layout_column_wrap(
          style = css(grid_template_rows = "0.8fr 0.65fr 1fr 1.95fr"),
          value_box(title = "GALICIA", value = p(htmlOutput("ccaa_data")),
                    showcase = echarts4rOutput("boxPlot_00"), theme = "primary",
                    showcase_layout = showcase_left_center(width = "140px", width_full_screen = 0.8, max_height = "140px", max_height_full_screen = 0.8)),
          echarts4rOutput("plot_00"),
          echarts4rOutput("plot_01"),
          reactableOutput("plot_02")
        ),
        leafletOutput("mapa_ccaa")
      )
    ),
    layout_column_wrap(
      width = '100%',
      card(
        full_screen = FALSE,
        card_header('PROVINCIA / MUNICIPO'),
        leafletOutput("mapa_provincia_municipio"),
        layout_column_wrap(
          layout_column_wrap(
            style = css(grid_template_rows = "0.5fr 0.4fr 1.4fr"),
            value_box(title = textOutput('current_provincia'), value = p(htmlOutput("provincia_data")), theme = "secondary"),
            echarts4rOutput("plot_03"),
            reactableOutput("plot_04")
          ),
          layout_column_wrap(
            style = css(grid_template_rows = "0.5fr 1.65fr 0.15fr"),
            value_box(title = textOutput('current_municipio'), value = p(htmlOutput("municipio_data")), theme = "light"),
            reactableOutput("plot_05"),
            div(style="display:flex;",
                p('Datos ', a("aqui", href="https://resultados2024.xunta.es/es/descargas", target="_blank", style="text-decoration:none"),
                  '|',
                  a("LinkedIn", href="https://www.linkedin.com/services/page/541338328a6084306a/", target="_blank", style="text-decoration:none")),
                capture::capture_pdf(selector = "body", filename = "informe", icon("camera"), "PDF",
                                     format = "png", style = "border-color: #485051; margin-left: auto;"))
          )
        )
      )
    )
  )
)
