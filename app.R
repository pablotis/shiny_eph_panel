
source("ETL/99-functions.R")
source("ETL/00-libraries.R")
source("ETL/01-extract.R")
source("ETL/02-transform.R")

waiting_screen <- tagList(
  spin_flower(),
  h4("Bancame un toque...")
) 

# Define UI for application that draws a histogram
ui <- page_navbar(
  useWaitress(color = "#7F7FFF"),
  title = "Análisis de flujo - EPH",
  bg = "white",
  underline = TRUE,
  
    nav_panel(
    title = "Sobre la App", 
            card_body(
              h1("Racondo sobre la aplicación"),
            p(
              em("La Encuesta Permanente de Hogares"), 
              "es una de las fuentes de información sociodemográfica más importante del Sistema Estadístico Nacional (SEN) Argentino.
              Si bien este operativo es más conocido por la tan famosa (y muchas veces mal utilizada/interpretada) Tasa de Desocupación [1], el abanico de indicadores que se pueden obtener para caracterizar las condiciones de vida de la población es basto."
              ),
            
            p("La metodología más frecuente (y por la que fue diseñada) para esta caracterización es la de ver en clave de 'foto' la situación de las y los argentinos respecto (principalmente) a su inserción en el mercado de trabajo. 
              Por ejemplo, puedo conocer, para el primer trimestre del año 2023, qué proporción de personas habían realizado una actividad laboral (ocupadas), qué proporción se encontraba buscando trabajo (desocupación) y cuántas ni estaban ocupadas ni desocupadas (inactividad)."
              ),
            
            p(
              "Pero esta encuesta presenta una peculiaridad, que es la de permitir un análisis en clave de 'película', esto es, conocer la evolución de un indicador en el tiempo. 
              Por ejemplo, quiero saber si la población ocupada que entrevisté en el primer trimestre del 2023 se encuentra en la misma situación o la ha modificado (pasó a la desocupación o inactividad)"
            )
            )
  ),
  nav_panel(
    autoWaiter(html = waiting_screen, color = "black"),
    title = "Foto", 
            fluidRow(
              column(filter_sankey_anio_ant, width = 2),
              column(filter_sankey_trim_ant, width = 2),
              column(filter_sankey_categoria, width = 2),
              column(filter_sankey_periodo_base, width = 2)
            ),
            card(full_screen = TRUE,
              #card_header("Flujo de la Condición de Actividad - FOTO"),
              #layout_sidebar(sidebar = filters),
              #!!!filters_sankey,
              highchartOutput("sankey")
              
              )
  ),
  nav_panel(title = "Película", 
            fluidRow(
              column(filter_line_desde, width = 3),
              column(filter_line_hacia, width = 3)  
            ),
            card(
              #card_header("Flujo de la Condición de Actividad - PELÍCULA"),
             # !!!filters_line,
              highchartOutput("line"),
              #plotOutput("line"),
             mainPanel(
                textOutput("text")
              )
  )
  ),
  nav_spacer(),
  nav_menu(
    title = "+Info",
    nav_item(a("La nueva EPH", href = "https://www.indec.gob.ar/ftp/cuadros/sociedad/metodologia_eph_continua.pdf")),
    nav_item("Metodología de Panel")
  ),
  ?nav_spacer(),
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$anio_ant, {
    if (input$anio_ant == 2016) {
      warning("choose another option")
    }
  }
  )
  
  observe({

    anio_ant <- as.numeric(input$anio_ant)
    anio_post <- ifelse(as.numeric(input$trimestre_ant) %in% c(1:3), as.numeric(input$anio_ant), as.numeric(input$anio_ant) + 1)
    trim_ant <- as.numeric(input$trimestre_ant)
    trim_post <-ifelse(as.numeric(input$trimestre_ant) %in% c(1:3), as.numeric(input$trimestre_ant) + 1, 1)
    
    sentido <- input$periodo_base
    categoria_lab <- ifelse(input$category == "Ocupado", "Ocupación", 
                            ifelse(input$category == "Desocupado", "Desocupación","Inactividad"))
    
    
    
    ### Armo la base de panel
    df_eph_panel <- reactive({
      armo_base_panel(anio_0 = anio_ant, 
                      trimestre_0 = trim_ant,
                      anio_1 = anio_post, 
                      trimestre_1 = trim_post)
      
    })
    
    output$sankey <- renderHighchart({
      highcharter::hchart(
        object = armo_tabla_sankey(
          table = preparo_base(
            df = df_eph_panel(), 
            periodo_base = input$periodo_base), 
          categoria = input$category),
        "sankey", 
        name = ifelse(sentido == "t_anterior", 
                      glue::glue("Flujo desde la {categoria_lab}"),
                      glue::glue("Flujo hacia la {categoria_lab}"))
      ) |> 
        hc_title(text = "Flujo de la condición de actividad.") |> 
        hc_subtitle(text = glue(
          "Panel {ifelse(trim_ant %in% 1:3, paste0(anio_ant, ' - ', 'trimestre ', trim_ant, ' y ', trim_post), 
        paste0(anio_ant, ' - ', 'trimestre ', trim_ant, ' y ', anio_ant + 1, ' trimestre ', trim_post))}")) |> 
        hc_caption(text = "Fuente: Elaboración propia en base a la EPH-INDEC") |> 
        hc_add_theme(hc_theme_smpl())
    })
    
    output$line <- renderHighchart({
      hchart(df_cond_act |> 
               filter(from == input$desde, to %in% input$hacia) |> 
               mutate(to = case_when(
                 from == "Desocupado_tant" & to == "Inactivo_tpost" ~ "% de Desocupados que pasan a la Inactividad:",
                 from == "Desocupado_tant" & to == "Desocupado_tpost" ~ "% de Desocupados que pasan a la Desocupación:",
                 from == "Desocupado_tant" & to == "Ocupado_tpost" ~ "% de Desocupados que pasan a la Ocupación:",
                 from == "Ocupado_tant" & to == "Inactivo_tpost" ~ "% de Ocupados que pasan a la Inactividad:",
                 from == "Ocupado_tant" & to == "Desocupado_tpost" ~ "% de Ocupados que pasan a la Desocupación:",
                 from == "Ocupado_tant" & to == "Ocupado_tpost" ~ "% de Ocupados que pasan a la Ocupación:",
                 from == "Inactivo_tant" & to == "Inactivo_tpost" ~ "% de Inactivos que pasan a la Inactividad:",
                 from == "Inactivo_tant" & to == "Desocupado_tpost" ~ "% de Inactivos que pasan a la Desocupación:",
                 from == "Inactivo_tant" & to == "Ocupado_tpost" ~ "% de Inactivos que pasan a la Ocupación:")),
             "line", 
             hcaes(periodo, weight, group = to)) |> 
        hc_add_theme(hc_theme_smpl()) |> 
        hc_tooltip(
          pointFormat = "<span  style='color: {series.color}'> {series.name} <b>{point.y}</b><br/></span>",
          shadow = TRUE,
          backgroundColor = "white",
          style = list(textOutline = "3px #00000"),
          borderColor = "red",
          borderWidth = 0
        ) |> 
        hc_annotations(
          list(
            labelOptions = list(
              shape = "connector",
              align = "right",
              justify = FALSE,
              crop = TRUE,
              style = list(fontSize = "0.8em", textOutline = "1px white")
            ),
            labels = list(
              list(point = list(x = 11, y = 40, xAxis = 0, yAxis = 0), 
                   text = "Comienzo de la Pandemia")
            )
          )
        )|>  
        hc_caption(
          text = "Elaboración propia en base a la EPH-INDEC."
        )
    })
    
  })
  
  
  
  output$text <- renderPrint({
    trim_ant <- substr(last(df_cond_act$periodo), 7, 7)
    anio_ant <- substr(last(df_cond_act$periodo), 1, 4)
    trim_post <- substr(last(df_cond_act$periodo), 10, 10)
    anio_post <- substr(last(df_cond_act$periodo), 9, 12)
    
    dato <- df_cond_act |>
      filter(from == "Desocupado_tant", to == "Ocupado_tpost", periodo == "2023_t1-t2") |>
      pull(weight)

    glue("> ¿Cómo se lee?: \nEjemplo: Si el panel en el eje x es '2023_t1-t2', la interpretación sería: Entre la población que se encontraba desocupada en el trimestre {trim_ant} del año {anio_ant},
         el {dato}% pasó a la Ocupación para el trimestre {trim_post} del mismo año.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
