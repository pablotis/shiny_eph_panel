
source("ETL/99-functions.R")
#source("ETL/00-libraries.R")
source("ETL/01-extract.R")
source("ETL/02-transform.R")


### Libraries
library(dplyr)
#library(ggplot2)
library(eph)
library(shiny)
library(highcharter)
library(arrow)
library(glue)
library(bslib)
library(bsicons)
library(gghighlight)
library(waiter)


waiting_screen <- tagList(
  spin_flower(),
  h4("Bancame un toque...")
) 

library(shinyalert)

# Define UI for application that draws a histogram
ui <- page_navbar(
  
  useWaitress(color = "#7F7FFF"),
  title = "Análisis de flujo - EPH",
  bg = "white",
  underline = TRUE,
  
  nav_panel(
    title = "Sobre la App", 
    card(
      class = "bg-dark",
      #padding = "20px", gap = "20px",
      br(),
      tags$blockquote("En la presente aplicación se va a poder estudiar el comportamiento del mercado de trabajo, específicamente la variable ", strong("Condición de Actividad,"), " bajo la estrategia de análisis de panel. 
                      Pero antes, hablemos un poco de la fuente de información que permite hacerlo, hablemos de la E-P-H."
      ),
      
      #br(),
      
      h1("La E-P-H"),
      p(
        strong(em("La Encuesta Permanente de Hogares")), 
        "es una de las fuentes de información sociodemográfica más importante del", a("Sistema Estadístico Nacional (SEN)", href = "https://www.indec.gob.ar/indec/web/Institucional-Indec-SistemaEstadistico"), "Argentino.
              Si bien este operativo es más conocido por la estimación de las principales Tasas del Mercado de Trabajo (Ocupación, Desocupación e Inactividad), el abanico de indicadores que se pueden obtener para caracterizar las condiciones de vida de la población es muy amplio."
      ),
      
      p("Para estudiar al mercado de trabajo a través de los datos que provee la EPH, dos son las estrategias de análisis plausibles de abordar. 
        La primera es el", strong("Análisis Transversal."), "Este abordaje implica caracterizar al conjunto de la población en un momento determinado, es sacarle una 'foto'. Este es el abordaje principal y para el cual fue diseñada la encuesta, aunque no el único."
      ),
      
      p("Una segunda manera de interpretar la información es mediante el", strong("Anáisis Longitudinal."), "Aquí la lectura es en clave de 'película'. Esto es, para una misma población, observo su evolución en el tiempo respecto al indicador seleccionado.
      Para ejemplificar, bajo este análisis puedo saber si la misma población ocupada que entrevisté en el primer trimestre del 2023 se encuentra en igual situación o la ha modificado (pasó a la desocupación o inactividad) en el trimestre siguiente"
      ),
      
      br(),
      h4("Condiciones de posibilidad para abordar un Análisis longitudinal de la EPH."),
      
      p("Como es sabido, toda encuesta tiene por detras (o debería tener) un diseño muestral. Este es el que le da sustento metodológico para poder utilizar luego la información que se provee. Es el que permite ilustrar qué puedo analizar y qué no con este operativo.
      En el caso de la EPH, su ", a("diseño muestral", href = "https://www.indec.gob.ar/ftp/cuadros/sociedad/metodologia_eph_continua.pdf"), "define que una parte del conjunto de hogares a encuestar se renueve entre trimestre y trimestre. La forma en la que se produce esta renovación 
      se conoce como ", strong("esquema de rotación."),
      "En pocas palabras, el esquema de rotación de la EPH es el '2-2-2' e implica que una vivienda es seleccionada para ser entrevistada 4 veces. En una primera instancia participa del operativo durante los primeros", strong("dos "), "trimestres de forma consecutiva, descansa los", strong("dos "), "trimestres siguientes y vuelve a participar por", strong("dos "), "trimestres más, para finalmente salir de la muestra y no volver a ser seleccionada.
      "),
      
      p("Al disponer de un esquema como el descripto, la muestra permite reconocer entre dos períodos a una misma población y poder así describir la evolución de ciertas variables enl tiempo. 
        Del total de una muestra trimestral, el análisis de panel (longitudinal) bajo el esquema de rotación 2-2-2 permite (teóricamente) observar la situación del 50% de la muestra para trimestres consecutivos (ejemplo, trimestre 1 y 2 del 2022) y para un mismo trimestre de años consecutivo (trimestre 1 del año 2022 y 2023)"
      ),
      
      p("Con esta breve introducción pasaremos a analizar la evolución en el tiempo de la población en cuanto a la Condición de Actividad.")
    )
  ),
  
  nav_panel(
    title = "Panel (puntual)", 
    fluidRow(
      column(filter_sankey_anio_ant, width = 3),
      column(filter_sankey_trim_ant, width = 3),
      column(filter_sankey_categoria, width = 3),
      column(filter_sankey_periodo_base, width = 3)
    ),
    layout_columns(
      col_widths = c(4,8),
      value_box(
        title = textOutput("pob"),
        value =  textOutput("pob_n"),
        showcase = bs_icon("activity"),
        p(textOutput("periodo"))
      ),
      
      card(
        autoWaiter(
          #html = waiting_screen, color = "black"
          color = "black"
        ),
        full_screen = TRUE,
        highchartOutput("sankey")
      )
      
    )
  ),
  nav_panel(title = "Panel (serie histórica)", 
            fluidRow(
              column(filter_line_desde, width = 3),
              column(filter_line_hacia, width = 3),
              actionButton(
                "btn_pop", 
                "¿Cómo se interpreta el dato?"
              ) |>
                popover(title = "Ejemplo de lectura",
                        
                        p("Si las opciones fijadas son:",
                          br(), 
                          strong("Desde:"), "Desocupado",
                          br(),
                          strong("Hacia:"), "Ocupación",
                          br(),
                          "Y el panel en el eje x es", strong('2023_t1-t2'), ", la interpretación sería:",
                          br(),
                          br(),
                          em("Entre la población que se encontraba desocupada en el trimestre 1 del año 2023, el 44% pasó a la Ocupación para el trimestre 2 del mismo año")
                        )
                )
            ),
            card(
              #card_header("Flujo de la Condición de Actividad - PELÍCULA"),
              # !!!filters_line,
              highchartOutput("line"),
              #plotOutput("line"),
              mainPanel(
                
                
                #textOutput("text")
              )
            )
  ),
  nav_spacer(),
  nav_menu(
    title = "+Info",
    nav_item(a("Documento metodológico: La nueva EPH", href = "https://www.indec.gob.ar/ftp/cuadros/sociedad/metodologia_eph_continua.pdf")),
    nav_item(a("Paquete {eph}", href = "https://docs.ropensci.org/eph/")),
  ),
  nav_spacer(),
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  shinyalert(
    title = "Buenas!",
    text = "Esta aplicación está en desarrollo. Si algo no está funcionando, se puede mejorar o incluso tenés una idea para agregar, podés escribirme a pablotisco@gmail.com",
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "warning",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "JOYA",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  observe({
    
    anio_ant <- as.numeric(input$f_anio_ant)
    anio_post <- ifelse(as.numeric(input$f_trimestre_ant) %in% c(1:3), as.numeric(input$f_anio_ant), as.numeric(input$f_anio_ant) + 1)
    trim_ant <- as.numeric(input$f_trimestre_ant)
    trim_post <-ifelse(as.numeric(input$f_trimestre_ant) %in% c(1:3), as.numeric(input$f_trimestre_ant) + 1, 1)
    
    periodo_filter <- glue::glue("{anio_ant}_t{trim_ant}-t{trim_post}")
    sentido <- input$f_periodo_base
    categoria <- input$f_category
    categoria_lab <- ifelse(input$f_category == "Ocupado", "Ocupación", 
                            ifelse(input$f_category == "Desocupado", "Desocupación","Inactividad"))
    
    output$pob <- renderText({
      paste("Población: ", ifelse(categoria_lab == "Ocupación", "Ocupada",
                                  ifelse(categoria_lab == "Desocupación", "Desocupada", "Inactiva")))
    })
    
    output$pob_n <- renderText({
      data <- read_parquet("data_output/df_tasas_mt.parquet") |> 
        filter(ANO4 == anio_ant & TRIMESTRE == ifelse(sentido == "t_anterior", trim_ant, trim_post)) |> 
        pull(ifelse(categoria_lab == "Ocupación", pob_ocupada,
                    ifelse(categoria_lab == "Desocupación", pob_desocupada, pob_inactiva)))
      
      format(data, big.mark = ".", decimal.mark = ",")
    })
    
    output$periodo <- renderText({
      paste("Año ", anio_ant, ", trimestre ", ifelse(sentido == "t_anterior", trim_ant, trim_post))
    })
    
    ### Armo la base de panel
    # df_eph_panel <- reactive({
    #   armo_base_panel(anio_0 = anio_ant, 
    #                   trimestre_0 = trim_ant,
    #                   anio_1 = anio_post, 
    #                   trimestre_1 = trim_post)
    #   
    # })
    
    df_eph_panel <- reactive({
      read_csv_arrow("data_output/panel_cond_act_historico_test.csv") |> 
        filter(periodo == periodo_filter,
               periodo_base == sentido)
    })
      
    output$sankey <- renderHighchart({
      highcharter::hchart(
        object = armo_tabla_sankey(table = df_eph_panel(),
                                   categoria_t = categoria),
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
               filter(periodo_base == "t_anterior") |> 
               filter(from == input$f_desde, to %in% input$f_hacia) |> 
               mutate(to = case_when(
                 from == "Desocupado_t0" & to == "Inactivo_t1" ~ "% de Desocupados que pasan a la Inactividad:",
                 from == "Desocupado_t0" & to == "Desocupado_t1" ~ "% de Desocupados que pasan a la Desocupación:",
                 from == "Desocupado_t0" & to == "Ocupado_t1" ~ "% de Desocupados que pasan a la Ocupación:",
                 from == "Ocupado_t0" & to == "Inactivo_t1" ~ "% de Ocupados que pasan a la Inactividad:",
                 from == "Ocupado_t0" & to == "Desocupado_t1" ~ "% de Ocupados que pasan a la Desocupación:",
                 from == "Ocupado_t0" & to == "Ocupado_t1" ~ "% de Ocupados que pasan a la Ocupación:",
                 from == "Inactivo_t0" & to == "Inactivo_t1" ~ "% de Inactivos que pasan a la Inactividad:",
                 from == "Inactivo_t0" & to == "Desocupado_t1" ~ "% de Inactivos que pasan a la Desocupación:",
                 from == "Inactivo_t0" & to == "Ocupado_t1" ~ "% de Inactivos que pasan a la Ocupación:")),
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
  # 
  # output$text <- renderPrint({
  #   trim_ant <- substr(last(df_cond_act$periodo), 7, 7)
  #   anio_ant <- substr(last(df_cond_act$periodo), 1, 4)
  #   trim_post <- substr(last(df_cond_act$periodo), 10, 10)
  #   anio_post <- substr(last(df_cond_act$periodo), 9, 12)
  #   
  #   dato <- df_cond_act |>
  #     filter(from == "Desocupado_tant", to == "Ocupado_tpost", periodo == "2023_t1-t2") |>
  #     pull(weight)
  #   
  #   
  #   glue("> ¿Cómo se lee?: Ejemplo: Si el panel en el eje x es '2023_t1-t2', la interpretación sería: Entre la población que se encontraba desocupada en el trimestre {trim_ant} del año {anio_ant},
  #        el {dato}% pasó a la Ocupación para el trimestre {trim_post} del mismo año.")
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
