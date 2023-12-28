
source("ETL/99-functions.R")
source("ETL/00-libraries.R")
source("ETL/02-transform.R")



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Análisis de flujo - EPH"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "category",
                  label = "Categoría de flujo", 
                  choices = c("Ocupado", "Desocupado", "Inactivo")
      ),
      
      selectInput(inputId = "periodo_base",
                  label = "Período base", 
                  choices = c("t_anterior", "t_posterior")
      ),
      
      selectInput(inputId = "anio_ant",
                  label = "Año 0", 
                  choices = 2021:2022, 
                  selected = 2021
      ),
      
      selectInput(inputId = "anio_post",
                  label = "Año 1", 
                  choices = 2021:2022, 
                  selected = 2021
      ),
      
      selectInput(inputId = "trimestre_ant",
                  label = "Trimestre 0", 
                  choices = 1:4, 
                  selected = 1
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      highchartOutput("sankey") 
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observe({
  anio_ant <- as.numeric(input$anio_ant)
  anio_post <- ifelse(as.numeric(input$trimestre_ant) %in% c(1:3), as.numeric(input$anio_ant), as.numeric(input$anio_ant) + 1)
  trim_ant <- as.numeric(input$trimestre_ant)
  trim_post <-ifelse(as.numeric(input$trimestre_ant) %in% c(1:3), as.numeric(input$trimestre_ant) + 1, 1)
  
  
  ### Armo la base de panel
  df_eph_panel <- reactive({
    armo_base_panel(anio_0 = anio_ant, 
                    trimestre_0 = trim_ant,
                    anio_1 = anio_post, 
                    trimestre_1 = trim_post)
    
  })
  
  # ### Armo el tabulado
  # tabla_armada <- preparo_base(df = df_eph_panel, 
  #                              periodo_base = "t_anterior")
  # 
  # tabla_sankey <- armo_tabla_sankey(table = tabla_armada, categoria = "Desocupado")
  # 
  # 
  output$sankey <- renderHighchart({
    highcharter::hchart(
      armo_tabla_sankey(
        table = preparo_base(
          df = df_eph_panel(), 
          periodo_base = input$periodo_base), 
        categoria = input$category),
      "sankey", 
      name = "Gender based Outcomes"
    )
  })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
