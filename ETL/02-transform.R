

# df_eph <- read_parquet("data_raw/df_eph.parquet") |> 
#   select(all_of(variables)) |> 
#   collect()
# 
# ### Creo lista para insumo de panel
# list_eph_2023t1_2 <- list(df_eph2023t1, df_eph2023t2)
# 
# ### Armo panel
# df_eph_panel <- organize_panels(bases = list_eph_2023t1_2, 
#                                 variables = c("ESTADO", "PONDERA"), 
#                                 window = "trimestral")


#armo_sankey(table = tabla_sankey)


### Preparo base df_cond_act:
periodo_categ <- unique(df_cond_act$periodo)

df_cond_act <- df_cond_act |> 
  mutate(periodo = factor(periodo, level = periodo_categ))



### ### Filtros para gráfico de flujo
filter_sankey_anio_ant <- selectInput(inputId = "anio_ant",
              label = "Año del panel",
              choices = 2003:2023,
              selected = 2022
  )

  # selectInput(inputId = "anio_post",
  #             label = "Año 1",
  #             choices = 2021:2022,
  #             selected = 2021
  # ),
  
filter_sankey_trim_ant <- selectInput(inputId = "trimestre_ant",
              label = "Trimestre inicial (del panel)",
              choices = 1:4,
              selected = 1
  )
  
filter_sankey_categoria <- selectInput(inputId = "category",
              label = "Categoría de base (el 100%)",
              choices = c("Ocupado", "Desocupado", "Inactivo")
  )
  
filter_sankey_periodo_base <- selectInput(inputId = "periodo_base",
              label = "Sentido (trimestre base)",
              choices = c("Trimestre anterior" = "t_anterior",
                          "Trimestre posterior" = "t_posterior")
  )


filters_sankey <- list(
 filter_sankey_anio_ant,
 filter_sankey_categoria,
 filter_sankey_periodo_base,
 filter_sankey_trim_ant
)


### Filtros para gráfico de línea
filter_line_desde <-  selectInput(inputId = "desde",
                                  label = "Desde",
                                  choices = c("Ocupación" = "Ocupado_tant", 
                                              "Desocupación" = "Desocupado_tant",
                                              "Inactividad" = "Inactivo_tant"), 
                                  selected = "Desocupado_tant"
)

filter_line_hacia <-   selectInput(inputId = "hacia",
                                   label = "Hacia",
                                   #choices = unique(df_cond_act$to),
                                   choices = c("Ocupación" = "Ocupado_tpost", 
                                               "Desocupación" = "Desocupado_tpost",
                                               "Inactividad" = "Inactivo_tpost"),
                                   selected = "Ocupado_tpost",
                                   multiple = TRUE
)

filters_line <- list(
  filter_line_desde,
  filter_line_hacia
)  
