armo_base_panel <- function(anio_0, trimestre_0, anio_1, trimestre_1){
  
  variables <- c("CODUSU", "NRO_HOGAR", "COMPONENTE", "ANO4", "TRIMESTRE", "CH04", "CH06", "ESTADO", "PONDERA")
  
  df_eph <- read_parquet("data_raw/df_eph.parquet") |> 
    select(all_of(variables)) |> 
    collect()
  
  #rownames(df_eph) <- NULL
  
  ### Creo lista para insumo de panel
  list_eph_panel <- list(
    df_eph |> filter(ANO4 == anio_0 & TRIMESTRE == trimestre_0),
    df_eph |> filter(ANO4 == anio_1 & TRIMESTRE == trimestre_1))
  
  ### Armo panel
  df_eph_panel <- organize_panels(bases = list_eph_panel, 
                                  variables = c("ESTADO", "PONDERA"), 
                                  window = "trimestral")
  
  return(df_eph_panel)
}


######################################################
preparo_base <- function(df, periodo_base = "t_posterior"){
  
  assertthat::assert_that(periodo_base %in% c("t_anterior", "t_posterior"),
                          msg = "Las opciones válidas son 't_posterior' o 't_anterior'")
  
  tabla <- df |> 
    select(ESTADO, ESTADO_t1, PONDERA, PONDERA_t1) |>
    mutate(ESTADO = case_when(ESTADO == 1 ~ "Ocupado_tant",
                              ESTADO == 2 ~ "Desocupado_tant",
                              ESTADO == 3 ~ "Inactivo_tant",
                              ESTADO == 4 ~ "Trab_familiar_tpost"),
           ESTADO_t1 = case_when(ESTADO_t1 == 1 ~ "Ocupado_tpost",
                                 ESTADO_t1 == 2 ~ "Desocupado_tpost",
                                 ESTADO_t1 == 3 ~ "Inactivo_tpost",
                                 ESTADO_t1 == 4 ~ "Trab_familiar_tpost"))
  
  if(periodo_base == "t_anterior"){
    tabla <- tabla |> 
      summarise(casos = sum(PONDERA),
                .by = c("ESTADO", "ESTADO_t1")) |> 
      group_by(ESTADO) |> 
      mutate(porc_base = round(casos / sum(casos) * 100, 1),
             periodo_base = "t_anterior",
             id = paste(ESTADO, ESTADO_t1, sep = " - ")) |> ungroup() |> 
      select(ESTADO, ESTADO_t1, porc_base, id, periodo_base)
  }
    
  if(periodo_base == "t_posterior"){ 
    tabla <- tabla |> 
      summarise(casos = sum(PONDERA_t1),
                .by = c("ESTADO_t1", "ESTADO")) |> 
      group_by(ESTADO_t1) |> 
      mutate(porc_base = round(casos / sum(casos) * 100, 1),
             periodo_base = "t_posterior",
             id = paste(ESTADO, ESTADO_t1, sep = " - ")) |> ungroup() |> 
      select(ESTADO, ESTADO_t1, porc_base, id, periodo_base)
  } 
  
  return(tabla)
}

### Test
#test <- preparo_base(df = df_eph_panel, periodo_base = "t_anterior")

armo_tabla_sankey <- function(table, categoria_t){
  
  if(unique(table$periodo_base) == "t_anterior"){
    periodo_t <- "t0"}
  
  if(unique(table$periodo_base) == "t_posterior"){
    periodo_t <- "t1"
  }
  
  names(table) <- c("from", "to", "weight", "id", "periodo_base", "categoria", "periodo")

  if(unique(table$periodo_base) == "t_anterior"){
    tabla_sankey <- table |>
      filter(from == glue::glue("{stringr::str_to_sentence(categoria_t)}_{periodo_t}")) |> 
      mutate(categoria = categoria_t)
  }
  
  if(unique(table$periodo_base) == "t_posterior"){
    tabla_sankey <- table |>
      filter(to == glue::glue("{stringr::str_to_sentence(categoria_t)}_{periodo_t}")) |> 
      mutate(categoria = categoria_t)
  }
  
  tabla_sankey <- tabla_sankey |> 
    mutate(from = stringr::str_replace_all(from, "_tant", "_t0"),
           to   = stringr::str_replace_all(to, "_tpost", "_t1"))
  
  return(tabla_sankey)
}

# 
# armo_sankey <- function(table){
#   
#   if(unique(table$periodo_base) == "t_anterior"){
#     periodo <- "tant"}
#   
#   if(unique(table$periodo_base) == "t_posterior"){
#     periodo <- "tpost"
#   }
#   
#   names(table) <- c("from", "to", "weight", "id", "periodo_base", "categoria")
#   
#   highcharter::hchart(table, "sankey", 
#          name = "Gender based Outcomes") |> 
#     highcharter::hc_title(text= glue::glue(
#       "Población base: {unique(table$categoria)} - {ifelse(periodo == 'tant', 'Trimestre anterior', 'Trimestre posterior')}"))
#     #hc_subtitle(text= "Población ocupada al trimestre 2 de 2023")
# 
# }


### Notas para highcharter
df_to_annotations_labels <- function(df, xAxis = 0, yAxis = 0) {
  
  stopifnot(hasName(df, "x"))
  stopifnot(hasName(df, "y"))
  stopifnot(hasName(df, "text"))
  
  df %>% 
    rowwise() %>% 
    mutate(point = list(list(x = x, y = y, xAxis = 0, yAxis = 0))) %>% 
    select(-x, -y)  
  
}
