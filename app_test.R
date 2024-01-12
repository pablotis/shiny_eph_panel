anio_ant <- 2017
trim_ant <- 1
anio_post <- 2017
trim_post <- 2

periodo_filter <- glue::glue("{anio_ant}_t{trim_ant}-t{trim_post}")
sentido <- "t_anterior"
categoria <- "Ocupado"
categoria_lab <- ifelse(categoria == "Ocupado", "Ocupación", 
                        ifelse(categoria == "Desocupado", "Desocupación","Inactividad"))

df_eph_panel <- read_csv_arrow("data_output/panel_cond_act_historico_test.csv") |> 
  filter(periodo == periodo_filter,
         periodo_base == sentido)
  
highcharter::hchart(
    object = armo_tabla_sankey(table = df_eph_panel,
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


f_desde <- "Ocupado_t0"
f_hacia <- c("Desocupado_t1", "Ocupado_t1", "Inactivo_t1")

hchart(df_cond_act |> 
         filter(sentido == "t_anterior") |> 
         filter(from == f_desde, to %in% f_hacia) |> 
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
