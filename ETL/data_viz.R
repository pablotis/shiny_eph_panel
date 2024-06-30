library(highcharter)
library(dplyr)

df_cond_act <- arrow::read_csv_arrow("data_output/panel_cond_act_historico.csv")

#colnames(df) <- c("desde", "hacia", "porc", "etiqueta", "sentido", "categoria", "periodo")
# 
# hchart(df |> 
#          mutate(x = row_number())
#          filter(from == "Desocupado_tant") ,
#          #filter(to == "Inactivo_tpost"), 
#        "spline", hcaes(x = periodo, y = weight, group = to))

df_etiqueta <- df_cond_act |> 
  distinct(periodo, .keep_all = TRUE) |> 
  mutate(x = row_number()) |> 
  select(x, periodo, y = weight) |> 
  filter(periodo %in% c("2020_t1-t2")) |> 
  mutate(text = "Pandemia")

df1_p <- df_to_annotations_labels(df_etiqueta)

hchart(df_cond_act |> 
         filter(from == "Desocupado_t0", to == "Desocupado_t1"),
       "line", 
       hcaes(periodo, weight, group = to))  |> 
  hc_add_theme(hc_theme_538()) |> 
  hc_tooltip(
    pointFormat = "<span  style='color: {series.color}'> {series.name} <b>{point.y}</b><br/></span>",
    shadow = FALSE,
    backgroundColor = "transparent",
    style = list(textOutline = "3px #404040"),
    borderColor = "transparent",
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
        list(point = list(x = 12, y = 40, xAxis = 0, yAxis = 0), text = "Comienzo de Pandemia")
      )
    )
  )|>  
  hc_caption(
    text = "Este gráfico utiliza la función Anotaciones de Highcharts para colocar
    etiquetas en varios puntos de interés. Las etiquetas son <i>responsivas</i> y se ocultarán
    para evitar la superposición en pantallas pequeñas."
  )



library(gghighlight)
library(ggplot2)

df_cond_act |> 
  filter(from == "Desocupado_t0") |> 
  ggplot() +
  geom_line(aes(x = periodo, y = weight, colour = to, group = to)) +
  gghighlight(to == "Desocupado_t1", line_label_type = "text_path") +
  #facet_wrap(facets = "to") +
  theme_minimal() +
  theme(axis.text.y.right = element_text(size = 50))


