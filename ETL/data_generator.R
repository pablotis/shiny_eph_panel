### Start from scratch
file.remove("data_output/paneles_eph_Desocupado_tanterior.csv")
file.remove("data_output/paneles_eph_Inactivo_tanterior.csv")
file.remove("data_output/paneles_eph_Ocupado_tanterior.csv")
file.remove("data_output/paneles_eph_Desocupado_tposterior.csv")
file.remove("data_output/paneles_eph_Inactivo_tposterior.csv")
file.remove("data_output/paneles_eph_Ocupado_tposterior.csv")
file.remove("data_output/panel_cond_act_historico_test.csv")



anios <- 2003:2023
trims <- 1:3
categoria <- c("Ocupado", "Desocupado", "Inactivo")


for (anio in seq_along(anios)) {
  
  anio_ant <- anios[anio]
  anio_post <- anios[anio]
  
  for (trim in seq_along(trims)) {
    
    trimestre_ant <- trims[trim]
    trimestre_post <- trims[trim] + 1
    
    df_panel <- armo_base_panel(anio_0 = anio_ant, 
                                trimestre_0 = trimestre_ant, 
                                anio_1 = anio_post, 
                                trimestre_1 = trimestre_post)
    
    df_panel <- preparo_base(df = df_panel, periodo_base = "t_anterior")
    
    for (i in seq_along(categoria)) {
      try({
      df_tabla <- armo_tabla_sankey(table = df_panel, categoria_t = categoria[i]) |> 
        mutate(periodo = glue::glue("{anio_ant}_t{trimestre_ant}-t{trimestre_post}"))
      
      write.table(df_tabla, file = glue::glue("data_output/paneles_eph_{categoria[i]}_tanterior.csv"), sep = ",", 
                  append = TRUE, quote = FALSE, 
                  col.names = TRUE, row.names = FALSE)
      }, silent = TRUE)
    }
  }
}


for (anio in seq_along(anios)) {
  
  anio_ant <- anios[anio]
  anio_post <- anios[anio]
  
  for (trim in seq_along(trims)) {
    
    trimestre_ant <- trims[trim]
    trimestre_post <- trims[trim] + 1
    
    df_panel <- armo_base_panel(anio_0 = anio_ant, 
                                trimestre_0 = trimestre_ant, 
                                anio_1 = anio_post, 
                                trimestre_1 = trimestre_post)
    
    df_panel <- preparo_base(df = df_panel, periodo_base = "t_posterior")
    
    for (i in seq_along(categoria)) {
      try({
        df_tabla <- armo_tabla_sankey(table = df_panel, categoria_t = categoria[i]) |> 
          mutate(periodo = glue::glue("{anio_ant}_t{trimestre_ant}-t{trimestre_post}"))
        
        write.table(df_tabla, file = glue::glue("data_output/paneles_eph_{categoria[i]}_tposterior.csv"), sep = ",", 
                    append = TRUE, quote = FALSE, 
                    col.names = TRUE, row.names = FALSE)
      }, silent = TRUE)
    }
  }
}

### Limpieza
df_cond_act <- readr::read_csv("data_output/paneles_eph_Desocupado_tanterior.csv") |> 
  bind_rows(readr::read_csv("data_output/paneles_eph_Inactivo_tanterior.csv")) |> 
  bind_rows(readr::read_csv("data_output/paneles_eph_Ocupado_tanterior.csv")) |>
  bind_rows(readr::read_csv("data_output/paneles_eph_Desocupado_tposterior.csv")) |>
  bind_rows(readr::read_csv("data_output/paneles_eph_Inactivo_tposterior.csv")) |> 
  bind_rows(readr::read_csv("data_output/paneles_eph_Ocupado_tposterior.csv")) |> 
  filter(from != "from")

readr::write_csv(df_cond_act, "data_output/panel_cond_act_historico_test.csv")
