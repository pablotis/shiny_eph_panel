

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
