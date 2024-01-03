
source("ETL/00-libraries.R")

# variables <- c("CODUSU", "NRO_HOGAR", "COMPONENTE", "ANO4", "TRIMESTRE", "CH04", "CH06", "ESTADO", "PONDERA")
# 
# # ### Importo y armo base
# df_eph_2003_2023 <- get_microdata(year = 2003:2023, period = 1:4, vars = variables)
# 
# tets <- df_eph_2003_2023 |>
#   calculate_tabulates("ANO4", "TRIMESTRE")
# 
# ### Guardo en formato parquet
# write_parquet(df_eph_2003_2023, "data_raw/df_eph.parquet")

### Cargo bases
df_cond_act <- arrow::read_csv_arrow("data_output/panel_cond_act_historico.csv")


