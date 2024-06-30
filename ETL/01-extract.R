
source("ETL/00-libraries.R")

### Cada vez que hay trimestre nuevo:
# variables <- c("CODUSU", "NRO_HOGAR", "COMPONENTE", "ANO4", "TRIMESTRE", "CH04", "CH06", "ESTADO", "PONDERA")
# 
# # ### Importo y armo base
# df_eph_tot <- get_microdata(year = 2003:2023, period = 1:4, vars = variables)
# 
#  check <- df_eph_tot |>
#   calculate_tabulates("ANO4", "TRIMESTRE")
# 
# ### Guardo en formato parquet
# write_parquet(df_eph_tot, "data_raw/df_eph.parquet")
# 
# ## Tasas del mercado de trabajo
# df_tasas_mt <- read_parquet("data_raw/df_eph.parquet") |>
#   group_by(ANO4, TRIMESTRE) |>
#   summarise(pob_total = sum(PONDERA),
#             pob_ocupada = sum(PONDERA[ESTADO == 1]),
#             pob_desocupada = sum(PONDERA[ESTADO == 2]),
#             pob_inactiva = sum(PONDERA[ESTADO == 3]))
# 
# write_parquet(df_tasas_mt, "data_output/df_tasas_mt.parquet")

### Cargo bases
df_cond_act <- arrow::read_csv_arrow("data_output/panel_cond_act_historico.csv")
df_tass_mt <- arrow::read_parquet("data_output/df_tasas_mt.parquet")

