

# carrega pacotes ---------------------------------------------------------

pacotes <- c("readxl", "janitor", "dplyr", "tibble", "ggplot2",
             "sf", "stringr")

to_install <- !pacotes %in% installed.packages()
if(any(to_install)) {
  install.packages(pacotes[to_install])
}

library(readxl)
library(janitor)
library(dplyr)
library(tibble)
library(ggplot2)
library(sf)
library(stringr)

source("C:/Users/bruna/Documents/scripts_R/unnest_ameacas_salve_funcao_200423.R")

infos <- read_xlsx("planilhas_salve/salve_exportacao_taxon_analitico_03_06_2023_12_24_19.xlsx") |> 
  clean_names()
colnames(infos)

ocorrencias <- read_xlsx("planilhas_salve/salve_exportacao_registros_03_06_2023_12_21_44.xlsx") |> 
  clean_names()
colnames(ocorrencias)

ocorrencias <- ocorrencias |> 
  select(nome_cientifico, longitude, latitude)

ocorrencias$longitude <- as.numeric(
  gsub(",", ".", ocorrencias$longitude)
)
ocorrencias$latitude <- as.numeric(
  gsub(",", ".", ocorrencias$latitude)
)

ocorrencias <- ocorrencias |> 
  distinct(nome_cientifico, longitude, latitude)

ocorrencias <- infos |> 
  select(grupo = grupo_avaliado,
         nome_cientifico = taxon) |> 
  left_join(ocorrencias)

ocorrencias |> 
  count(nome_cientifico, sort = TRUE, name = "n_pts")
  
# n pontos por grupo
ocorrencias |> 
  count(grupo, sort = TRUE, name = "n_pts")
  
# n especies por grupo
ocorrencias |>
  distinct(grupo, nome_cientifico) |> 
  count(grupo, sort = TRUE, name = "n_pts")


# ameacas -----------------------------------------------------------------

# ameacas <- infos |> 
#   select(especie = taxon,
#          ameaca) |> 
#   rownames_to_column(var = "id_spp")
# 
# ameacas_sep <- unnest_ameacas(ameacas)
# 
# ameacas_sep <- ameacas_sep |> 
#   left_join(unique(ocorrencias[, c(1,2)]), 
#             by = c("especie" = "nome_cientifico")) |> 
#   select(grupo, especie:titulo)
# 
# writexl::write_xlsx(ameacas_sep, "planilhas/ameacas_sep2.xlsx")

# após edição no excel do que não foi separado corretamente
ameacas_sep <- read_xlsx("planilhas/ameacas_sep.xlsx", sheet = "ameacas_sep")
ameacas_n2 <- read_xlsx("planilhas/ameacas_sep.xlsx", sheet = "ameacas_n2")

freq_ameacas_n2 <- ameacas_n2 |> 
  group_by(grupo) |> 
  count(ameaca_n2) |> 
  arrange(grupo, desc(n))

# treemap no flourish: https://public.flourish.studio/visualisation/14005991/

freq_ameacas_spp <- ameacas_n2 |>
  group_by(grupo) |> 
  count(especie, sort = TRUE)

# https://public.flourish.studio/visualisation/14006335/
# writexl::write_xlsx(freq_ameacas_spp, "planilhas/freq_ameacas_spp.xlsx")

# areas protegidas --------------------------------------------------------

# ucs <- st_read("shapes/UCsFedIcmb_EstMunicMMA_s2k.shp")
# tis <- st_read("shapes/GEOFT_TERRA_INDIGENA.shp")
# tis <- st_make_valid(tis)
brasil <- st_read("shapes/BR_UF_2022.shp")

sudeste <- brasil |> 
  mutate(NM_REGIAO = 
           gsub("\n", "", brasil$NM_REGIAO)) |> 
  filter(NM_REGIAO == "Sudeste")

filtrar_se <- function(shp) {
  
  shp <- sf::st_transform(shp, crs = st_crs(sudeste)) 
    
  final <- shp |> 
    sf::st_filter(st_union(sudeste))
  
  return(final)
}

# tis_se <- filtrar_se(tis)
# ucs_se <- filtrar_se(ucs)

# st_write(tis_se, "shapes/tis_se.shp")
# st_write(ucs_se, "shapes/ucs_se.shp")
tis_se <- st_read("shapes/tis_se.shp")
ucs_se <- st_read("shapes/ucs_se.shp")


# empreendimentos ---------------------------------------------------------

## hidrelétricas

### ANA
# reservatorios_ana <- st_read("shapes/geoft_bho_massa_dagua_v2019.shp") |> 
#   clean_names()
# 
# reservatorios_ana$usoprinc |> unique()
# ahes_texto <- c("PCH", "CGH", "UHE")
# ahes_pol <- reservatorios_ana |> 
#   filter(detipoapr %in% ahes_texto | 
#            nmgenerico %in% ahes_texto |
#            usoprinc == "Hidrelétrica")
# nrow(ahes_pol)
# 
# ahes_pol_se <- ahes_pol |> 
#   filtrar_se()

# st_write(ahes_pol_se, "shapes/ahes_pol_se.shp")
ahes_pol_se <- st_read("shapes/ahes_pol_se.shp")

### ANEEL

# ahes <- st_read("shapes/Aproveitamento_Hidrelétricos_AHE.shp")
# 
# ahes_se <- ahes |> 
#   filter(FASE %in% c("Operação", "Desativada") |
#          str_detect(FASE, "Construção")) |> 
#   filtrar_se()

# st_write(, "shapes/ahes_se.shp", delete_layer = TRUE)
ahes_se <- st_read("shapes/ahes_se.shp")
