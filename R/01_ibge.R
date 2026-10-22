# 01 - Dados do IBGE: municípios, regiões imediatas, PIB e população --------
# Fontes (baixadas em dados/brutos/ibge/):
#   - API de Localidades do IBGE: município -> região geográfica imediata
#   - SIDRA tabela 5938: PIB dos municípios a preços correntes (2023)
#   - SIDRA tabela 4709: população residente, Censo 2022
#   - geobr (IPEA): malhas das regiões imediatas e das UFs

library(tidyverse)
library(here)
library(jsonlite)

pasta_ibge <- here("dados", "brutos", "ibge")

# Composição territorial ----------------------------------------------------
municipios <- fromJSON(file.path(pasta_ibge, "municipios_ibge.json"), flatten = TRUE) |>
  as_tibble() |>
  transmute(
    cod_mun  = as.character(id),
    nome_mun = nome,
    cod_rgi  = as.character(`regiao-imediata.id`),
    nome_rgi = str_replace_all(`regiao-imediata.nome`, "¿", "–"),  # a API traz "¿" no lugar de "–"
    sigla_uf = `regiao-imediata.regiao-intermediaria.UF.sigla`,
    regiao   = `regiao-imediata.regiao-intermediaria.UF.regiao.nome`
  )

# A primeira linha das respostas do SIDRA é o cabeçalho
ler_sidra <- function(arquivo, nome_valor) {
  fromJSON(file.path(pasta_ibge, arquivo)) |>
    as_tibble() |>
    slice(-1) |>
    transmute(cod_mun = D1C, "{nome_valor}" := as.numeric(V))
}

pib <- ler_sidra("pib_municipios.json", "pib_mil")         # mil R$, 2023
pop <- ler_sidra("populacao_censo2022.json", "populacao")  # pessoas, 2022

municipios <- municipios |>
  left_join(pib, by = "cod_mun") |>
  left_join(pop, by = "cod_mun")

# A API já traz Boa Esperança do Norte (MT), instalado após 2023: em 2022-2023
# seu território estava em Sorriso e Nova Ubiratã, na mesma região imediata.
# Por isso ele fica sem PIB e população próprios, sem perda no agregado.
sem_dados <- municipios |> filter(is.na(pib_mil) | is.na(populacao))
print(sem_dados |> select(cod_mun, nome_mun, nome_rgi, sigla_uf))
stopifnot(nrow(sem_dados) <= 1, nrow(municipios) - nrow(sem_dados) == 5570)

# Região imediata: PIB per capita = soma do PIB / soma da população ----------
regioes <- municipios |>
  group_by(cod_rgi, nome_rgi, sigla_uf, regiao) |>
  summarise(n_municipios = n(),
            pib_mil   = sum(pib_mil, na.rm = TRUE),
            populacao = sum(populacao, na.rm = TRUE),
            .groups = "drop") |>
  mutate(pib_pc = pib_mil * 1000 / populacao)

cat("Regiões imediatas:", nrow(regioes), "\n")
print(summary(regioes$pib_pc))

dir.create(here("dados", "tratados"), showWarnings = FALSE)
write_rds(municipios, here("dados", "tratados", "municipios.rds"))
write_rds(regioes,    here("dados", "tratados", "regioes_ibge.rds"))

# Malhas para os mapas (geobr) ---------------------------------------------
# As regiões imediatas (criadas em 2017) não mudaram desde então
malha_rgi <- geobr::read_immediate_region(year = 2022, simplified = TRUE, showProgress = FALSE)
malha_rgi <- malha_rgi |>
  transmute(cod_rgi = as.character(code_immediate)) |>
  filter(cod_rgi %in% regioes$cod_rgi)   # remove as lagoas Mirim e dos Patos (RS)
stopifnot(nrow(malha_rgi) == 510, all(malha_rgi$cod_rgi %in% regioes$cod_rgi))
malha_uf  <- geobr::read_state(year = 2022, simplified = TRUE, showProgress = FALSE)

write_rds(malha_rgi, here("dados", "tratados", "malha_rgi.rds"))
write_rds(malha_uf,  here("dados", "tratados", "malha_uf.rds"))
