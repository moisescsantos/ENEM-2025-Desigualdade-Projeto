# 04 - Cruzamento ENEM x perfil x PIB e modelos por região imediata ---------
# Análise ecológica: as relações valem ENTRE REGIÕES, não entre indivíduos.

library(tidyverse)
library(here)
library(fixest)
library(modelsummary)

agr     <- read_rds(here("dados", "tratados", "enem_agregado.rds"))
regioes <- read_rds(here("dados", "tratados", "regioes_ibge.rds"))

# Base regional --------------------------------------------------------------
# 3 regiões não têm local de prova: seus participantes fizeram a prova em
# regiões vizinhas e ficam sem perfil próprio (saem dos modelos com perfil).
base <- regioes |>
  left_join(agr$notas_rgi, by = "cod_rgi") |>
  left_join(agr$perfil_rgi, by = "cod_rgi") |>
  mutate(log_pib_pc   = log(pib_pc),
         log_renda_pc = log(renda_pc),
         across(c(mae_superior, sem_computador, ppi, pct_privada, so_publica,
                  sem_wifi, renda_ate_1sm), ~ .x * 100))   # em pontos percentuais

cat("Regiões sem perfil socioeconômico:", sum(is.na(base$renda_pc)), "\n")
write_rds(base, here("dados", "tratados", "base_regional.rds"))

# Modelos (ponderados pelo nº de concluintes; EP agrupados por UF) -----------
dados_mod <- base |> filter(!is.na(renda_pc))
modelos <- list(
  "(1) Só PIB"         = feols(nota_geral ~ log_pib_pc, dados_mod,
                               weights = ~n_concluintes, cluster = ~sigla_uf),
  "(2) Só perfil"      = feols(nota_geral ~ log_renda_pc + mae_superior + sem_computador + ppi,
                               dados_mod, weights = ~n_concluintes, cluster = ~sigla_uf),
  "(3) PIB + perfil"   = feols(nota_geral ~ log_pib_pc + log_renda_pc + mae_superior +
                                 sem_computador + ppi, dados_mod,
                               weights = ~n_concluintes, cluster = ~sigla_uf),
  "(4) + % privada"    = feols(nota_geral ~ log_pib_pc + log_renda_pc + mae_superior +
                                 sem_computador + ppi + pct_privada, dados_mod,
                               weights = ~n_concluintes, cluster = ~sigla_uf),
  "(5) + EF de UF"     = feols(nota_geral ~ log_pib_pc + log_renda_pc + mae_superior +
                                 sem_computador + ppi + pct_privada | sigla_uf, dados_mod,
                               weights = ~n_concluintes, cluster = ~sigla_uf)
)
etable(modelos)
write_rds(modelos, here("resultados", "tabelas", "modelos.rds"))

# Regiões acima / abaixo do esperado ----------------------------------------
# Resíduo do modelo (4): nota observada menos a prevista pelo PIB e pelo perfil.
dados_mod$esperada <- predict(modelos[["(4) + % privada"]], newdata = dados_mod)
residuos <- dados_mod |>
  transmute(cod_rgi, regiao_imediata = paste0(nome_rgi, " (", sigla_uf, ")"), regiao,
            n_concluintes, pib_pc, renda_pc, nota_geral, esperada,
            residuo = nota_geral - esperada)
write_csv(residuos, here("resultados", "tabelas", "residuos_regioes.csv"))
residuos |> slice_max(residuo, n = 10) |> print()
residuos |> slice_min(residuo, n = 10) |> print()

# Diferença privada x estadual por região -----------------------------------
gap <- agr$notas_rgi_rede |>
  filter(rede %in% c("Privada", "Estadual"), n >= 30) |>
  select(cod_rgi, rede, nota_geral) |>
  pivot_wider(names_from = rede, values_from = nota_geral) |>
  drop_na() |>
  mutate(gap = Privada - Estadual) |>
  left_join(base |> select(cod_rgi, nome_rgi, sigla_uf, regiao, pib_pc, log_pib_pc,
                           n_concluintes), by = "cod_rgi")
m_gap <- feols(gap ~ log_pib_pc, gap, weights = ~n_concluintes, cluster = ~sigla_uf)
etable(m_gap)
write_rds(list(dados = gap, modelo = m_gap), here("resultados", "tabelas", "gap_regioes.rds"))

# Tabela de modelos em Markdown (para o README)
modelsummary(modelos, stars = TRUE, gof_map = c("nobs", "r.squared"),
             output = here("resultados", "tabelas", "modelos.md"))
