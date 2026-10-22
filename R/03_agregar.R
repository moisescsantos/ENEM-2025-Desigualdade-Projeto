# 03 - Agrega os microdados do ENEM por região imediata ---------------------
# Participantes e resultados não têm chave comum (LGPD). A ponte entre eles é
# o território: cada base é agregada por região imediata e depois cruzada.
#
# População analisada: concluintes do ensino médio em 2025.
#   - Notas: participantes com escola informada (vinculados ao Censo Escolar
#     2025), localizados pelo município da ESCOLA.
#   - Perfil: TP_ST_CONCLUSAO = 2 ("concluirei em 2025"), localizados pelo
#     município da PROVA (único disponível na base de participantes).

library(tidyverse)
library(here)
library(DBI)

con <- dbConnect(duckdb::duckdb())
res  <- sprintf("'%s'", here("dados", "tratados", "enem_resultados.parquet"))
part <- sprintf("'%s'", here("dados", "tratados", "enem_participantes.parquet"))
consulta <- function(sql) as_tibble(dbGetQuery(con, sql))

municipios <- read_rds(here("dados", "tratados", "municipios.rds"))
rede_rotulo <- c("1" = "Federal", "2" = "Estadual", "3" = "Municipal", "4" = "Privada")

# 1. Visão nacional ----------------------------------------------------------
visao_geral <- consulta(sprintf("
  SELECT count(*)                                            AS inscritos,
         count(*) FILTER (CO_ESCOLA IS NOT NULL)             AS com_escola,
         avg((TP_PRESENCA_LC <> 1 OR TP_PRESENCA_MT <> 1)::INT) AS abstencao,
         avg((NU_NOTA_REDACAO = 0)::INT) FILTER (TP_PRESENCA_LC = 1) AS redacao_zero
  FROM %s", res))
print(visao_geral)

# Motivos da redação sem nota (presentes no 1º dia)
status_redacao <- consulta(sprintf("
  SELECT TP_STATUS_REDACAO AS status, count(*) AS n
  FROM %s WHERE TP_PRESENCA_LC = 1 GROUP BY 1 ORDER BY 1", res))

# 2. Notas dos concluintes por município da escola e rede -------------------
# Nota geral = média simples das 4 provas objetivas e da redação, para quem
# esteve presente nos dois dias.
notas_mun_rede <- consulta(sprintf("
  SELECT CO_MUNICIPIO_ESC AS cod_mun,
         TP_DEPENDENCIA_ADM_ESC AS rede,
         TP_LOCALIZACAO_ESC AS localizacao,
         count(*) AS n,
         sum((NU_NOTA_CN + NU_NOTA_CH + NU_NOTA_LC + NU_NOTA_MT + NU_NOTA_REDACAO) / 5) AS soma_geral,
         sum(NU_NOTA_MT) AS soma_mt,
         sum(NU_NOTA_REDACAO) AS soma_red
  FROM %s
  WHERE CO_ESCOLA IS NOT NULL
    AND TP_PRESENCA_CN = 1 AND TP_PRESENCA_CH = 1
    AND TP_PRESENCA_LC = 1 AND TP_PRESENCA_MT = 1
  GROUP BY 1, 2, 3", res)) |>
  mutate(rede = rede_rotulo[as.character(rede)],
         localizacao = if_else(localizacao == 1, "Urbana", "Rural"))

# Distribuição das notas por rede (para o gráfico de densidade)
dist_rede <- consulta(sprintf("
  SELECT TP_DEPENDENCIA_ADM_ESC AS rede,
         floor((NU_NOTA_CN + NU_NOTA_CH + NU_NOTA_LC + NU_NOTA_MT + NU_NOTA_REDACAO) / 5 / 10) * 10 AS faixa,
         count(*) AS n
  FROM %s
  WHERE CO_ESCOLA IS NOT NULL
    AND TP_PRESENCA_CN = 1 AND TP_PRESENCA_CH = 1 AND TP_PRESENCA_LC = 1 AND TP_PRESENCA_MT = 1
  GROUP BY 1, 2", res)) |>
  mutate(rede = rede_rotulo[as.character(rede)])

# 3. Perfil socioeconômico dos concluintes por município da prova -----------
# Renda familiar (Q007) pelo ponto médio de cada faixa, dividida pelo nº de
# moradores (Q005). A última faixa ("acima de R$ 30.360") recebe 1,25 x o limite.
perfil_mun <- consulta(sprintf("
  WITH base AS (
    SELECT CO_MUNICIPIO_PROVA AS cod_mun,
           CASE Q007 WHEN 'A' THEN 0       WHEN 'B' THEN 759     WHEN 'C' THEN 1897.5
                     WHEN 'D' THEN 2656.5  WHEN 'E' THEN 3415.5  WHEN 'F' THEN 4174.5
                     WHEN 'G' THEN 5313    WHEN 'H' THEN 6831    WHEN 'I' THEN 8349
                     WHEN 'J' THEN 9867    WHEN 'K' THEN 11385   WHEN 'L' THEN 12903
                     WHEN 'M' THEN 14421   WHEN 'N' THEN 16698   WHEN 'O' THEN 20493
                     WHEN 'P' THEN 26565   WHEN 'Q' THEN 37950 END / TRY_CAST(Q005 AS DOUBLE) AS renda_pc,
           Q007 IN ('A', 'B')       AS renda_ate_1sm,
           Q002 IN ('F', 'G')       AS mae_superior,
           Q023 = 'A'               AS so_publica,
           Q020 = 'A'               AS sem_wifi,
           Q021 = 'A'               AS sem_computador,
           TP_COR_RACA IN (2, 3, 5) AS ppi
    FROM %s
    WHERE TP_ST_CONCLUSAO = 2
  )
  SELECT cod_mun,
         count(*) AS n_perfil,
         sum(renda_pc)  AS soma_renda_pc, count(renda_pc) AS n_renda,
         sum(renda_ate_1sm::INT)  AS renda_ate_1sm,
         sum(mae_superior::INT)   AS mae_superior,
         sum(so_publica::INT)     AS so_publica,
         sum(sem_wifi::INT)       AS sem_wifi,
         sum(sem_computador::INT) AS sem_computador,
         sum(ppi::INT)            AS ppi
  FROM base GROUP BY 1", part))

dbDisconnect(con, shutdown = TRUE)

# 4. Leva tudo para a região imediata ----------------------------------------
para_rgi <- municipios |> select(cod_mun, cod_rgi)

sem_regiao <- c(notas = sum(!notas_mun_rede$cod_mun %in% para_rgi$cod_mun),
                perfil = sum(!perfil_mun$cod_mun %in% para_rgi$cod_mun))
print(sem_regiao)   # códigos de município ausentes da tabela do IBGE

notas_rgi_rede <- notas_mun_rede |>
  inner_join(para_rgi, by = "cod_mun") |>
  group_by(cod_rgi, rede) |>
  summarise(n = sum(n), nota_geral = sum(soma_geral) / n,
            nota_mt = sum(soma_mt) / n, nota_red = sum(soma_red) / n, .groups = "drop")

notas_rgi <- notas_mun_rede |>
  inner_join(para_rgi, by = "cod_mun") |>
  group_by(cod_rgi) |>
  summarise(n_concluintes = sum(n),
            nota_geral = sum(soma_geral) / n_concluintes,
            nota_mt    = sum(soma_mt) / n_concluintes,
            nota_red   = sum(soma_red) / n_concluintes,
            pct_privada = sum(n[rede == "Privada"]) / n_concluintes,
            .groups = "drop")

perfil_rgi <- perfil_mun |>
  inner_join(para_rgi, by = "cod_mun") |>
  group_by(cod_rgi) |>
  summarise(n_perfil = sum(n_perfil),
            renda_pc = sum(soma_renda_pc) / sum(n_renda),
            across(c(renda_ate_1sm, mae_superior, so_publica, sem_wifi, sem_computador, ppi),
                   ~ sum(.x) / n_perfil),
            .groups = "drop")

# Notas por UF e rede (visão geral)
notas_uf_rede <- notas_mun_rede |>
  inner_join(municipios |> select(cod_mun, sigla_uf, regiao), by = "cod_mun") |>
  group_by(sigla_uf, regiao, rede) |>
  summarise(n = sum(n), nota_geral = sum(soma_geral) / n, .groups = "drop")

notas_localizacao <- notas_mun_rede |>
  group_by(rede, localizacao) |>
  summarise(n_total = sum(n), nota_geral = sum(soma_geral) / n_total, .groups = "drop")

write_rds(list(visao_geral = visao_geral, status_redacao = status_redacao,
               dist_rede = dist_rede, notas_rgi_rede = notas_rgi_rede,
               notas_rgi = notas_rgi, perfil_rgi = perfil_rgi,
               notas_uf_rede = notas_uf_rede, notas_localizacao = notas_localizacao),
          here("dados", "tratados", "enem_agregado.rds"))
