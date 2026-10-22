# 06 - Além da média: quantis regionais e escolaridade da mãe ---------------
# 1) Para cada região imediata, calculamos os quantis da nota dos concluintes
#    (p10 = alunos de nota baixa, p90 = de nota alta) e vemos como cada um
#    varia com o PIB per capita e com a RENDA MÉDIA DA REGIÃO. Não é a renda
#    da família de cada aluno: em 2025 a LGPD impede ligar nota e questionário.
# 2) Comparamos renda e % de mães com ensino superior como preditores.

library(tidyverse)
library(here)
library(DBI)
library(fixest)

con <- dbConnect(duckdb::duckdb())
notas <- as_tibble(dbGetQuery(con, sprintf("
  SELECT CO_MUNICIPIO_ESC AS cod_mun,
         (NU_NOTA_CN + NU_NOTA_CH + NU_NOTA_LC + NU_NOTA_MT + NU_NOTA_REDACAO) / 5 AS nota
  FROM '%s'
  WHERE CO_ESCOLA IS NOT NULL
    AND TP_PRESENCA_CN = 1 AND TP_PRESENCA_CH = 1
    AND TP_PRESENCA_LC = 1 AND TP_PRESENCA_MT = 1",
  here("dados", "tratados", "enem_resultados.parquet"))))
dbDisconnect(con, shutdown = TRUE)

municipios <- read_rds(here("dados", "tratados", "municipios.rds"))
base <- read_rds(here("dados", "tratados", "base_regional.rds")) |> filter(!is.na(renda_pc))

probs <- c(0.10, 0.25, 0.50, 0.75, 0.90)
quantis <- notas |>
  inner_join(municipios |> select(cod_mun, cod_rgi), by = "cod_mun") |>
  group_by(cod_rgi) |>
  reframe(quantil = probs, nota = quantile(nota, probs), n = n()) |>
  inner_join(base |> select(cod_rgi, sigla_uf, log_pib_pc, log_renda_pc), by = "cod_rgi")

# Coeficiente de cada preditor (sozinho) em cada quantil --------------------
coef_quantis <- crossing(quantil = probs, preditor = c("log_renda_pc", "log_pib_pc")) |>
  mutate(modelo = map2(quantil, preditor, \(q, x)
    feols(as.formula(paste("nota ~", x)), filter(quantis, quantil == q),
          weights = ~n, cluster = ~sigla_uf)),
    coef = map2_dbl(modelo, preditor, \(m, x) coef(m)[[x]]),
    ep   = map2_dbl(modelo, preditor, \(m, x) se(m)[[x]]),
    r2   = map_dbl(modelo, \(m) fixest::r2(m, "r2"))) |>
  select(-modelo)
print(coef_quantis)

# Teste formal: o coeficiente no p10 é diferente do coeficiente no p90? ------
# Empilhamos p10 e p90 e estimamos a interação preditor x (quantil = p90).
# Os erros agrupados por UF levam em conta que as duas linhas vêm da mesma região.
extremos <- quantis |> filter(quantil %in% c(0.10, 0.90)) |> mutate(p90 = quantil == 0.90)
teste_dif <- map_dfr(c("log_renda_pc", "log_pib_pc"), function(x) {
  m <- feols(as.formula(sprintf("nota ~ %s * p90", x)), extremos,
             weights = ~n, cluster = ~sigla_uf)
  termo <- paste0(x, ":p90TRUE")
  tibble(preditor = x, dif_p90_p10 = coef(m)[[termo]], ep = se(m)[[termo]],
         p_valor = fixest::pvalue(m)[[termo]])
})
print(teste_dif)

# Amplitude interna (p90 - p10) x renda média da região
amplitude <- quantis |>
  filter(quantil %in% c(0.10, 0.90)) |>
  select(cod_rgi, sigla_uf, n, log_renda_pc, quantil, nota) |>
  pivot_wider(names_from = quantil, values_from = nota, names_prefix = "q") |>
  mutate(amplitude = q0.9 - q0.1)
m_amp <- feols(amplitude ~ log_renda_pc, amplitude, weights = ~n, cluster = ~sigla_uf)

write_rds(list(coef = coef_quantis, teste = teste_dif,
               amplitude = summary(amplitude$amplitude), m_amp = m_amp),
          here("resultados", "tabelas", "quantis.rds"))

fig_q <- coef_quantis |>
  mutate(preditor = recode(preditor, log_renda_pc = "Renda média da região",
                           log_pib_pc = "PIB per capita da região")) |>
  ggplot(aes(factor(quantil * 100), coef, colour = preditor, group = preditor)) +
  geom_line(linewidth = 1, position = position_dodge(width = 0.3)) +
  geom_pointrange(aes(ymin = coef - 1.96 * ep, ymax = coef + 1.96 * ep),
                  position = position_dodge(width = 0.3)) +
  scale_colour_manual(values = c("Renda média da região" = "#1f6feb",
                                 "PIB per capita da região" = "#e76f51")) +
  labs(x = "Quantil da nota dentro da região (10 = alunos de nota baixa, 90 = nota alta)",
       y = "Pontos de nota por aumento de\n1 unidade no log (≈ +172%)", colour = NULL,
       title = "O contexto regional pesa mais para quem tem nota baixa?",
       subtitle = "Cada ponto é uma regressão entre as 507 regiões imediatas · IC de 95%") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title.position = "plot",
        plot.title = element_text(face = "bold"))
ggsave(here("resultados", "figuras", "quantis_renda_pib.png"), fig_q,
       width = 8, height = 5, dpi = 150, bg = "white")

# Escolaridade da mãe x renda -------------------------------------------------
r2_de <- function(f) fixest::r2(feols(f, base, weights = ~n_concluintes), "r2")
padroniza <- function(x) (x - mean(x)) / sd(x)
base_z <- base |> mutate(across(c(nota_geral, log_renda_pc, mae_superior), padroniza))

mae <- list(
  correlacao = cor(base$log_renda_pc, base$mae_superior),
  r2_renda   = r2_de(nota_geral ~ log_renda_pc),
  r2_mae     = r2_de(nota_geral ~ mae_superior),
  r2_ambos   = r2_de(nota_geral ~ log_renda_pc + mae_superior),
  modelos    = list(
    "Sem EF de UF" = feols(nota_geral ~ log_renda_pc + mae_superior, base_z,
                           weights = ~n_concluintes, cluster = ~sigla_uf),
    "Com EF de UF" = feols(nota_geral ~ log_renda_pc + mae_superior | sigla_uf, base_z,
                           weights = ~n_concluintes, cluster = ~sigla_uf))
)
print(mae[1:4])
etable(mae$modelos)
write_rds(mae, here("resultados", "tabelas", "mae_renda.rds"))
