# 05 - Mapas e gráficos ------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(patchwork)
library(scales)

agr      <- read_rds(here("dados", "tratados", "enem_agregado.rds"))
base     <- read_rds(here("dados", "tratados", "base_regional.rds"))
malha    <- read_rds(here("dados", "tratados", "malha_rgi.rds"))
malha_uf <- read_rds(here("dados", "tratados", "malha_uf.rds"))
residuos <- read_csv(here("resultados", "tabelas", "residuos_regioes.csv"),
                     col_types = cols(cod_rgi = "c"))
gap      <- read_rds(here("resultados", "tabelas", "gap_regioes.rds"))$dados

dir_fig <- here("resultados", "figuras")
cores_rede <- c(Federal = "#2a9d8f", Estadual = "#7a7a7a", Municipal = "#e9c46a", Privada = "#1f6feb")
cores_regiao <- c(Norte = "#2a9d8f", Nordeste = "#e76f51", `Centro-Oeste` = "#e9c46a",
                  Sudeste = "#1f6feb", Sul = "#6a4c93")
tema <- theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot", legend.position = "top",
        plot.title = element_text(face = "bold"))
salvar <- function(nome, fig, w = 8, h = 5) {
  ggsave(file.path(dir_fig, nome), fig, width = w, height = h, dpi = 150, bg = "white")
}

# 1. Distribuição das notas por rede ----------------------------------------
dist <- agr$dist_rede |>
  filter(rede != "Municipal") |>
  group_by(rede) |> mutate(pct = n / sum(n)) |> ungroup()
medias_rede <- agr$notas_localizacao |>
  group_by(rede) |> summarise(media = weighted.mean(nota_geral, n_total))

fig_dist <- ggplot(dist, aes(faixa + 5, pct, colour = rede)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = cores_rede) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = "Nota geral (média das 4 provas e da redação)", y = "% dos concluintes da rede",
       colour = NULL, title = "Distribuição das notas dos concluintes por rede de ensino",
       subtitle = "ENEM 2025 · rede municipal omitida (menos de 7 mil concluintes)") +
  tema
salvar("distribuicao_por_rede.png", fig_dist)

# 2. Estadual x privada por UF -----------------------------------------------
uf <- agr$notas_uf_rede |>
  filter(rede %in% c("Estadual", "Privada")) |>
  select(sigla_uf, regiao, rede, nota_geral) |>
  pivot_wider(names_from = rede, values_from = nota_geral) |>
  mutate(gap = Privada - Estadual, sigla_uf = fct_reorder(sigla_uf, Estadual))

fig_uf <- ggplot(uf, aes(y = sigla_uf)) +
  geom_segment(aes(x = Estadual, xend = Privada, yend = sigla_uf), colour = "grey75", linewidth = 1) +
  geom_point(aes(x = Estadual, colour = "Estadual"), size = 2.5) +
  geom_point(aes(x = Privada, colour = "Privada"), size = 2.5) +
  geom_text(aes(x = Privada + 4, label = round(gap)), hjust = 0, size = 3, colour = "grey40") +
  scale_colour_manual(values = cores_rede) +
  labs(x = "Nota geral média dos concluintes", y = NULL, colour = NULL,
       title = "Diferença entre escolas privadas e estaduais em cada UF",
       subtitle = "UFs ordenadas pela nota da rede estadual · número = diferença em pontos") +
  tema
salvar("estadual_privada_uf.png", fig_uf, h = 7)

# 3. Nota x PIB per capita e x renda dos alunos ------------------------------
r2_simples <- function(x, y, w) summary(lm(y ~ x, weights = w))$r.squared
disp <- base |> filter(!is.na(renda_pc))
r2_pib   <- r2_simples(disp$log_pib_pc, disp$nota_geral, disp$n_concluintes)
r2_renda <- r2_simples(disp$log_renda_pc, disp$nota_geral, disp$n_concluintes)

painel_disp <- function(x, titulo, r2v, xlab) {
  ggplot(disp, aes({{ x }}, nota_geral)) +
    geom_point(aes(size = n_concluintes, colour = regiao), alpha = 0.6) +
    geom_smooth(aes(weight = n_concluintes), method = "lm", se = FALSE,
                colour = "black", linewidth = 0.8) +
    scale_x_log10(labels = label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",", accuracy = 1)) +
    scale_colour_manual(values = cores_regiao) +
    scale_size_area(max_size = 7, guide = "none") +
    labs(x = xlab, y = "Nota geral média", colour = NULL,
         title = titulo, subtitle = paste("R² =", format(round(r2v, 2), decimal.mark = ","))) +
    tema
}
fig_disp <- painel_disp(pib_pc, "PIB per capita da região", r2_pib, "PIB per capita 2023 (escala log)") +
  painel_disp(renda_pc, "Renda dos alunos", r2_renda,
              "Renda familiar per capita declarada (escala log)") +
  plot_layout(guides = "collect") +
  plot_annotation(title = "O que explica melhor a nota média de cada região imediata?",
                  subtitle = "Cada ponto é uma das 507 regiões imediatas, com tamanho proporcional ao nº de concluintes",
                  theme = theme(plot.title = element_text(face = "bold", size = 14))) &
  theme(legend.position = "bottom")
salvar("nota_pib_renda.png", fig_disp, w = 11, h = 5.5)

# 4. Mapas -------------------------------------------------------------------
mapa_base <- malha |> left_join(base, by = "cod_rgi") |>
  left_join(residuos |> select(cod_rgi, residuo), by = "cod_rgi")

mapa <- function(var, titulo, escala) {
  ggplot(mapa_base) +
    geom_sf(aes(fill = {{ var }}), colour = NA) +
    geom_sf(data = malha_uf, fill = NA, colour = "white", linewidth = 0.25) +
    escala +
    labs(title = titulo, fill = NULL) +
    theme_void(base_size = 11) +
    theme(legend.position = "right", plot.title = element_text(face = "bold", hjust = 0.5))
}
m1 <- mapa(nota_geral, "Nota geral média", scale_fill_viridis_c(option = "viridis"))
m2 <- mapa(pib_pc, "PIB per capita (R$)",
           scale_fill_viridis_c(option = "magma", trans = "log10",
                                labels = label_number(scale = 1e-3, suffix = " mil")))
m3 <- mapa(renda_pc, "Renda per capita dos alunos (R$)",
           scale_fill_viridis_c(option = "magma", trans = "log10", labels = label_number()))
m4 <- mapa(residuo, "Nota acima/abaixo do esperado",
           scale_fill_gradient2(low = "#c0392b", mid = "#f7f7f7", high = "#1f6feb",
                                limits = c(-40, 40), oob = squish))
fig_mapas <- (m1 + m2) / (m3 + m4) +
  plot_annotation(title = "ENEM 2025 por região geográfica imediata",
                  caption = "Esperado = previsto por PIB per capita, perfil socioeconômico e % de alunos da rede privada (modelo 4).",
                  theme = theme(plot.title = element_text(face = "bold", size = 15)))
salvar("mapas_regioes.png", fig_mapas, w = 11, h = 10)

# 5. Diferença privada x estadual vs PIB -------------------------------------
fig_gap <- ggplot(gap, aes(pib_pc, gap)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(aes(size = n_concluintes, colour = regiao), alpha = 0.6) +
  geom_smooth(aes(weight = n_concluintes), method = "lm", se = TRUE, colour = "black") +
  scale_x_log10(labels = label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",", accuracy = 1)) +
  scale_colour_manual(values = cores_regiao) +
  scale_size_area(max_size = 7, guide = "none") +
  labs(x = "PIB per capita 2023 (escala log)", y = "Nota privada − nota estadual (pontos)",
       colour = NULL, title = "A vantagem da rede privada é menor nas regiões mais ricas",
       subtitle = sprintf("%d regiões imediatas com pelo menos 30 concluintes em cada rede", nrow(gap))) +
  tema
salvar("gap_privada_estadual_pib.png", fig_gap)
