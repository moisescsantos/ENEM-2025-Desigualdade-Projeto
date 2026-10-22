# 02 - Converte os microdados do ENEM 2025 (CSV) para Parquet ---------------
# O CSV de resultados tem 2,1 GB. Lemos uma única vez com o duckdb, direto do
# disco, guardando só as colunas usadas. As consultas seguintes levam segundos.
# Os microdados ficam fora do repositório: baixe em
# https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/enem

library(DBI)
library(here)

pasta_inep <- Sys.getenv(
  "ENEM_2025_DADOS",
  "C:/Users/Moises/Documents/Analise de Dados/Novos dados ENEM/microdados_enem_2025/DADOS"
)
saida <- here("dados", "tratados")

con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA threads=4; PRAGMA memory_limit='4GB'")

converter <- function(arquivo, colunas, tipos, destino) {
  # Tipos explícitos: evita varrer o arquivo inteiro para adivinhá-los
  tipos_sql <- paste(sprintf("'%s': '%s'", names(tipos), tipos), collapse = ", ")
  sql <- sprintf(
    "COPY (SELECT %s FROM read_csv('%s/%s', delim=';', header=true,
                     encoding='latin-1', types={%s}))
     TO '%s/%s' (FORMAT parquet, COMPRESSION zstd)",
    paste(colunas, collapse = ", "), pasta_inep, arquivo, tipos_sql, saida, destino
  )
  inicio <- Sys.time()
  dbExecute(con, sql)
  message(destino, ": ", round(difftime(Sys.time(), inicio, units = "mins"), 1), " min")
}

# Resultados: escola, presença e notas -------------------------------------
col_res <- c("CO_ESCOLA", "CO_MUNICIPIO_ESC", "SG_UF_ESC", "TP_DEPENDENCIA_ADM_ESC",
             "TP_LOCALIZACAO_ESC", "CO_MUNICIPIO_PROVA", "SG_UF_PROVA",
             "TP_PRESENCA_CN", "TP_PRESENCA_CH", "TP_PRESENCA_LC", "TP_PRESENCA_MT",
             "NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT",
             "TP_STATUS_REDACAO", "NU_NOTA_REDACAO")
tipos_res <- c(CO_ESCOLA = "VARCHAR", CO_MUNICIPIO_ESC = "VARCHAR", CO_MUNICIPIO_PROVA = "VARCHAR",
               TP_DEPENDENCIA_ADM_ESC = "INTEGER", TP_LOCALIZACAO_ESC = "INTEGER",
               TP_PRESENCA_CN = "INTEGER", TP_PRESENCA_CH = "INTEGER",
               TP_PRESENCA_LC = "INTEGER", TP_PRESENCA_MT = "INTEGER",
               NU_NOTA_CN = "DOUBLE", NU_NOTA_CH = "DOUBLE", NU_NOTA_LC = "DOUBLE",
               NU_NOTA_MT = "DOUBLE", TP_STATUS_REDACAO = "INTEGER", NU_NOTA_REDACAO = "DOUBLE")
converter("RESULTADOS_2025.csv", col_res, tipos_res, "enem_resultados.parquet")

# Participantes: perfil e questionário socioeconômico ----------------------
col_part <- c("TP_FAIXA_ETARIA", "TP_SEXO", "TP_COR_RACA", "TP_ST_CONCLUSAO", "IN_TREINEIRO",
              "CO_MUNICIPIO_PROVA", "SG_UF_PROVA", sprintf("Q%03d", 1:23))
tipos_part <- c(CO_MUNICIPIO_PROVA = "VARCHAR", TP_FAIXA_ETARIA = "INTEGER",
                TP_COR_RACA = "INTEGER", TP_ST_CONCLUSAO = "INTEGER", IN_TREINEIRO = "INTEGER",
                setNames(rep("VARCHAR", 23), sprintf("Q%03d", 1:23)))
converter("PARTICIPANTES_2025.csv", col_part, tipos_part, "enem_participantes.parquet")

print(dbGetQuery(con, sprintf("SELECT count(*) AS n FROM '%s/enem_resultados.parquet'", saida)))
print(dbGetQuery(con, sprintf("SELECT count(*) AS n FROM '%s/enem_participantes.parquet'", saida)))
dbDisconnect(con, shutdown = TRUE)
