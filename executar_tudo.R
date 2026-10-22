# Roda o projeto do início ao fim ------------------------------------------
# Abra o projeto-enem-2025.Rproj no RStudio e rode este arquivo.
# Pré-requisito: microdados do ENEM 2025 descompactados. Se estiverem em outra
# pasta, defina Sys.setenv(ENEM_2025_DADOS = ".../microdados_enem_2025/DADOS").

# Cada etapa roda em um ambiente próprio, para que objetos e funções de uma
# etapa não interfiram nas seguintes.
etapas <- c("01_ibge.R", "02_enem_parquet.R", "03_agregar.R", "04_analise.R",
            "05_figuras.R", "06_quantis_mae.R",
            "07_relatorio.R")   # HTML, Word e PDF

for (etapa in etapas) {
  message("\n== ", etapa)
  source(here::here("R", etapa), local = new.env())
}
