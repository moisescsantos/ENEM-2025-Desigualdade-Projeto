# 07 - Gera o relatório em HTML, Word e PDF ----------------------------------

library(here)

rmd <- here("relatorio", "relatorio.Rmd")
if (!file.exists(here("relatorio", "referencia.docx"))) source(here("R", "00_referencia_word.R"))

# HTML (site no GitHub Pages)
rmarkdown::render(rmd, quiet = TRUE, envir = new.env())
dir.create(here("docs"), showWarnings = FALSE)
file.copy(here("relatorio", "relatorio.html"), here("docs", "index.html"), overwrite = TRUE)

# Word: texto justificado e linhas cinza nas tabelas (estilos em referencia.docx)
rmarkdown::render(
  rmd, quiet = TRUE, envir = new.env(), output_file = "relatorio_enem_2025.docx",
  output_format = rmarkdown::word_document(
    toc = TRUE, fig_width = 6.5,
    reference_docx = here("relatorio", "referencia.docx"),
    pandoc_args = c("--metadata", "toc-title=Sumário")
  )
)

# PDF: impressão do HTML pelo Microsoft Edge em modo headless
edge <- c("C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
          "C:/Program Files/Microsoft/Edge/Application/msedge.exe")
edge <- edge[file.exists(edge)][1]
if (!is.na(edge)) {
  html <- normalizePath(here("relatorio", "relatorio.html"), winslash = "/")
  pdf  <- normalizePath(here("relatorio", "relatorio_enem_2025.pdf"), winslash = "/", mustWork = FALSE)
  system2(edge, c("--headless=new", "--disable-gpu", "--no-pdf-header-footer",
                  "--virtual-time-budget=15000",
                  shQuote(paste0("--print-to-pdf=", pdf)),
                  shQuote(paste0("file:///", utils::URLencode(html)))),
          stdout = FALSE, stderr = FALSE)
} else {
  message("Microsoft Edge não encontrado: PDF não gerado.")
}
