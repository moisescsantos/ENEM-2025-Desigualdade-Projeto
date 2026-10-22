# 00 - Modelo de estilos para a versão Word do relatório --------------------
# Parte do modelo padrão do pandoc e ajusta dois pontos:
#   - texto do corpo justificado (linhas com a mesma largura);
#   - tabelas com uma linha cinza bem clara entre as linhas.
# Os blocos de código (como o diagrama da seção 1) usam outro estilo e
# continuam alinhados à esquerda e sem linhas.

library(here)

pandoc <- file.path(rmarkdown::find_pandoc()$dir, "pandoc")
pasta  <- file.path(tempdir(), "referencia_word")
unlink(pasta, recursive = TRUE)
dir.create(pasta)

padrao <- file.path(tempdir(), "padrao.docx")
system2(pandoc, c("-o", shQuote(padrao), "--print-default-data-file", "reference.docx"))
zip::unzip(padrao, exdir = pasta)

arq_estilos <- file.path(pasta, "word", "styles.xml")
estilos <- paste(readLines(arq_estilos, encoding = "UTF-8", warn = FALSE), collapse = "\n")

substituir <- function(texto, de, para) {
  stopifnot(grepl(de, texto, fixed = TRUE))   # falha se o modelo do pandoc mudar
  sub(de, para, texto, fixed = TRUE)
}

# Corpo do texto justificado (Primeiro parágrafo e Compacto herdam do Corpo)
estilos <- substituir(estilos,
  '<w:spacing w:before="180" w:after="180" />',
  '<w:spacing w:before="180" w:after="180" />\n      <w:jc w:val="both" />')

# Linhas cinza-claro entre as linhas das tabelas
estilos <- substituir(estilos,
  '<w:tblInd w:w="0" w:type="dxa" />',
  paste0('<w:tblInd w:w="0" w:type="dxa" />\n',
         '      <w:tblBorders>\n',
         '        <w:bottom w:val="single" w:sz="4" w:space="0" w:color="E3E3E3" />\n',
         '        <w:insideH w:val="single" w:sz="4" w:space="0" w:color="E3E3E3" />\n',
         '      </w:tblBorders>'))
estilos <- substituir(estilos,
  '<w:bottom w:val="single"/>',
  '<w:bottom w:val="single" w:sz="8" w:space="0" w:color="C8C8C8"/>')

writeLines(estilos, arq_estilos, useBytes = TRUE)

destino <- here("relatorio", "referencia.docx")
unlink(destino)
zip::zip(destino, files = list.files(pasta, recursive = TRUE, all.files = TRUE),
         root = pasta, mode = "mirror")
message("Modelo salvo em ", destino)
