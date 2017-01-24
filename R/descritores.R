#' Conexão do R com a SIDRA - IBGE
#'
#' Esta função retorna uma lista com os ids e o conteúdo (XML) da descrição da tabela solicitada.
#' @param tabela Número da tabela.
#' @keywords IBGE SIDRA dados
#' @examples
#' desc_PAM <- descritores(1612)

# library(rvest)

descritores <- function(tabela) {

  requireNamespace('httr')
  requireNamespace('rvest')

  resp <- httr::GET(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=", tabela))

  conteudo <- httr::content(resp)

  ids <- paste0("span#", conteudo %>%
                  rvest::html_nodes("span") %>% rvest::html_attrs() %>%
                  unlist() %>% unique() %>%
                  grep(pattern = 'lbl', value = TRUE))
  structure(
    list(
      ids = ids,
      conteudo = conteudo),
    class = "descritores_SIDRA"
  )
}

# if ("try-error" %in% class(pag)) {
#   tf <- tempfile()
#   download.file(url = paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=",
#                              tabela),
#                 destfile = tf, quiet = TRUE)
#   pag <- read_html(tf, encoding = "UTF-8")
#   file.remove(tf)
# }
#
# closeAllConnections()
