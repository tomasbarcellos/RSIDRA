#' Conexão do R com a SIDRA - IBGE
#'
#' Esta função retorna o texto de determinado seletor css de uma página.
#' @param pagina Página que retorna da requisição da API com a função descritores().
#' @param css Seletor de css em que as informações serão tiradas
#' @keywords IBGE SIDRA dados

pega_texto <- function(pagina, css) {
  requireNamespace("rvest")
  pagina %>% rvest::html_nodes(css) %>% rvest::html_text()
}
