#' Conexão do R com a SIDRA - IBGE
#'
#' Esta função retorna uma lista com as variáveis e categorias de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @keywords IBGE SIDRA dados
#' @export
#' @examples
#' periodo_PAM <- SIDRA_periodo(1612)

SIDRA_periodo <- function(tabela) {
  resposta <- descritores(tabela)

  ids_periodo <- grep(pattern = "Periodo", x = resposta$ids, value = TRUE)

  periodos <- lapply(ids_periodo, pega_texto, pagina = resposta$conteudo)

  stringr::str_split(periodos[[2]], ", ") %>% unlist()
}
