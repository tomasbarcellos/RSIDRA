#' Conexão do R com a SIDRA - IBGE
#'
#' Esta função retorna uma lista com as variáveis e categorias de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @keywords IBGE SIDRA dados
#' @export
#' @examples
#' nota_PAM <- SIDRA_notas(1612)

SIDRA_notas <- function(tabela) {
  resposta <- descritores(tabela)

  ids_nota <- grep(pattern = "Descricao", x = resposta$ids, value = TRUE)

  lapply(ids_nota, pega_texto, pagina = resposta$conteudo) %>% paste(collapse = " ")
}
