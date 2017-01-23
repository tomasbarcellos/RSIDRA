#' Conexão do R com a SIDRA - IBGE
#'
#' Esta função retorna uma lista com as variáveis e categorias de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @keywords IBGE SIDRA dados
#' @export
#' @examples
#' nivel_PAM <- SIDRA_nivel(1612)

SIDRA_nivel <- function(tabela) {
  resposta <- descritores(tabela)

  ids_nivel <- grep(pattern = "Nivelterritorial", x = resposta$ids, value = TRUE)[-1]

  lapply(ids_nivel, pega_texto, pagina = resposta$conteudo) %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    as.data.frame() %>% `names<-`(c('codigo', "descrição"))
}
