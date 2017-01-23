#' Conexão do R com a SIDRA - IBGE
#'
#' Esta função retorna uma lista com as variáveis e categorias de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @keywords IBGE SIDRA dados
#' @export
#' @examples
#' var_PAM <- SIDRA_variaveis(1612)

# library(rvest)

SIDRA_variaveis <- function(tabela) {
  resposta <- descritores(tabela)

  # variaveis
  ids_var <- grep(pattern = "Variaveis", x = resposta$ids, value = TRUE)[-1]
  lapply(ids_var, pega_texto, pagina = resposta$conteudo) %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    as.data.frame() %>% `names<-`(c('codigo', "descrição"))

}
