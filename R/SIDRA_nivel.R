#' Conexão do R com a SIDRA - IBGE
#'
#' Esta função retorna uma lista com as variáveis e categorias de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @param consulta Em caso de consulta, além dos códigos, as descrições também são retornadas.
#' @keywords IBGE SIDRA dados
#' @export
#' @examples
#' nivel_PAM <- SIDRA_nivel(1612)

SIDRA_nivel <- function(tabela, consulta = FALSE) {
  resposta <- descritores(tabela)

  ids_nivel <- grep(pattern = "Nivelterritorial", x = resposta$ids, value = TRUE)[-1]

  df_nivel <- lapply(ids_nivel, pega_texto, pagina = resposta$conteudo) %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    as.data.frame() %>% `names<-`(c('codigo', "descrição"))

  if (consulta == FALSE) {
    return(unlist(df_nivel$codigo))
  } else {
    return(df_nivel)
  }
}
