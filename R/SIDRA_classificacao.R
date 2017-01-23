#' Conexão do R com a SIDRA - IBGE
#'
#' Esta função retorna uma lista com as variáveis e categorias de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @keywords IBGE SIDRA dados
#' @export
#' @examples
#' clas_PAM <- SIDRA_classificacao(1612)

SIDRA_classificacao <- function(tabela, consulta = FALSE) {
  resposta <- descritores(tabela)

  # classificação
  ids_clas <- grep(pattern = "lblI?d?Classificacao", x = resposta$ids, value = TRUE)
  clas <- lapply(ids_clas, pega_texto, pagina = resposta$conteudo)

  if (length(clas) >= 2) {
    df_clas <- clas %>% matrix(ncol = 2, byrow = TRUE) %>%
      as.data.frame() %>% `names<-`(c('codigo', "descrição"))
  } else {
    warning("Não foram encontrados classificadores para esta tabela.")
    return(character(0))
  }

  if (consulta == FALSE) {
    return(unlist(df_clas$codigo))
  } else {
    return(df_clas)
  }
}
