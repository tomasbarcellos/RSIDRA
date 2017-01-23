#' Conexão do R com a SIDRA - IBGE
#'
#' Esta função retorna uma lista com as variáveis e categorias de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @keywords IBGE SIDRA dados
#' @export
#' @examples
#' tabelas <- SIDRA_tabelas()

SIDRA_tabelas <- function(tabela) {

  if (missing(tabela)) {
    tabs <- lapply(1:9999, function(x) try(SIDRA_tabelas(x)))
    tabs_ok <- tabs[!sapply(tabs, function(x) "try-error" %in% class(x))]
    df_tabs <- do.call("rbind", tabs_ok)
    Sys.sleep(0.2)
    return(df_tabs)
  }

  resposta <- descritores(tabela)

  # tabela
  ids_tabela <- grep(pattern = "Tabela", x = resposta$ids, value = TRUE)

  lapply(ids_tabela, pega_texto, pagina = resposta$conteudo) %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    as.data.frame() %>% `names<-`(c('codigo', "descrição"))
}
