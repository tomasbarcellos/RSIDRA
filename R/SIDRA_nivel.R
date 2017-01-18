#' Conexão do R com a SIDRA - IBGE
#'
#' Esta função retorna uma lista com as variáveis e categorias de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @keywords IBGE SIDRA dados
#' @export
#' @examples
#' nivel_PAM <- SIDRA_nivel(1612)

SIDRA_nivel <- function(tabela) {
  pag <- read_html(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=",
                          tabela))
  
  y <- pag %>% html_nodes("table") %>% html_text() %>%
    stringr::str_split("\r\n") %>% lapply(tm::stripWhitespace) %>%
    lapply(function(x) x[!x %in% c(" ", "")]) %>%
    lapply(function(x) {
      x[substr(x, 1,1) == " "] <- substr(x[substr(x, 1,1) == " "],
                                         2,nchar(x[substr(x, 1,1) == " "]))
      x
    })
  
  niv <- y[[length(y)]] %>% (function(x) {
    x[-grep(pattern = "Listar unidades", x = x)]
  }) %>% gsub(pattern = "/", replacement = "") %>%
    matrix(nrow = length(.)/2, byrow = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    `names<-`(c("nível", "descrição"))
  
  return(niv)
}
