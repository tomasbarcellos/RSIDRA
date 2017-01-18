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
  
  var <- y[[2]] %>%
    sub(pattern = " - casas decimais:.*", replacement = "") %>%
    strsplit(" ") %>% lapply(function(x) {
      return(c(x[1], paste(x[2:length(x)], collapse = " ")))
    }) %>% do.call(what = "rbind") %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    `names<-`(c("cod_var", "desc_var"))

  return(var)
}
