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
    tabs <- lapply(1:10, function(x) try(SIDRA_tabelas(x)))
    tabs_ok <- tabs[!sapply(tabs, function(x) "try-error" %in% class(x))]
    df_tabs <- do.call("rbind", tabs_ok)
    
    return(df_tabs) 
  }
  
  pag <- try(read_html(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=",
                          tabela), options = c("NOERROR", "NOWARNING")))
  
  if ("try-error" %in% class(pag)) {
    tf <- tempfile()
    download.file(url = paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=",
                               tabela),
                  destfile = tf, quiet = TRUE)
    pag <- read_html(tf, encoding = "UTF-8")
  }
  
  closeAllConnections() ; file.remove(tf)
  
  if (exists(pag)) cat("Informação coletada com sucesso!\n")
    
  y <- pag %>% html_nodes("table") %>% html_text() %>%
    stringr::str_split("\r\n") %>% lapply(tm::stripWhitespace) %>%
    lapply(function(x) x[!x %in% c(" ", "")]) %>%
    lapply(function(x) {
      x[substr(x, 1,1) == " "] <- substr(x[substr(x, 1,1) == " "],
                                         2,nchar(x[substr(x, 1,1) == " "]))
      x
    })
  
  tab <- paste(y[[1]], collapse = "") %>%
    sub(pattern = "/T/ Tabela: ", replacement = "") %>%
    strsplit(" - ") %>% `[[`(1)
  
  df_tab <- data.frame(tabela = tab[1],
                       descrição = tab[2],
                       stringsAsFactors = FALSE)
  return(df_tab)
}
