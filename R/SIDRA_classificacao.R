#' Conexão do R com a SIDRA - IBGE
#'
#' Esta função retorna uma lista com as variáveis e categorias de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @keywords IBGE SIDRA dados
#' @export
#' @examples
#' cat_PAM <- SIDRA_classificacao(1612)

SIDRA_classificacao <- function(tabela) {
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

  nomes_cat <- grep(pattern = "/.*?/", x = y[[3]], value = TRUE) %>%
    gsub(pattern = "/", replacement = "")
  y[[3]] <- NULL

  cat <- vector("list", length(y) - 3)

  for (i in seq_along(cat)) {
    cat[[i]] <- y[[2+i]] %>% (function(x) {
      x[substr(x, 1,1) %in% 0:9]
    }) %>%
      strsplit(" ") %>%
      lapply(function(x) {
        return(c(x[1], paste(x[2:length(x)], collapse = " ")))
      }) %>%
      do.call(what = "rbind") %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      `names<-`(c("categoria", "descrição"))
  }

  tamanho_cat <- sapply(cat, nrow)
  classificacao <- rep(nomes_cat, times = tamanho_cat)

  df_cat <- cbind(classificacao,
               do.call(rbind, cat),
               stringsAsFactors = FALSE)

  return(df_cat)
}
