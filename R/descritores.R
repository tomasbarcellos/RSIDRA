#' Conexão do R com a SIDRA - IBGE
#'
#' Esta função retorna uma lista com as variáveis e categorias de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @keywords IBGE SIDRA dados
#' @export
#' @examples
#' desc_PAM <- descritores(1612)

# library(rvest)

descritores <- function(tabela) {
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

  tab <- paste(y[[1]], collapse = "") %>%
    sub(pattern = "/T/ Tabela: ", replacement = "") %>%
    strsplit(" - ") %>% `[[`(1) %>%
    `names<-`(c("tabela", "descrição"))

  var <- y[[2]] %>%
    sub(pattern = " - casas decimais:.*", replacement = "") %>%
    strsplit(" ") %>% lapply(function(x) {
      return(c(x[1], paste(x[2:length(x)], collapse = " ")))
    }) %>% do.call(what = "rbind") %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
      `names<-`(c("variável", "descrição"))

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
  classificação <- rep(nomes_cat, times = tamanho_cat)

  cat <- cbind(classificação,
               do.call(rbind, cat),
               stringsAsFactors = FALSE)

  niv <- y[[length(y)]] %>% (function(x) {
    x[-grep(pattern = "Listar unidades", x = x)]
  }) %>% gsub(pattern = "/", replacement = "") %>%
    matrix(nrow = length(.)/2, byrow = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>% `names<-`(c("nível", "descrição"))

  return(list(tabela = tab, variável = var,
              categoria = cat, nível = niv))
}
