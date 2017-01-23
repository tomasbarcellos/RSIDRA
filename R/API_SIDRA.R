#' Conexão do R com a SIDRA - IBGE
#'
#' Esta função retorna um data.frame a tabela solicitada.
#' @param tabela Número da tabela.
#' @param classificador Classificador a ser detalhado. O padrão é "", retornando os totais da tabela. Para verificar os classificadores disponíveis na tabela em questão use a função SIDRA_classidicacao().
#' @param cod_cat Código para definição de subconjunto do classificador. Para verificar as categorias disponíveis na tabela em questão use a função SIDRA_classidicacao().
#' @param nivel Nível geográfico de agregação dos dados 1 = Brasil e 6 = Município, etc. Para verificar os níveis disponíveis na tabela em questão use a função SIDRA_nivel().
#' @param cod_nivel Código contendo conjunto no nível que será selecionado. Pode-se usar o código de determina UF para obter apenas seus dados ou "all" para todos (padrão). Para mais informações visite http://api.sidra.ibge.gov.br/home/ajuda.
#' @param periodo Período dos dados. O padrão é "all", isto é, todos os anos disponíveis. Para verificar os períodos disponíveis na tabela em questão use a função SIDRA_periodo().
#' @param variavel Quais variáveis devem retornar? O padrão é "allxp", isto é, todas exceto aquelas calculadas pela SIDRA (percentuais). Para verificar as variáveis disponíveis na tabela em questão use a função SIDRA_variaveis().
#' @param inicio,fim Início e fim do período desejado.
#' @keywords IBGE SIDRA dados
#' @export
#' @examples
#' PAM <- API_SIDRA(1612, 81)

API_SIDRA <- function(tabela,
                      classificador = SIDRA_classificacao(tabela), cod_cat = rep("all", length(classificador)),
                      nivel = 1, cod_nivel = rep("all", length(nivel)),
                      periodo = "all", variavel = "allxp",
                      inicio, fim) {
  # Início das verificações
  if (length(tabela) > 1) {
    stop("Solicite os dados de uma tabela por vez. Para mais de uma use mapply()", call. = FALSE)
    }

  if (!tabela %in% RSIDRA::tabelas_SIDRA$tabela) {
    stop("A tabela informada não é válida", call. = FALSE)
  }

  if (!missing(inicio) && !missing(fim)) {
    periodo <- paste0(inicio, "-", fim)
  }

  if (length(nivel) != length(cod_nivel)) {
    stop("Os argumentos nivel e cod_nivel devem ter o mesmo tamanho", call. = FALSE)
  }

  cod_nivel <- lapply(cod_nivel, paste, collapse = ",")

  area <- paste0("/n", nivel, "/", cod_nivel, collapse = "")

  if (length(classificador) != length(cod_cat)) {
    stop("Os argumentos 'classificador' e 'cod_cat' devem ter o mesmo tamanho", call. = FALSE)
  }

  cod_cat <- lapply(cod_cat, paste, collapse = ",")

  cod_cat <- gsub(pattern = "\\+", replacement = "%20", x = cod_cat)

  if (length(classificador) == 0) {
    categ <- classificador
  } else {
    categ <- paste0("/c", classificador, "/", cod_cat, collapse = "")
  }

  url_fixa <- "http://api.sidra.ibge.gov.br/values"

  url_variavel <- paste0("/t/", tabela, "/p/", periodo,
                         "/v/", variavel, area, categ)

  resp <- httr::GET(paste0(url_fixa, url_variavel))

  res <- jsonlite::fromJSON(httr::content(resp, "text",
                                          encoding = "UTF-8"),
                            simplifyVector = FALSE)
  res <- do.call("rbind", res)
  res <- as.data.frame(res)
  res <- lapply(X = res, FUN = do.call, what = c)
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  names(res) <- res[1, ]
  res <- res[-1, ]
  numericas <- c(grep(pattern = "\\(Código\\)", names(res)), length(res))
  res[, numericas] <- suppressWarnings(sapply(res[, numericas], as.numeric))

  return(res)
}

