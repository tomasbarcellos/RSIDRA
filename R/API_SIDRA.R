#' Conexão do R com a SIDRA - IBGE
#'
#' Esta função retorna um data.frame a tabela solicitada.
#' @param tabela Número da tabela.
#' @param classificador Classificador a ser detalhado. Para verificar os classificadores disponíveis na tabela em questão use a função descritores().
#' @param cod_cat Código para definição de subconjunto do classificador. Para verificar as categorias disponíveis na tabela em questão use a função descritores().
#' @param nivel Nível geográfico de agregação dos dados 1 = Brasil e 6 = Município.
#' @param cod_nivel Código contendo conjunto no nível que será selecionado. Pode-se usar o código de determina UF para obter apenas seus dados ou "all" para todos (padrão).
#' @param periodo Período dos dados. O padrão é "all", isto é, todos os anos disponíveis.
#' @param variavel Quais variáveis devem retornar? O padrão é "allxp", isto é, todas exceto aquelas calculadas pela SIDRA (percentuais)
#' @param header Deve retornar com um cabeçalho? Mesmo com header = FALSE, as colunas são nomeadas.
#' @keywords IBGE SIDRA dados
#' @export
#' @examples
#' PAM <- API_SIDRA(1612, 81)
# API_SIDRA

API_SIDRA <- function(tabela,
                      classificador, cod_cat = "all",
                      nivel = 1, cod_nivel = "all",
                      periodo = "all", variavel = "allxp",
                      header = FALSE) {

  url_fixa <- "http://api.sidra.ibge.gov.br/values"

  url_variavel <- paste0("/t/", tabela,
                         "/p/", periodo,
                         "/v/", variavel,
                         paste0("/n", nivel, "/"), cod_nivel,
                         paste0("/c", classificador, "/"), cod_cat,
                         if(header == FALSE) "/h/n")

  res <- rjson::fromJSON(file = paste0(url_fixa, url_variavel))
  res <- do.call("rbind", res)
  res <- as.data.frame(res)
  res <- lapply(X = res, FUN = do.call, what = c)
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  numericas <- which(sapply(res, pode_num))
  res[, numericas] <- sapply(res[, numericas], as.numeric)
  warn_orig <- options("warn")
  options(warn = -1)
  res$V <- as.numeric(res$V)
  options(warn = warn_orig[[1]])


  # ainda falta inserir várias verificações

  return(res)
}

