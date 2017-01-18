#' Conexão do R com a SIDRA - IBGE
#'
#' Verifica se um vetor pode ou nao ser convertido em numérico em . Retorna um valor lógico.
#' @param x Um vetor.
#' @keywords numeric coercion
#' @examples
#' PAM <- API_SIDRA(1612, 1, categoria = 81)
#' numericos <- sapply(PAM, pode_num)

pode_num <- <- function(x) {
  numeros <- sum(!is.na(as.numeric(x)))
  return(all(length(x) == numeros))
}