library(RSIDRA)

categ <- SIDRA_classificacao(1612)
categ <- unique(categ$classificacao) %>%
  sub(pattern = "C", replacement = "") # tem que mudar

tbl <- SIDRA_tabelas(1612)$tabela

niveis <- SIDRA_nivel(1612)$nível %>%
  sub(pattern = "N", replacement = "") # tem que mudar

var <- SIDRA_variaveis(1612)

PAM <- API_SIDRA(tabela = tbl,
                 classificador = categ,
                 nivel = niveis[1])
str(PAM)

todas_tabelas <- SIDRA_tabelas()

save(todas_tabelas, "tabelas.RDS")
