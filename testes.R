library(RSIDRA)

tab <- 1612

categ <- SIDRA_classificacao(tab)
categ <- unique(categ$classificacao)

tab == SIDRA_tabelas(tab)$tabela

niveis <- SIDRA_nivel(tab)$nível

var <- SIDRA_variaveis(tab)

PAM <- API_SIDRA(tabela = tab,
                 classificador = categ,
                 nivel = niveis[1])
str(PAM)

# todas_tabelas <- SIDRA_tabelas()
# save(todas_tabelas, "tabelas.RDS")
