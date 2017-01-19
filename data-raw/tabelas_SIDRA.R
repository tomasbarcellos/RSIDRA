requireNamespace("tools", quietly = TRUE)
requireNamespace("devtools", quietly = TRUE)
# A linha abaixo demora a rodar
tabelas_SIDRA <- SIDRA_tabelas()
use_data(tabelas_SIDRA)
use_data(tabelas_SIDRA,
         compress = checkRdaFiles("data/tabelas_SIDRA.rda")$compress,
         overwrite = TRUE)
