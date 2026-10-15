# =====================================================================
# 2 — COMPARAR PRÉ E PÓS DENTRO DE CADA GRUPO
# =====================================================================
# PARA QUE SERVE:
# Testa se os mesmos participantes mudaram entre o início e o fim, para cada fator.
#
# O QUE EXECUTAR ANTES:
# Script 1.
#
# O QUE VOCÊ RECEBE:
# resultados_wilcoxon: número de pessoas, médias, estatística W, p e p corrigido.
#
# COMO LER OS RESULTADOS:
# Pré = medida antes do experimento; Pós = medida depois.
# Mudança ou Delta = Pós - Pré. Positivo = aumento no indicador;
# negativo = redução; zero = nenhuma mudança observada.
# Os fatores F1 a F5 são indicadores exploratórios, NÃO os escores
# oficiais padronizados do IHS. Não confunda aumento com prova de
# melhora clínica ou de causalidade.
# =====================================================================

# SCRIPT 2 — WILCOXON PAREADO PRÉ × PÓS POR FATOR
# Dependência: executar 1_preparacao.R.
stopifnot(exists("pares_fatores"))
# Para cada grupo e fator, compara as respostas antes e depois.
resultados_wilcoxon <- pares_fatores %>% group_by(Grupo,Fator) %>%
 group_modify(~{
  x <- .x$Pre; y <- .x$Pos
# O Wilcoxon pareado usa os mesmos participantes nos dois momentos.
  teste <- if(length(x)>=2 && any(x!=y)) wilcox.test(y,x,paired=TRUE,exact=FALSE) else NULL
  tibble(N=length(x),Media_Pre=mean(x),Media_Pos=mean(y),
         Media_Delta=mean(y-x),Mediana_Delta=median(y-x),
         W=if(is.null(teste)) NA_real_ else unname(teste$statistic),
         p_valor=if(is.null(teste)) NA_real_ else teste$p.value)
 }) %>% ungroup() %>% group_by(Grupo) %>%
# Holm corrige os valores de p porque fazemos vários testes.
 mutate(p_Holm=p.adjust(p_valor,method="holm")) %>% ungroup()
print(resultados_wilcoxon,n=Inf)
# Salva a tabela completa em Excel.
# A coluna Dimensão avaliada mostra o nome completo sem alterar os cálculos.
resultados_wilcoxon_publicacao <- resultados_wilcoxon %>% mutate(`Dimensão avaliada`=unname(nomes_fatores[Fator]),.after=Fator)
write_xlsx(resultados_wilcoxon_publicacao,"02_wilcoxon_fatores.xlsx")
# p < 0,05 sugere evidência de mudança; não prova causalidade.
