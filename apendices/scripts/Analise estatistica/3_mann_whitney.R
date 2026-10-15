# =====================================================================
# 3 — COMPARAR AS MUDANÇAS ENTRE EXPERIMENTAL E CONTROLE
# =====================================================================
# PARA QUE SERVE:
# Compara a mudança (pós menos pré) de um grupo com a do outro.
#
# O QUE EXECUTAR ANTES:
# Script 1.
#
# O QUE VOCÊ RECEBE:
# resultados_mann: médias das mudanças, diferença entre grupos, U e p corrigido.
#
# COMO LER OS RESULTADOS:
# Pré = medida antes do experimento; Pós = medida depois.
# Mudança ou Delta = Pós - Pré. Positivo = aumento no indicador;
# negativo = redução; zero = nenhuma mudança observada.
# Os fatores F1 a F5 são indicadores exploratórios, NÃO os escores
# oficiais padronizados do IHS. Não confunda aumento com prova de
# melhora clínica ou de causalidade.
# =====================================================================

# SCRIPT 3 — MANN–WHITNEY: DELTAS EXPERIMENTAL × CONTROLE
stopifnot(exists("pares_fatores"))
# Repete a comparação para F1, F2, F3, F4 e F5.
resultados_mann <- bind_rows(lapply(unique(pares_fatores$Fator),function(f){
 d <- filter(pares_fatores,Fator==f)
# Mudanças dos participantes do grupo Experimental.
 a <- filter(d,Grupo=="Experimental")$Delta
# Mudanças dos participantes do grupo Controle.
 b <- filter(d,Grupo=="Controle")$Delta
# O Mann–Whitney compara dois grupos de pessoas diferentes.
 t <- if(length(a)>0 && length(b)>0)
   wilcox.test(a,b,paired=FALSE,exact=FALSE) else NULL
 tibble(Fator=f,N_Exp=length(a),N_Ctrl=length(b),
        Delta_Exp=mean(a),Delta_Ctrl=mean(b),
        Diferenca_Medias=mean(a)-mean(b),
        U=if(is.null(t)) NA_real_ else unname(t$statistic),
        p_valor=if(is.null(t)) NA_real_ else t$p.value)
})) %>% mutate(p_Holm=p.adjust(p_valor,method="holm"))
print(resultados_mann,n=Inf)
# Exporta os resultados para uma planilha.
# A coluna Dimensão avaliada mostra o nome completo sem alterar os cálculos.
resultados_mann_publicacao <- resultados_mann %>% mutate(`Dimensão avaliada`=unname(nomes_fatores[Fator]),.after=Fator)
write_xlsx(resultados_mann_publicacao,"03_mann_whitney_fatores.xlsx")
