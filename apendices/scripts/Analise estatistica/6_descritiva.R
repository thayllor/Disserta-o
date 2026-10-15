# =====================================================================
# 6 — RESUMIR A MUDANÇA MÉDIA
# =====================================================================
# PARA QUE SERVE:
# Calcula as médias pré, pós e da mudança de cada grupo em cada fator.
#
# O QUE EXECUTAR ANTES:
# Script 1.
#
# O QUE VOCÊ RECEBE:
# Tabela resumida e gráfico de barras. Não é um teste estatístico.
#
# COMO LER OS RESULTADOS:
# Pré = medida antes do experimento; Pós = medida depois.
# Mudança ou Delta = Pós - Pré. Positivo = aumento no indicador;
# negativo = redução; zero = nenhuma mudança observada.
# Os fatores F1 a F5 são indicadores exploratórios, NÃO os escores
# oficiais padronizados do IHS. Não confunda aumento com prova de
# melhora clínica ou de causalidade.
# =====================================================================

# SCRIPT 6 — MUDANÇA MÉDIA POR FATOR (DESCRITIVA)
stopifnot(exists("pares_fatores"),exists("rotulos_fatores"))
# Resume quantas pessoas há e quais foram as médias pré e pós.
resumo_fatores <- pares_fatores %>% group_by(Grupo,Fator) %>%
 summarise(N=n(),Pre=mean(Pre),Pos=mean(Pos),Delta=mean(Delta),
           .groups="drop")
print(resumo_fatores,n=Inf)
# Desenha barras com a mudança média de cada fator e grupo.
g <- ggplot(resumo_fatores,aes(x=Fator,y=Delta,fill=Grupo))+
 geom_hline(yintercept=0,linetype="dashed")+
 geom_col(position="dodge")+
 scale_x_discrete(labels=rotulos_fatores)+
 labs(title="Mudança média observada por fator",y="Média das mudanças individuais",x="Fator")+
 theme_minimal()+theme(axis.text.x=element_text(angle=25,hjust=1,size=8))
print(g)
ggsave("06_mudancas_fatores.png",g,width=15,height=7,dpi=300)
# Salva a tabela de médias.
# A coluna Dimensão avaliada mostra o nome completo sem alterar os cálculos.
resumo_fatores_publicacao <- resumo_fatores %>% mutate(`Dimensão avaliada`=unname(nomes_fatores[Fator]),.after=Fator)
write_xlsx(resumo_fatores_publicacao,"06_descritiva_fatores.xlsx")
