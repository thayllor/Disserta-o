# =====================================================================
# 5 — VER A VARIAÇÃO DAS MUDANÇAS
# =====================================================================
# PARA QUE SERVE:
# Mostra se as mudanças são semelhantes ou muito diferentes entre participantes.
#
# O QUE EXECUTAR ANTES:
# Script 1.
#
# O QUE VOCÊ RECEBE:
# Tabela com dispersão e gráficos de caixa (boxplots) para cada fator.
#
# COMO LER OS RESULTADOS:
# Pré = medida antes do experimento; Pós = medida depois.
# Mudança ou Delta = Pós - Pré. Positivo = aumento no indicador;
# negativo = redução; zero = nenhuma mudança observada.
# Os fatores F1 a F5 são indicadores exploratórios, NÃO os escores
# oficiais padronizados do IHS. Não confunda aumento com prova de
# melhora clínica ou de causalidade.
# =====================================================================

# SCRIPT 5 — VARIABILIDADE DAS MUDANÇAS POR FATOR E GRUPO
stopifnot(exists("pares_fatores"),exists("rotulos_fatores"))
# Calcula média, mediana, desvio-padrão, intervalo interquartil, mínimo e máximo.
variabilidade <- pares_fatores %>% group_by(Grupo,Fator) %>%
 summarise(N=n(),Media=mean(Delta),Mediana=median(Delta),
           DP=sd(Delta),IQR=IQR(Delta),Min=min(Delta),Max=max(Delta),
           .groups="drop")
print(variabilidade,n=Inf)
# Em vez de colocar os cinco fatores na mesma figura, geramos
# CINCO gráficos: um por fator. Cada ponto é um participante.
for (fator_atual in sort(unique(pares_fatores$Fator))) {
  d <- dplyr::filter(pares_fatores, Fator == fator_atual)
  grafico <- ggplot(d, aes(x=Grupo, y=Delta, fill=Grupo)) +
    geom_hline(yintercept=0, linetype="dashed", color="gray50") +
    geom_boxplot(width=.42, alpha=.35, outlier.shape=NA) +
    geom_jitter(width=.09, height=0, size=2.5, alpha=.8) +
    labs(title=paste("Distribuição das mudanças —",rotulos_fatores[[fator_atual]]),
         subtitle="Cada ponto representa um participante; linha tracejada = nenhuma mudança",
         x="Grupo", y="Mudança no fator (pós − pré)") +
    theme_minimal(base_size=13) + theme(legend.position="none")
  print(grafico)
  ggsave(paste0("05_variabilidade_",fator_atual,".png"),grafico,
         width=7,height=5,dpi=300)
}
# Tabela de apoio; não confundir dispersão com significância estatística.
# A coluna Dimensão avaliada mostra o nome completo sem alterar os cálculos.
variabilidade_publicacao <- variabilidade %>% mutate(`Dimensão avaliada`=unname(nomes_fatores[Fator]),.after=Fator)
writexl::write_xlsx(variabilidade_publicacao,"05_Tabela_variabilidade_por_fator.xlsx")
