# =====================================================================
# 4 — COMPARAR CADA PARTICIPANTE
# =====================================================================
# PARA QUE SERVE:
# Mostra quanto cada pessoa mudou em cada um dos cinco fatores.
#
# O QUE EXECUTAR ANTES:
# Script 1.
#
# O QUE VOCÊ RECEBE:
# Tabela detalhada, fatores lado a lado, contagem de aumentos/reduções e mapa de calor.
#
# COMO LER OS RESULTADOS:
# Pré = medida antes do experimento; Pós = medida depois.
# Mudança ou Delta = Pós - Pré. Positivo = aumento no indicador;
# negativo = redução; zero = nenhuma mudança observada.
# Os fatores F1 a F5 são indicadores exploratórios, NÃO os escores
# oficiais padronizados do IHS. Não confunda aumento com prova de
# melhora clínica ou de causalidade.
# =====================================================================

# ================================================================
# SCRIPT 4 — COMPARAÇÃO INDIVIDUAL DOS CINCO FATORES
# Execute o Script 1 antes deste.
# Não calcula escore total do IHS: cada fator é independente.
# ================================================================
stopifnot(exists("pares_fatores"),exists("rotulos_fatores"))
library(dplyr); library(tidyr); library(ggplot2); library(writexl)

# 1) Tabela detalhada: uma linha por pessoa e fator.
# Delta = Pós - Pré; valores positivos indicam mudança favorável
# no indicador exploratório (não equivalem a melhora clínica).
# Uma linha para cada pessoa em cada fator, com pré, pós e diferença.
individuais <- pares_fatores %>%
  transmute(Participante=Codigo, Grupo, Fator,
            `Média pré`=Pre, `Média pós`=Pos,
            `Mudança (Δ)`=Delta,
            Resultado=case_when(Delta>0~"Aumento",Delta<0~"Redução",
                                TRUE~"Sem mudança")) %>%
  arrange(Grupo,Participante,Fator)

# 2) Tabela compacta: uma linha por pessoa; F1...F5 são deltas.
# Coloca F1 a F5 em colunas para facilitar a comparação entre pessoas.
comparativo_individual <- individuais %>%
  select(Participante,Grupo,Fator,`Mudança (Δ)`) %>%
  pivot_wider(names_from=Fator,values_from=`Mudança (Δ)`) %>%
  arrange(Grupo,Participante)

# 3) Resumo individual: contagem de fatores com aumento/redução.
# Não se trata de pontuação total nem de ranking clínico.
# Conta quantos fatores aumentaram, diminuíram ou não mudaram.
resumo_individual <- individuais %>% group_by(Grupo,Participante) %>%
 summarise(`Fatores com aumento`=sum(`Mudança (Δ)`>0),
           `Fatores com redução`=sum(`Mudança (Δ)`<0),
           `Fatores sem mudança`=sum(`Mudança (Δ)`==0),
           .groups="drop")

cat("\nCOMPARAÇÃO INDIVIDUAL (pré, pós e mudança):\n")
print(individuais,n=Inf)
cat("\nMUDANÇAS NOS CINCO FATORES POR PARTICIPANTE:\n")
print(comparativo_individual,n=Inf)

# 4) Mapa de calor: participantes × fatores; zero = sem mudança.
# Cada quadrado representa a mudança de uma pessoa em um fator.
heatmap <- ggplot(individuais,aes(x=Fator,y=Participante,fill=`Mudança (Δ)`))+
 geom_tile(color="white",linewidth=.5)+
 geom_text(aes(label=sprintf("%+.2f",`Mudança (Δ)`)),size=3.4)+
 facet_grid(Grupo~.,scales="free_y",space="free_y")+
 scale_fill_gradient2(low="#B65B5B",mid="white",high="#438C72",midpoint=0)+
 labs(title="Mudança individual nos cinco fatores",
      subtitle="Diferença entre os indicadores pós e pré-experimento",
      x="Fator psicológico",y="Participante",fill="Mudança")+
 theme_minimal(base_size=12)+
 scale_x_discrete(labels=rotulos_fatores)+
 theme(panel.grid=element_blank(),strip.text=element_text(face="bold"),
       axis.text.x=element_text(angle=28,hjust=1,size=8))
print(heatmap)
# Salva o mapa de calor como imagem em alta resolução.
ggsave("04_mapa_calor_individual.png",heatmap,width=14,height=9,dpi=300)

# 5) Exportação: resultados brutos completos e versões legíveis.
# Salva as três tabelas em um único arquivo Excel.
write_xlsx(list(`Pré Pós e Mudança`=individuais,
                `Comparação lado a lado`=comparativo_individual,
                `Resumo por participante`=resumo_individual),
           "04_comparacao_individual.xlsx")
