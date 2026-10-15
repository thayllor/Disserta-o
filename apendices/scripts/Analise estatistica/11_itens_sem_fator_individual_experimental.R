# ============================================================================
# SCRIPT 11 — ITENS SEM FATOR: COMPARAÇÃO INDIVIDUAL DO GRUPO EXPERIMENTAL
# ============================================================================
# OBJETIVO:
# Mostrar o que cada participante respondeu ANTES e DEPOIS em cada pergunta
# que não pertence aos fatores F1–F5. Não cria um fator artificial.
#
# EXECUTE ANTES: somente o Script 1.
#
# PERGUNTAS ANALISADAS: 2, 4, 25, 27, 32, 33 e 34.
# OBSERVAÇÃO: as respostas são as frequências ORIGINAIS (escala 1–5).
# Aumento da resposta NÃO significa necessariamente melhora da habilidade.
# Especialmente no item 34, a direção da mudança depende do enunciado.
# ============================================================================
stopifnot(exists("dados_respostas_originais"),exists("mapa_colunas"),
          exists("itens_sem_fator"),exists("codigos"))
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)

# 1) Escolher apenas os participantes do grupo Experimental.
#    Cada participante deve ter resposta antes e depois.
originais <- dados_respostas_originais %>%
  filter(Grupo=="Experimental",Momento %in% c("Pre","Pos"))

# 2) Converter as sete perguntas em linhas para facilitar a comparação.
#    As colunas originais do Excel contêm o número e o texto da pergunta.
colunas_itens <- unname(mapa_colunas[as.character(itens_sem_fator)])
perguntas <- tibble(Coluna=colunas_itens,
                   Questao=as.integer(itens_sem_fator))

respostas_longa <- originais %>%
  select(Nomes,Grupo,Momento,all_of(colunas_itens)) %>%
  pivot_longer(cols=all_of(colunas_itens),
               names_to="Coluna",values_to="Resposta") %>%
  left_join(perguntas,by="Coluna") %>%
  left_join(codigos %>% select(Nomes,Grupo,Codigo),by=c("Nomes","Grupo"))

# 3) Uma linha para cada participante e pergunta, com pré e pós lado a lado.
#    Diferença = resposta pós - resposta pré (pode ser negativa).
comparacao <- respostas_longa %>%
  select(Codigo,Questao,Coluna,Momento,Resposta) %>%
  pivot_wider(names_from=Momento,values_from=Resposta) %>%
  filter(!is.na(Pre),!is.na(Pos)) %>%
  mutate(Diferenca=Pos-Pre,
         Direcao=case_when(Diferenca>0~"Aumentou a frequência",
                          Diferenca<0~"Diminuiu a frequência",
                          TRUE~"Mesma frequência")) %>%
  arrange(Questao,Codigo)

# 4) TABELA 04: resultados individuais, sem nomes pessoais.
tabela_individual <- comparacao %>%
  transmute(`Participante`=Codigo,
            `Questão`=Questao,
            `Pergunta`=Coluna,
            `Resposta pré`=Pre,
            `Resposta pós`=Pos,
            `Mudança na resposta`=Diferenca,
            `Interpretação descritiva`=Direcao)

# 5) TABELA 05: quantas pessoas aumentaram, reduziram ou mantiveram
#    a frequência em cada pergunta. Não é teste estatístico.
contagem <- comparacao %>%
  count(Questao,Direcao,name="Participantes") %>%
  complete(Questao,
           Direcao=c("Aumentou a frequência","Diminuiu a frequência",
                    "Mesma frequência"),
           fill=list(Participantes=0)) %>%
  arrange(Questao,Direcao)

# 6) TABELA 06: média das respostas ANTES e DEPOIS em cada questão.
#    São médias descritivas das respostas 1–5; não são escores de fator.
resumo <- comparacao %>% group_by(Questao) %>%
  summarise(`Participantes`=n(),
            `Média pré`=round(mean(Pre),2),
            `Média pós`=round(mean(Pos),2),
            `Mudança média`=round(mean(Diferenca),2),
            .groups="drop")

print(tabela_individual,n=Inf)
print(contagem,n=Inf)
print(resumo,n=Inf)

# 7) FIGURA 11: mapa de calor por pergunta e participante.
#    Cada célula é a diferença da resposta original (pós - pré).
#    Verde = respondeu com maior frequência; vermelho = menor frequência.
mapa <- ggplot(comparacao,
               aes(x=factor(Questao),y=Codigo,fill=Diferenca))+
  geom_tile(color="white",linewidth=.5)+
  geom_text(aes(label=sprintf("%+d",as.integer(Diferenca))),size=4)+
  scale_fill_gradient2(low="#B65B5B",mid="white",high="#438C72",midpoint=0,
                       limits=c(-4,4))+
  labs(title="Mudança individual nas perguntas sem fator — Experimental",
       subtitle="Resposta pós menos resposta pré (frequência original de 1 a 5)",
       x="Número da pergunta",y="Participante",fill="Diferença")+
  theme_minimal(base_size=12)+theme(panel.grid=element_blank())
print(mapa)
ggsave("11_FIGURA_Mudanca_individual_perguntas_sem_fator.png",mapa,
       width=11,height=7,dpi=300)

# 8) Exportar tabelas prontas para consultar e inserir no trabalho.
write_xlsx(tabela_individual,
           "TABELA_04_Respostas_individuais_itens_sem_fator_Experimental.xlsx")
write_xlsx(contagem,
           "TABELA_05_Contagem_de_mudancas_itens_sem_fator_Experimental.xlsx")
write_xlsx(resumo,
           "TABELA_06_Medias_por_pergunta_sem_fator_Experimental.xlsx")

cat("\nATENÇÃO: mudanças na frequência de resposta não equivalem, por si só,\n",
    "a melhora ou piora nas habilidades sociais.\n")
