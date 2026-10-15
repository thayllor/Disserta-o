# =====================================================================
# 7 — MEDIR O TAMANHO DA DIFERENÇA ENTRE GRUPOS
# =====================================================================
# PARA QUE SERVE:
# Calcula a correlação bisserial por postos a partir do teste de Mann–Whitney.
#
# O QUE EXECUTAR ANTES:
# Scripts 1 e 3.
#
# O QUE VOCÊ RECEBE:
# r bisserial: varia de -1 a +1; o sinal indica qual grupo tende a ter maior mudança.
#
# COMO LER OS RESULTADOS:
# Pré = medida antes do experimento; Pós = medida depois.
# Mudança ou Delta = Pós - Pré. Positivo = aumento no indicador;
# negativo = redução; zero = nenhuma mudança observada.
# Os fatores F1 a F5 são indicadores exploratórios, NÃO os escores
# oficiais padronizados do IHS. Não confunda aumento com prova de
# melhora clínica ou de causalidade.
# =====================================================================

# SCRIPT 7 — CORRELAÇÃO BISSERIAL POR POSTOS
# r = 2U/(n_exp*n_ctrl)-1; positivo favorece Experimental.
stopifnot(exists("resultados_mann"))
# Usa o valor U já calculado pelo Script 3.
efeitos <- resultados_mann %>% mutate(
# Fórmula: 2 × U / (número experimental × número controle) − 1.
 r_Bisserial=ifelse(N_Exp*N_Ctrl>0,2*U/(N_Exp*N_Ctrl)-1,NA_real_),
# Classificação apenas descritiva; não substitui interpretação do estudo.
 Magnitude=case_when(abs(r_Bisserial)<.1~"Desprezível",
                     abs(r_Bisserial)<.3~"Pequena",
                     abs(r_Bisserial)<.5~"Moderada",
                     TRUE~"Grande"),
 Direcao=case_when(r_Bisserial>0~"Experimental",
                   r_Bisserial<0~"Controle",TRUE~"Sem predominância"))
print(efeitos,n=Inf)
# O r bisserial compara a MUDANÇA do grupo Experimental com a do Controle.
# Não é o tamanho de efeito do Experimental isoladamente.
# Valores positivos: maior mudança no Experimental; negativos: no Controle.
g <- ggplot(efeitos, aes(x=reorder(Fator,r_Bisserial), y=r_Bisserial)) +
  geom_hline(yintercept=0,linetype="dashed",color="gray40") +
  geom_col(fill="#537C9B",width=.55) +
  geom_text(aes(label=sprintf("%+.2f",r_Bisserial)),
            hjust=ifelse(efeitos$r_Bisserial>=0,-.15,1.15),size=4) +
  coord_flip(ylim=c(-1.1,1.1)) +
  scale_x_discrete(labels=rotulos_fatores)+
  labs(title="Tamanho do efeito: Experimental versus Controle",
       subtitle="Comparação das mudanças pré–pós de cada grupo, em cada fator",
       x="Fator", y="r bisserial por postos (−1 a +1)",
       caption="Positivo = maior mudança no Experimental; negativo = maior mudança no Controle. Não indica significância.") +
  theme_minimal(base_size=12)+theme(axis.text.x=element_text(angle=25,hjust=1,size=8))
print(g)
ggsave("07_Efeito_entre_grupos_por_fator.png",g,width=15,height=7,dpi=300)
# A coluna Dimensão avaliada mostra o nome completo sem alterar os cálculos.
efeitos_publicacao <- efeitos %>% mutate(`Dimensão avaliada`=unname(nomes_fatores[Fator]),.after=Fator)
writexl::write_xlsx(efeitos_publicacao,"07_Tabela_tamanho_efeito_entre_grupos.xlsx")
