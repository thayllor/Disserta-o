# =====================================================================
# 1 — IMPORTAR E PREPARAR OS DADOS
# =====================================================================
# PARA QUE SERVE:
# Lê a planilha, identifica as perguntas do IHS, calcula os cinco fatores e junta o pré com o pós da mesma pessoa.
#
# O QUE EXECUTAR ANTES:
# Nenhum. Execute este script primeiro.
#
# O QUE VOCÊ RECEBE:
# dados: respostas importadas; fatores: perguntas de cada fator; pares_fatores: uma linha por pessoa e fator, com Pre, Pos e Delta.
#
# COMO LER OS RESULTADOS:
# Pré = medida antes do experimento; Pós = medida depois.
# Mudança ou Delta = Pós - Pré. Positivo = aumento no indicador;
# negativo = redução; zero = nenhuma mudança observada.
# Os fatores F1 a F5 são indicadores exploratórios, NÃO os escores
# oficiais padronizados do IHS. Não confunda aumento com prova de
# melhora clínica ou de causalidade.
# =====================================================================

# SCRIPT 1 — PREPARAÇÃO DOS INDICADORES EXPLORATÓRIOS F1–F5
# Execute este arquivo antes de todos os demais.
# Não calcula escores T, percentis ou escores oficiais do IHS.
library(readxl); library(dplyr); library(tidyr); library(ggplot2); library(writexl)
# Abre a janela para você escolher o arquivo Excel no computador.
arquivo <- file.choose()
# Quatro linhas iniciais: fator, habilidade, número, enunciado.
dados <- read_excel(arquivo, sheet="Comparação controle", skip=3)
stopifnot(all(c("Nomes","Grupo","questionario") %in% names(dados)))
# Questões 1–38 estão nas colunas cujo nome começa pelo número e ponto.
# Procura as colunas das questões 1 até 38 pelo número no começo do título.
cols <- names(dados)[grepl("^([1-9]|[12][0-9]|3[0-8])\\.", names(dados))]
numeros <- as.integer(sub("\\..*$","",cols))
if (length(cols)!=38 || !setequal(numeros,1:38))
  stop("Não foram identificados exatamente os 38 itens originais.")
mapa_colunas <- setNames(cols,as.character(numeros))
# Distribuição conforme protocolo de apuração. Item 14 pertence a F1 e F4.
# Cada fator reúne perguntas diferentes. A questão 14 entra em F1 e F4.
fatores <- list(
 F1=c(21,16,15,29,20,11,14,5,1,12,7),
 F2=c(3,28,8,35,30,6,10),
 F3=c(17,37,36,13,22,24,19),
 F4=c(23,26,9,14),
 F5=c(18,38,31)
)
# NOMES OFICIAIS DOS FATORES (extraídos do protocolo apresentado).
# O código F1–F5 permanece nos cálculos para não quebrar os scripts.
nomes_fatores <- c(
 F1="Enfrentamento e autoafirmação com risco",
 F2="Autoafirmação na expressão de sentimento positivo",
 F3="Conversação e desenvoltura social",
 F4="Autoexposição a desconhecidos e situações novas",
 F5="Autocontrole da agressividade"
)
# Nome legível para gráficos: código + nome do fator.
rotulos_fatores <- paste0(names(nomes_fatores)," — ",nomes_fatores)
names(rotulos_fatores) <- names(nomes_fatores)

# ITENS QUE NÃO PERTENCEM A NENHUM DOS CINCO FATORES.
# As respostas originais precisam ser preservadas antes de qualquer inversão.
itens_sem_fator <- setdiff(1:38,unique(unlist(fatores)))
# Esperado: 2, 4, 25, 27, 32, 33 e 34.
stopifnot(setequal(itens_sem_fator,c(2,4,25,27,32,33,34)))

# Orientação EXPLORATÓRIA segundo a ação descrita nos itens:
# para estes itens, frequência maior significa menor habilidade.
# A questão 31 é invertida exclusivamente no indicador F5, por seu
# peso negativo no protocolo; não equivale ao cálculo oficial do Resultado T.
# Itens marcados aqui têm sua direção invertida na escala de 1 a 5.
invertidos <- c(8,9,13,17,18,19,22,23,24,26,31,34,36,37)
# OBS: a lista acima deve ser revisada contra os enunciados antes da publicação.
# O item 4 e os itens sem fator não entram nos indicadores.
dados$Nomes <- trimws(as.character(dados$Nomes))
dados$Grupo <- trimws(as.character(dados$Grupo))
dados$questionario <- trimws(as.character(dados$questionario))
dados$Grupo[dados$Grupo=="Experimento"] <- "Experimental"
# Identifica se a resposta é do começo (Pre) ou do final (Pos).
dados$Momento <- ifelse(grepl("^Pre",dados$questionario,ignore.case=TRUE),
                        "Pre",ifelse(grepl("^P[oó]s",dados$questionario,ignore.case=TRUE),
                                     "Pos",NA_character_))
dados <- dados %>% filter(Grupo %in% c("Experimental","Controle"),!is.na(Momento),
                          !is.na(Nomes),Nomes!="")
if(anyDuplicated(dados[,c("Nomes","Grupo","Momento")]))
 stop("Participantes duplicados no mesmo grupo e momento.")
# Confere se as respostas são números de 1 a 5.
for (numero in 1:38) {
 col <- mapa_colunas[[as.character(numero)]]
 dados[[col]] <- suppressWarnings(as.numeric(dados[[col]]))
 if(any(!is.na(dados[[col]]) & !(dados[[col]] %in% 1:5)))
  stop(paste("Resposta fora da escala 1–5 no item",numero))
}
# Guarda respostas BRUTAS para a análise individual dos itens sem fator.
# A cópia conserva a frequência original marcada no questionário (1 a 5).
dados_respostas_originais <- dados

# Primeiro orientar respostas: positivo significa maior habilidade.
# Para os itens invertidos: 1 vira 5; 2 vira 4; 3 continua 3.
for(numero in invertidos){
 col <- mapa_colunas[[as.character(numero)]]
 dados[[col]] <- ifelse(is.na(dados[[col]]),NA_real_,6-dados[[col]])
}
# Indicadores: média dos itens orientados, calculada APENAS se todos
# os itens daquele fator estiverem respondidos.
# Calcula a média das perguntas de cada fator para cada resposta.
for(f in names(fatores)){
 cs <- unname(mapa_colunas[as.character(fatores[[f]])])
 m <- as.matrix(dados[,cs])
 dados[[f]] <- ifelse(rowSums(!is.na(m))==ncol(m),rowMeans(m,na.rm=TRUE),NA_real_)
}
# Pareamento individual; Nomes é usado internamente, não em arquivos exportados.
long <- dados %>% select(Nomes,Grupo,Momento,all_of(names(fatores))) %>%
 pivot_longer(cols=all_of(names(fatores)),names_to="Fator",values_to="Escore")
# Junta pré e pós da MESMA pessoa para calcular a mudança individual.
pares_fatores <- long %>% pivot_wider(names_from=Momento,values_from=Escore) %>%
 filter(!is.na(Pre),!is.na(Pos)) %>%
 mutate(Delta=Pos-Pre)
# Código anônimo estável, por ordem alfabética dentro de cada grupo.
# Cria códigos anônimos, como E01 e C01, para tabelas e gráficos.
codigos <- pares_fatores %>% distinct(Grupo,Nomes) %>%
 arrange(Grupo,Nomes) %>% group_by(Grupo) %>%
 mutate(Codigo=paste0(ifelse(Grupo=="Experimental","E","C"),
                      sprintf("%02d",row_number()))) %>% ungroup()
pares_fatores <- pares_fatores %>% left_join(codigos,by=c("Grupo","Nomes"))
cat("\nParticipantes pareados por fator:\n")
print(pares_fatores %>% count(Grupo,Fator))
cat("\nATENÇÃO: indicadores exploratórios (médias orientadas), NÃO escores normativos.\n")




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



# =====================================================================
# 8 — RELACIONAR SESSÕES E MUDANÇA
# =====================================================================
# PARA QUE SERVE:
# Verifica visualmente se quem participou de mais sessões teve mudanças maiores.
#
# O QUE EXECUTAR ANTES:
# Script 1.
#
# O QUE VOCÊ RECEBE:
# Tabela e gráfico de dispersão por fator; não prova causa e efeito.
#
# COMO LER OS RESULTADOS:
# Pré = medida antes do experimento; Pós = medida depois.
# Mudança ou Delta = Pós - Pré. Positivo = aumento no indicador;
# negativo = redução; zero = nenhuma mudança observada.
# Os fatores F1 a F5 são indicadores exploratórios, NÃO os escores
# oficiais padronizados do IHS. Não confunda aumento com prova de
# melhora clínica ou de causalidade.
# =====================================================================

# SCRIPT 8 — SESSÕES PARTICIPADAS × MUDANÇA POR FATOR
# Execute 1_preparacao.R antes; a segunda aba é lida do mesmo arquivo.
stopifnot(exists("pares_fatores"),exists("arquivo"))
# Lê a segunda aba, que contém a pergunta sobre sessões frequentadas.
q <- readxl::read_excel(arquivo,sheet=2,skip=1)
# Identifica a coluna que guarda os nomes dos participantes.
nome <- names(q)[tolower(trimws(names(q)))=="nomes"]
# Localiza a pergunta que informa o número de sessões.
ses <- grep("Quantas se",names(q),ignore.case=TRUE,value=TRUE)
if(length(nome)!=1 || length(ses)!=1)
 stop("Confira os cabeçalhos Nomes e Quantas seções/sessões na segunda aba.")
# Monta uma tabela simples com nome e quantidade de sessões.
sessoes <- tibble(Nomes=trimws(as.character(q[[nome]])),
                 Sessoes=suppressWarnings(as.numeric(
                   sub("^\\s*([0-9]+).*$","\\1",as.character(q[[ses]])))))
sessoes <- sessoes %>% filter(!is.na(Sessoes),!is.na(Nomes),Nomes!="")
if(anyDuplicated(sessoes$Nomes))stop("Respostas de sessões duplicadas.")
# Junta o número de sessões às mudanças calculadas no Script 1.
dados_sessoes <- pares_fatores %>% filter(Grupo=="Experimental") %>%
 left_join(sessoes,by="Nomes")
if(anyNA(dados_sessoes$Sessoes))
 warning("Há participantes sem número de sessões; serão omitidos no gráfico.")
dados_sessoes <- dados_sessoes %>% filter(!is.na(Sessoes)) %>%
 select(Codigo,Fator,Pre,Pos,Delta,Sessoes)
print(dados_sessoes,n=Inf)
# TABELA 1: uma linha por participante, com sessões e as cinco mudanças.
# Assim podemos comparar pessoas sem sobrepor pontos e rótulos.
tabela_sessoes <- dados_sessoes %>%
  select(Codigo,Sessoes,Fator,Delta) %>%
  tidyr::pivot_wider(names_from=Fator,values_from=Delta) %>%
  arrange(desc(Sessoes),Codigo)
print(tabela_sessoes,n=Inf)
writexl::write_xlsx(tabela_sessoes,"08_Tabela_sessoes_e_mudancas_individuais.xlsx")

# GRÁFICOS: um fator por imagem. Pontos são pessoas; o número de sessões
# aparece no eixo vertical, e as pessoas no horizontal, ordenadas por sessões.
# Cor azul = aumento; vermelho = redução; cinza = sem mudança.
for (fator_atual in sort(unique(dados_sessoes$Fator))) {
  d <- dplyr::filter(dados_sessoes,Fator==fator_atual) %>%
    arrange(Sessoes,Codigo) %>% mutate(Codigo=factor(Codigo,levels=Codigo))
  g <- ggplot(d,aes(x=Codigo,y=Delta,fill=Delta>0)) +
    geom_hline(yintercept=0,linetype="dashed",color="gray50") +
    geom_col(width=.65,show.legend=FALSE) +
    geom_text(aes(label=paste0(Sessoes," sessões")),
              vjust=ifelse(d$Delta>=0,-.35,1.3),size=3.5) +
    scale_fill_manual(values=c("TRUE"="#438C72","FALSE"="#C16A65")) +
    coord_cartesian(clip="off") +
    labs(title=paste("Participação e mudança —",rotulos_fatores[[fator_atual]]),
         subtitle="Cada barra é um participante; rótulo = sessões frequentadas",
         x="Participante",y="Mudança no fator (pós − pré)") +
    theme_minimal(base_size=12) +
    theme(plot.margin=margin(15,15,10,10))
  print(g)
  ggsave(paste0("08_sessoes_",fator_atual,".png"),g,
         width=9,height=5.5,dpi=300)
}
# Não interpretar visualmente como correlação significativa.
# =====================================================================
# 9 — ORGANIZAR AS TABELAS PARA O TRABALHO
# =====================================================================
# PARA QUE SERVE:
# Reúne os resultados principais em tabelas com títulos simples.
#
# O QUE EXECUTAR ANTES:
# Scripts 1, 2, 3 e 7.
#
# O QUE VOCÊ RECEBE:
# Um Excel com pré/pós, comparação dos grupos e evolução individual.
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
# SCRIPT 9 — TABELAS SIMPLES PARA DISSERTAÇÃO
# Execute os scripts 1, 2, 3 e 7 antes deste.
# Não altera nenhum cálculo: apenas organiza e arredonda a saída.
# ================================================================
stopifnot(exists("resultados_wilcoxon"),exists("resultados_mann"),
          exists("efeitos"),exists("pares_fatores"))
library(dplyr); library(tidyr); library(writexl)

# Tabela A — pré e pós por grupo e fator (Wilcoxon).
# Tabela A: médias antes e depois e teste de Wilcoxon por grupo.
tabela_pre_pos <- resultados_wilcoxon %>%
 transmute(Fator,Grupo,Participantes=N,
           `Pré (média)`=round(Media_Pre,2),
           `Pós (média)`=round(Media_Pos,2),
           `Mudança média`=round(Media_Delta,2),
           `p ajustado (Holm)`=round(p_Holm,3)) %>%
 arrange(Fator,Grupo)

# Tabela B — diferença das mudanças entre os grupos e tamanho de efeito.
# Tabela B: compara mudanças e inclui o tamanho do efeito.
tabela_entre_grupos <- efeitos %>%
 transmute(Fator,`N experimental`=N_Exp,`N controle`=N_Ctrl,
           `Mudança experimental`=round(Delta_Exp,2),
           `Mudança controle`=round(Delta_Ctrl,2),
           `Diferença entre grupos`=round(Diferenca_Medias,2),
           `p ajustado (Holm)`=round(p_Holm,3),
           `r bisserial`=round(r_Bisserial,2),Magnitude,Direcao) %>%
 arrange(Fator)

# Tabela C — evolução de cada participante por fator.
# Tabela C: mostra a mudança de cada pessoa em cada fator.
tabela_individual <- pares_fatores %>%
 transmute(Participante=Codigo,Grupo,Fator,
           Pré=round(Pre,2),Pós=round(Pos,2),
           Mudança=round(Delta,2)) %>% arrange(Grupo,Participante,Fator)

print(tabela_pre_pos,n=Inf)
print(tabela_entre_grupos,n=Inf)
print(tabela_individual,n=Inf)
# Coluna com o nome completo para uso direto no texto da dissertação.
# Preserva a coluna Fator como identificador, mas deixa o significado explícito.
stopifnot(exists("nomes_fatores"))
tabela_pre_pos <- tabela_pre_pos %>% mutate(`Dimensão avaliada`=unname(nomes_fatores[Fator]),.after=Fator)
tabela_entre_grupos <- tabela_entre_grupos %>% mutate(`Dimensão avaliada`=unname(nomes_fatores[Fator]),.after=Fator)
tabela_individual <- tabela_individual %>% mutate(`Dimensão avaliada`=unname(nomes_fatores[Fator]),.after=Fator)

# Salva as três tabelas em abas diferentes do mesmo Excel.
write_xlsx(list(`Pré e Pós`=tabela_pre_pos,
                `Comparação de grupos`=tabela_entre_grupos,
                `Evolução individual`=tabela_individual),
           "09_tabelas_dissertacao.xlsx")
# NOTAS PARA O TEXTO:
# Δ = pós menos pré. p ajustado por Holm dentro da família de testes.
# Valores são indicadores exploratórios, não escores normativos oficiais.
# p>=0,05 não demonstra ausência de efeito; n pequeno limita precisão.

# Arquivos separados com nomes claros, para localizar cada tabela no trabalho.
write_xlsx(tabela_pre_pos,"TABELA_01_Pre_Pos_Wilcoxon_por_Grupo_e_Fator.xlsx")
write_xlsx(tabela_entre_grupos,"TABELA_02_Experimental_vs_Controle_MannWhitney_Efeito.xlsx")
write_xlsx(tabela_individual,"TABELA_03_Pre_Pos_e_Mudanca_de_Cada_Participante.xlsx")



# =====================================================================
# SCRIPT 10 — OUTRAS ANÁLISES INDIVIDUAIS (F1–F5)
# Execute o Script 1 antes. Não depende dos testes estatísticos.
# =====================================================================
# Perguntas: quem mais aumentou/reduziu em cada fator?
# Quantos participantes aumentaram, diminuíram ou ficaram iguais?
# As diferenças são descritivas; não são escores normativos oficiais.
stopifnot(exists("pares_fatores"),exists("rotulos_fatores"))
library(dplyr);library(ggplot2);library(writexl)

# 1) RANKING POR FATOR: ordena as pessoas por mudança.
ranking <- pares_fatores %>%
  group_by(Grupo,Fator) %>% arrange(desc(Delta),.by_group=TRUE) %>%
  mutate(Posicao=row_number()) %>% ungroup() %>%
  transmute(Grupo,Fator,Posicao,Participante=Codigo,
            `Antes (pré)`=round(Pre,2),`Depois (pós)`=round(Pos,2),
            `Mudança (pós - pré)`=round(Delta,2))
print(ranking,n=Inf)

# 2) CONTAGEM POR FATOR: quantas pessoas aumentaram ou diminuíram?
# Empate = valores idênticos (sem limite arbitrário de melhora).
contagem <- pares_fatores %>%
  mutate(Resultado=case_when(Delta>0~"Aumento",Delta<0~"Redução",
                           TRUE~"Sem mudança")) %>%
  count(Grupo,Fator,Resultado,name="Participantes") %>%
  tidyr::complete(Grupo,Fator,Resultado=c("Aumento","Redução","Sem mudança"),
                  fill=list(Participantes=0)) %>%
  group_by(Grupo,Fator) %>%
  mutate(`Total no fator`=sum(Participantes),
         `Percentual (%)`=round(100*Participantes/`Total no fator`,1)) %>%
  ungroup()
print(contagem,n=Inf)


# =====================================================================
# 3) GRÁFICO — COMPARAÇÃO DAS MUDANÇAS ENTRE OS GRUPOS
# =====================================================================
#
# OBJETIVO:
# Comparar a porcentagem de participantes que apresentaram
# aumento, redução ou nenhuma mudança em cada fator.
#
# Cada fator terá duas barras:
# - Experimental
# - Controle
#
# Cada barra representa 100% dos participantes do grupo.
#
# VERDE: aumento no indicador
# VERMELHO: redução no indicador
# CINZA: nenhuma mudança
#
# Este gráfico é descritivo. Não indica significância estatística.
#
# Usa o objeto "contagem", criado na parte 2 deste script.
# Não é necessário executar os outros scripts novamente.

library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------------------------------------------------
# 3.1. PREPARAR OS DADOS
# ------------------------------------------------------------

dados_grafico <- contagem %>%
  mutate(
    # Definir a ordem das categorias na legenda.
    Resultado = factor(
      Resultado,
      levels = c("Aumento", "Redução", "Sem mudança")
    ),

    # Definir a ordem dos grupos.
    Grupo = factor(
      Grupo,
      levels = c("Experimental", "Controle")
    ),

    # Usar nomes completos dos fatores.
    Fator_Nome = factor(
      Fator,
      levels = names(rotulos_fatores),
      labels = unname(rotulos_fatores)
    )
  )

# ------------------------------------------------------------
# 3.2. CRIAR O GRÁFICO
# ------------------------------------------------------------

grafico_contagem <- ggplot(
  dados_grafico,
  aes(
    x = Grupo,
    y = `Percentual (%)`,
    fill = Resultado
  )
) +

  # Criar barras empilhadas.
  # A soma das três categorias corresponde a 100%.
  geom_col(
    width = 0.65,
    position = "stack"
  ) +

  # Mostrar somente a porcentagem dentro das barras.
  # Categorias com 0% não recebem rótulo.
  geom_text(
    aes(
      label = ifelse(
        `Percentual (%)` >= 10,
        paste0(round(`Percentual (%)`), "%"),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +

  # Um painel para cada fator.
  facet_wrap(
    ~ Fator_Nome,
    ncol = 2
  ) +

  # Cores para os três tipos de mudança.
  scale_fill_manual(
    values = c(
      "Aumento" = "#438C72",
      "Redução" = "#C16A65",
      "Sem mudança" = "#888888"
    ),
    drop = FALSE
  ) +

  # Eixo vertical de 0% a 100%.
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +

  # Títulos e descrições.
  labs(
    title = "Distribuição das mudanças por fator e grupo",
    subtitle = paste(
      "Comparação entre Experimental e Controle",
      "nos momentos pré e pós-experimento"
    ),
    x = "Grupo",
    y = "Participantes (%)",
    fill = "Tipo de mudança",
    caption = paste(
      "Aumento, redução e ausência de mudança referem-se",
      "aos indicadores fatoriais exploratórios.",
      "Gráfico descritivo, sem teste de significância."
    )
  ) +

  # Aparência do gráfico.
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(
      face = "bold",
      size = 10
    ),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10)
  )

# ------------------------------------------------------------
# 3.3. MOSTRAR O GRÁFICO NO RSTUDIO
# ------------------------------------------------------------

print(grafico_contagem)

# ------------------------------------------------------------
# 3.4. SALVAR A FIGURA PARA A DISSERTAÇÃO
# ------------------------------------------------------------

ggsave(
  filename = "FIGURA_10_Distribuicao_Mudancas_por_Grupo_e_Fator.png",
  plot = grafico_contagem,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

cat("\nGráfico de comparação entre grupos gerado.\n")

# 4) Arquivo para colocar no trabalho: abas identificadas pelo conteúdo.
write_xlsx(list(`Ranking por fator`=ranking,
                `Contagem e percentual`=contagem),
           "10_Tabelas_analises_individuais.xlsx")



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
