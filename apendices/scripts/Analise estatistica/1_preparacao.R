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
