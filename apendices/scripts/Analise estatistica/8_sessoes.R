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
