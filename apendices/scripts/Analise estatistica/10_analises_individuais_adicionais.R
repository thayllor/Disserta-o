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
