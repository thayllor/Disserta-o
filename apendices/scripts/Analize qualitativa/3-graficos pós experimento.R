# ============================================================
# SCRIPT Q3 — ANÁLISE DESCRITIVA DO PÓS-EXPERIMENTO
# ============================================================

# PRÉ-REQUISITOS:
# Executar Q1 e Q2 antes deste script.
#
# Gráficos:
# - Escalas numéricas: colunas ordenadas
# - Sim/Não e outras categorias: pizza ou barras
# - Número de sessões: colunas
# - Comparações entre grupos: colunas agrupadas
#
# N = número de participantes
# % = percentual de respostas válidas
#
# ATENÇÃO:
# Os resultados são descritivos.
# Não representam testes de significância estatística.


# ============================================================
# 1. BIBLIOTECAS E PASTA
# ============================================================

if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(writexl)

pasta <- "Graficos_Qualitativos/Q3"

dir.create(
  pasta,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 2. UTILIZAR OS OBJETOS DO Q1
# ============================================================

base_pos <- dados_experimento_pos
base_pre_pos <- dados_pre_pos
base_geral <- dados_gerais_pos


# ============================================================
# 3. IDENTIFICAR PERGUNTAS
# ============================================================

# Busca uma pergunta pelo texto.
# Interrompe caso não encontre exatamente uma coluna.

localizar_q3 <- function(base, busca) {
  
  resultado <- grep(
    busca,
    names(base),
    ignore.case = TRUE,
    value = TRUE
  )
  
  if (length(resultado) != 1) {
    stop(
      paste(
        "Erro ao identificar pergunta:",
        busca,
        "| Encontradas:",
        length(resultado)
      )
    )
  }
  
  resultado
}


# ============================================================
# 4. DEFINIR PERGUNTAS E TIPOS DE RESPOSTA
# ============================================================

# "escala": respostas numéricas de 0 a 5.
# "categorica": respostas como Sim/Não.
# "ordinal": categorias textuais com ordem.
# "sessoes": quantidade de sessões.
# "barras": categorias não ordenadas.

catalogo <- data.frame(
  
  Codigo = c(
    "Sessoes", "Horarios", "Diversao",
    "Tripulante", "Impostor", "Jogo",
    "Pedir", "Oferecer", "Conversa",
    "Feedback", "Conforto", "Pessoas",
    "Amizades", "Discussao_Part",
    "Discussao_Outros", "Assuntos",
    "Desenvolvimento", "Duracao_Total",
    "Duracao_Sessao"
  ),
  
  Busca = c(
    "Quantas seções do experimento",
    "dias e horários marcados",
    "se divertiu no período",
    "divertiu jogando de Tripulante",
    "divertiu jogando de Impostor",
    "Como você considera o jogo",
    "costumava pedir ajuda",
    "costumava oferecer ajuda",
    "costumava iniciar uma conversa",
    "costumava dar ou receber feedback",
    "mais confortável falando",
    "conheceu pessoas novas",
    "desenvolveu amizades",
    "discutiu.*outro participante",
    "discutiu.*outra pessoa",
    "assuntos diversos",
    "sentiu que desenvolveu suas",
    "tempo total de 2 meses",
    "sessões de 1h"
  ),
  
  Tipo = c(
    "sessoes", "categorica", "escala",
    "escala", "escala", "barras",
    "escala", "escala", "escala",
    "escala", "categorica", "categorica",
    "categorica", "categorica",
    "categorica", "categorica",
    "categorica", "ordinal", "ordinal"
  ),
  
  stringsAsFactors = FALSE
)

catalogo$Pergunta <- vapply(
  catalogo$Busca,
  function(x) localizar_q3(base_pos, x),
  character(1)
)


# ============================================================
# 5. CONTAR RESPOSTAS
# ============================================================

# N: frequência absoluta.
# Percentual: frequência relativa entre respostas válidas.

contar_q3 <- function(base, pergunta) {
  
  base %>%
    transmute(
      Resposta = trimws(
        as.character(.data[[pergunta]])
      )
    ) %>%
    filter(
      !is.na(Resposta),
      Resposta != ""
    ) %>%
    count(Resposta, name = "N") %>%
    mutate(
      Percentual = round(
        100 * N / sum(N), 1
      )
    )
}


# ============================================================
# 6. CRIAR GRÁFICOS
# ============================================================

criar_grafico_q3 <- function(tabela, pergunta, tipo) {
  
  if (nrow(tabela) == 0) {
    return(NULL)
  }
  
  titulo <- paste(
    strwrap(pergunta, width = 48),
    collapse = "\n"
  )
  
  total <- sum(tabela$N)
  
  # ----------------------------------------------------------
  # A. ESCALAS NUMÉRICAS DE 0 A 5
  # ----------------------------------------------------------
  
  if (tipo == "escala") {
    
    # Verificar se todas as respostas são de 0 a 5.
    if (!all(tabela$Resposta %in% as.character(0:5))) {
      stop(paste(
        "Resposta inesperada na escala:",
        pergunta
      ))
    }
    
    # Incluir categorias não observadas com N = 0.
    tabela <- tabela %>%
      mutate(
        Resposta = factor(
          Resposta,
          levels = as.character(0:5)
        )
      ) %>%
      complete(
        Resposta,
        fill = list(N = 0)
      ) %>%
      mutate(
        Percentual = round(
          100 * N / total, 1
        )
      )
    
    grafico <- ggplot(
      tabela,
      aes(x = Resposta, y = N)
    ) +
      geom_col(
        fill = "#4878A8",
        width = 0.65
      ) +
      geom_text(
        aes(label = ifelse(
          N > 0,
          paste0(N, " (", Percentual, "%)"),
          ""
        )),
        vjust = -0.4,
        size = 3.5
      ) +
      scale_y_continuous(
        breaks = seq(0, total, by = 1),
        expand = expansion(mult = c(0, 0.22))
      ) +
      labs(
        x = "Valor da escala",
        y = "Número de participantes",
        caption = paste(
          "Escala ordinal de 0 a 5.",
          "Os valores estão apresentados em ordem crescente.",
          "N = participantes; % = respostas válidas."
        )
      ) +
      theme_minimal(base_size = 11)
    
    # ----------------------------------------------------------
    # B. QUANTIDADE DE SESSÕES
    # ----------------------------------------------------------
    
  } else if (tipo == "sessoes") {
    
    tabela <- tabela %>%
      mutate(
        Numero = as.numeric(Resposta)
      ) %>%
      arrange(Numero)
    
    grafico <- ggplot(
      tabela,
      aes(x = Numero, y = N)
    ) +
      geom_col(
        fill = "#4878A8",
        width = 0.65
      ) +
      geom_text(
        aes(label = N),
        vjust = -0.4
      ) +
      scale_x_continuous(
        breaks = sort(unique(tabela$Numero))
      ) +
      scale_y_continuous(
        breaks = seq(0, total, by = 1),
        expand = expansion(mult = c(0, 0.2))
      ) +
      labs(
        x = "Quantidade de sessões",
        y = "Número de participantes",
        caption = "N = participantes por quantidade de sessões."
      ) +
      theme_minimal(base_size = 11)
    
    # ----------------------------------------------------------
    # C. AVALIAÇÕES ORDINAIS DE DURAÇÃO
    # ----------------------------------------------------------
    
  } else if (tipo == "ordinal") {
    
    # Ordem lógica das respostas.
    # As categorias não respondidas não são inventadas
    # como observações.
    
    ordem <- c(
      "Foi curta demais",
      "Foi curta",
      "Foi o tempo ideal",
      "Foi longa",
      "Foi longa demais"
    )
    
    tabela <- tabela %>%
      mutate(
        Resposta = factor(
          Resposta,
          levels = unique(c(
            ordem,
            as.character(Resposta)
          ))
        )
      )
    
    grafico <- ggplot(
      tabela,
      aes(x = Resposta, y = N)
    ) +
      geom_col(
        fill = "#4878A8",
        width = 0.65
      ) +
      geom_text(
        aes(label = paste0(
          N, " (", Percentual, "%)"
        )),
        vjust = -0.4,
        size = 3.5
      ) +
      scale_y_continuous(
        breaks = seq(0, total, by = 1),
        expand = expansion(mult = c(0, 0.2))
      ) +
      labs(
        x = "Avaliação da duração",
        y = "Número de participantes",
        caption = paste(
          "Categorias ordenadas da duração",
          "mais curta para a mais longa."
        )
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(
          angle = 15,
          hjust = 1
        )
      )
    
    # ----------------------------------------------------------
    # D. RESPOSTAS CATEGÓRICAS
    # ----------------------------------------------------------
    
  } else if (tipo == "categorica") {
    
    grafico <- ggplot(
      tabela,
      aes(x = "", y = N, fill = Resposta)
    ) +
      geom_col(
        width = 1,
        color = "white"
      ) +
      coord_polar(theta = "y") +
      geom_text(
        aes(label = paste0(
          N, " (", Percentual, "%)"
        )),
        position = position_stack(vjust = 0.5),
        size = 3.5
      ) +
      labs(
        fill = "Resposta",
        caption = "Cada fatia representa a proporção de respostas válidas."
      ) +
      theme_void(base_size = 11) +
      theme(
        legend.position = "bottom"
      )
    
    # ----------------------------------------------------------
    # E. CATEGORIAS NÃO ORDENADAS
    # ----------------------------------------------------------
    
  } else {
    
    grafico <- ggplot(
      tabela,
      aes(
        x = reorder(Resposta, N),
        y = N
      )
    ) +
      geom_col(
        fill = "#4878A8",
        width = 0.65
      ) +
      geom_text(
        aes(label = paste0(
          N, " (", Percentual, "%)"
        )),
        hjust = -0.2
      ) +
      coord_flip() +
      scale_y_continuous(
        breaks = seq(0, total, by = 1),
        expand = expansion(mult = c(0, 0.3))
      ) +
      labs(
        x = NULL,
        y = "Número de participantes",
        caption = "As categorias não representam uma escala ordenada."
      ) +
      theme_minimal(base_size = 11)
  }
  
  # ----------------------------------------------------------
  # F. FORMATAÇÃO COMUM
  # ----------------------------------------------------------
  
  grafico +
    labs(
      title = titulo,
      subtitle = if (tipo == "escala") {
        paste0("Escala de 0 a 5 | Respostas válidas: n = ", total)
      } else {
        paste0("Respostas válidas: n = ", total)
      }
    ) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 10
      ),
      plot.subtitle = element_text(size = 9),
      plot.caption = element_text(
        size = 8,
        hjust = 0
      ),
      plot.caption.position = "plot"
    )
}


# ============================================================
# 7. GERAR GRÁFICOS INDIVIDUAIS
# ============================================================

graficos <- list()
tabelas <- list()

for (i in seq_len(nrow(catalogo))) {
  
  codigo <- catalogo$Codigo[i]
  pergunta <- catalogo$Pergunta[i]
  tipo <- catalogo$Tipo[i]
  
  tabela <- contar_q3(
    base_pos,
    pergunta
  )
  
  tabelas[[codigo]] <- tabela %>%
    mutate(
      Codigo = codigo,
      Pergunta = pergunta,
      .before = 1
    )
  
  graficos[[codigo]] <- criar_grafico_q3(
    tabela,
    pergunta,
    tipo
  )
}


# ============================================================
# 8. AGRUPAR AS FIGURAS
# ============================================================

agrupamentos <- list(
  
  "01_Participacao" = c(
    "Sessoes", "Horarios"
  ),
  
  "02_Diversao_Geral" = "Diversao",
  
  "03_Diversao_Papeis" = c(
    "Tripulante", "Impostor"
  ),
  
  "04_Avaliacao_Jogo" = "Jogo",
  
  "05_Ajuda" = c(
    "Pedir", "Oferecer"
  ),
  
  "06_Comunicacao_Feedback" = c(
    "Conversa", "Feedback"
  ),
  
  # Socialização: conforto, novas pessoas e amizades
  "08_Socializacao" = c(
    "Conforto", "Pessoas", "Amizades"
  ),
  
  # Discussões sobre o experimento
  "09_Discussoes" = c(
    "Discussao_Part", "Discussao_Outros"
  ),
  
  "11_Conversas_Diversas" = "Assuntos",
  
  "12_Desenvolvimento_Percebido" = "Desenvolvimento",
  
  "14_Duracao" = c(
    "Duracao_Total", "Duracao_Sessao"
  )
)

figuras <- list()

for (nome in names(agrupamentos)) {
  
  selecionados <- graficos[
    agrupamentos[[nome]]
  ]
  
  selecionados <- Filter(
    Negate(is.null),
    selecionados
  )
  
  if (length(selecionados) == 0) {
    next
  }
  
  figura <- patchwork::wrap_plots(
    selecionados,
    ncol = length(selecionados)
  )
  
  figuras[[nome]] <- figura
  
  print(figura)
  
  ggsave(
    file.path(
      pasta,
      paste0(nome, ".png")
    ),
    plot = figura,
    width = if (length(selecionados) == 3) 20 else
      if (length(selecionados) == 2) 16 else 9,
    height = 7,
    dpi = 300
  )
}


# ============================================================
# 9. COMPARAÇÃO PRÉ × PÓS DA CRENÇA
# ============================================================

coluna_crenca <- localizar_q3(
  base_pre_pos,
  "acredita que jogos multiplayer"
)

crenca_long <- base_pre_pos %>%
  mutate(
    Momento = case_when(
      questionario == "Pre-experimento" ~ "Pré",
      questionario %in% c(
        "Pos-experimento",
        "Pós-experimento"
      ) ~ "Pós",
      TRUE ~ NA_character_
    ),
    Resposta = trimws(
      as.character(.data[[coluna_crenca]])
    )
  ) %>%
  filter(
    !is.na(Momento),
    !is.na(Nomes),
    Nomes != "",
    !is.na(Resposta),
    Resposta != ""
  ) %>%
  select(Nomes, Momento, Resposta)

# Verificar duplicatas antes de parear.
if (anyDuplicated(
  crenca_long[c("Nomes", "Momento")]
)) {
  stop("Existem respostas duplicadas na comparação Pré/Pós.")
}

crenca_pares <- crenca_long %>%
  pivot_wider(
    names_from = Momento,
    values_from = Resposta
  ) %>%
  filter(
    !is.na(`Pré`),
    !is.na(`Pós`)
  )

tabela_crenca <- crenca_pares %>%
  pivot_longer(
    cols = c(`Pré`, `Pós`),
    names_to = "Momento",
    values_to = "Resposta"
  ) %>%
  count(Momento, Resposta, name = "N") %>%
  group_by(Momento) %>%
  mutate(
    Percentual = round(
      100 * N / sum(N), 1
    )
  ) %>%
  ungroup()

grafico_crenca <- ggplot(
  tabela_crenca,
  aes(
    x = Resposta,
    y = N,
    fill = Momento
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(label = N),
    position = position_dodge(width = 0.8),
    vjust = -0.4
  ) +
  scale_y_continuous(
    breaks = seq(0, nrow(crenca_pares), by = 1),
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    title = paste(
      strwrap(coluna_crenca, width = 65),
      collapse = "\n"
    ),
    subtitle = paste0(
      "Participantes com respostas pareadas: n = ",
      nrow(crenca_pares)
    ),
    x = "Resposta",
    y = "Número de participantes",
    fill = "Momento",
    caption = paste(
      "Comparação descritiva das respostas dos",
      "mesmos participantes antes e depois do experimento."
    )
  ) +
  theme_minimal(base_size = 12)

figuras[["13_Crenca_Pre_Pos"]] <- grafico_crenca

print(grafico_crenca)

ggsave(
  file.path(pasta, "13_Crenca_Pre_Pos.png"),
  plot = grafico_crenca,
  width = 11,
  height = 7,
  dpi = 300
)


# ============================================================
# 10. COMPARAÇÃO EXPERIMENTAL × CONTROLE
# ============================================================

coluna_eventos <- localizar_q3(
  base_geral,
  "participou de algum evento ou situação"
)

coluna_desenvolvimento <- localizar_q3(
  base_geral,
  "desenvolveu suas habilidade"
)

comparacao_long <- base_geral %>%
  select(
    Grupo,
    all_of(c(
      coluna_eventos,
      coluna_desenvolvimento
    ))
  ) %>%
  pivot_longer(
    cols = -Grupo,
    names_to = "Pergunta",
    values_to = "Resposta",
    values_transform = list(
      Resposta = as.character
    )
  ) %>%
  mutate(
    Resposta = trimws(Resposta),
    Resposta = case_when(
      tolower(Resposta) == "sim" ~ "Sim",
      tolower(Resposta) %in% c("não", "nao") ~ "Não",
      TRUE ~ Resposta
    )
  ) %>%
  filter(
    !is.na(Resposta),
    Resposta != ""
  )

# Não transformar respostas abertas automaticamente.
respostas_revisar <- comparacao_long %>%
  filter(!Resposta %in% c("Sim", "Não"))

comparacao_binaria <- comparacao_long %>%
  filter(Resposta %in% c("Sim", "Não"))

tabela_comparacao <- comparacao_binaria %>%
  count(
    Pergunta,
    Grupo,
    Resposta,
    name = "N"
  ) %>%
  group_by(Pergunta, Grupo) %>%
  mutate(
    N_Validos = sum(N),
    Percentual = round(
      100 * N / N_Validos, 1
    )
  ) %>%
  ungroup()

criar_comparativo <- function(pergunta) {
  
  tabela <- tabela_comparacao %>%
    filter(Pergunta == pergunta) %>%
    mutate(
      Grupo = factor(
        Grupo,
        levels = c("Controle", "Experimento")
      ),
      Resposta = factor(
        Resposta,
        levels = c("Não", "Sim")
      )
    )
  
  ggplot(
    tabela,
    aes(
      x = Grupo,
      y = Percentual,
      fill = Resposta
    )
  ) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7
    ) +
    geom_text(
      aes(
        label = paste0(
          N, " (", Percentual, "%)"
        )
      ),
      position = position_dodge(width = 0.8),
      vjust = -0.4,
      size = 3.2
    ) +
    scale_fill_manual(
      values = c(
        "Não" = "#F8766D",
        "Sim" = "#00BFC4"
      )
    ) +
    scale_y_continuous(
      limits = c(0, 115),
      breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste(
        strwrap(pergunta, width = 43),
        collapse = "\n"
      ),
      x = "Grupo",
      y = "Percentual de participantes",
      fill = "Resposta",
      caption = paste(
        "Percentuais calculados separadamente",
        "dentro de cada grupo."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 10
      ),
      legend.position = "bottom"
    )
}

grafico_eventos <- criar_comparativo(
  coluna_eventos
)

grafico_desenvolvimento <- criar_comparativo(
  coluna_desenvolvimento
)

figura_grupos <- grafico_eventos +
  grafico_desenvolvimento

figuras[["15_Comparacao_Grupos"]] <- figura_grupos

print(figura_grupos)

ggsave(
  file.path(pasta, "15_Comparacao_Grupos.png"),
  plot = figura_grupos,
  width = 16,
  height = 7,
  dpi = 300
)


# ============================================================
# 11. EXPORTAR TABELAS
# ============================================================

tabela_frequencias <- bind_rows(
  tabelas,
  .id = "Identificador"
)

writexl::write_xlsx(
  list(
    Frequencias_Pos = tabela_frequencias,
    Crenca_Pre_Pos = tabela_crenca,
    Comparacao_Grupos = tabela_comparacao,
    Respostas_Revisar = respostas_revisar,
    Catalogo = catalogo
  ),
  path = file.path(
    pasta,
    "Tabelas_Q3.xlsx"
  )
)


# ============================================================
# 12. RESULTADO FINAL
# ============================================================

cat("\n===== Q3 CONCLUÍDO =====\n")

cat(
  "Quantidade de figuras:",
  length(figuras),
  "\n"
)

print(names(figuras))

cat("\nPasta de saída:\n")
cat(normalizePath(pasta), "\n")