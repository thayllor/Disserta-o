
# ============================================================
# SCRIPT Q2 — PERFIL DOS PARTICIPANTES
# ============================================================
#
# PRÉ-REQUISITO:
# Executar o Script Q1 antes deste script.
#
# OBJETIVO:
# Caracterizar os participantes do grupo experimental
# antes da realização das sessões.
#
# GRÁFICOS INDIVIDUAIS:
# 1. Idade
# 2. Sexo
# 3. Escolaridade
# 4. Timidez declarada
# 5. Experiência anterior com Among Us
#
# FIGURAS AGRUPADAS:
# 6. Perfil demográfico (idade, sexo, escolaridade)
# 7. Perfil e experiência anterior (timidez, Among Us)
#
# N = número de participantes com resposta válida.
#
# As análises são exclusivamente descritivas.
# ============================================================


# ============================================================
# 1. INSTALAR E CARREGAR PACOTES
# ============================================================

pacotes <- c(
  "dplyr",
  "ggplot2",
  "patchwork"
)

for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

invisible(lapply(
  pacotes,
  library,
  character.only = TRUE
))


# ============================================================
# 2. PREPARAR O AMBIENTE
# ============================================================

# Verificar se Q1 foi executado.
stopifnot(
  exists("dados_pre_pos"),
  exists("dados_perfil_pre")
)

# Pasta para salvar os resultados.
pasta_q2 <- "Graficos_Qualitativos"

dir.create(
  pasta_q2,
  recursive = TRUE,
  showWarnings = FALSE
)

# Selecionar somente respostas pré-experimento.
# Isso evita contar a mesma pessoa duas vezes.

base_pre <- dados_pre_pos %>%
  filter(questionario == "Pre-experimento")

# Conferir quantidade de participantes.
cat(
  "\nParticipantes no pré-experimento:",
  nrow(base_pre),
  "\n"
)

# Verificar possíveis duplicatas.
if (anyDuplicated(base_pre$Nomes)) {
  stop(
    "Existem participantes duplicados no pré-experimento."
  )
}


# ============================================================
# 3. FUNÇÃO PARA IDENTIFICAR PERGUNTAS
# ============================================================
#
# Localiza uma coluna pelo texto da pergunta.
# Interrompe se nenhuma ou várias forem encontradas.

localizar_q2 <- function(base, busca) {
  
  resultado <- grep(
    busca,
    names(base),
    ignore.case = TRUE,
    value = TRUE
  )
  
  if (length(resultado) != 1) {
    stop(
      paste(
        "Erro ao localizar:",
        busca,
        "| Colunas encontradas:",
        length(resultado)
      )
    )
  }
  
  resultado
}


# ============================================================
# 4. FUNÇÃO PARA CONTAR RESPOSTAS
# ============================================================
#
# N representa a frequência absoluta.
# Respostas ausentes não entram na contagem.

contar <- function(base, coluna) {
  
  base %>%
    mutate(
      Resposta = trimws(
        as.character(.data[[coluna]])
      )
    ) %>%
    filter(
      !is.na(Resposta),
      Resposta != ""
    ) %>%
    count(
      Resposta,
      name = "N"
    )
}


# ============================================================
# 5. FUNÇÃO PARA CRIAR GRÁFICOS CATEGÓRICOS
# ============================================================
#
# Utilizada para:
# - Sexo
# - Escolaridade
# - Timidez
# - Experiência com Among Us
#
# Cada barra representa uma categoria de resposta.

criar_grafico <- function(tabela, titulo) {
  
  if (nrow(tabela) == 0) {
    stop(
      paste("Não existem respostas válidas:", titulo)
    )
  }
  
  ggplot(
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
      aes(label = N),
      hjust = -0.2,
      size = 4
    ) +
    
    coord_flip() +
    
    scale_y_continuous(
      breaks = function(x) {
        seq(0, ceiling(max(x)), by = 1)
      },
      expand = expansion(
        mult = c(0, 0.25)
      )
    ) +
    
    labs(
      title = titulo,
      subtitle = paste0(
        "Respostas válidas: n = ",
        sum(tabela$N)
      ),
      x = NULL,
      y = "Número de participantes"
    ) +
    
    theme_minimal(base_size = 12) +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 12
      ),
      plot.subtitle = element_text(size = 10),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}


# ============================================================
# 6. GRÁFICO 1 — IDADE
# ============================================================
#
# Mostra quantos participantes possuem cada idade.
#
# A idade é tratada como valor numérico.
# Respostas ausentes ou inválidas são excluídas.

tabela_idade <- base_pre %>%
  mutate(
    Idade = suppressWarnings(
      as.numeric(as.character(Idade))
    )
  ) %>%
  filter(
    !is.na(Idade),
    is.finite(Idade),
    Idade >= 0
  ) %>%
  count(
    Idade,
    name = "N"
  ) %>%
  arrange(Idade)

if (nrow(tabela_idade) == 0) {
  stop("Nenhuma idade válida foi encontrada.")
}

grafico_idade <- ggplot(
  tabela_idade,
  aes(
    x = factor(Idade),
    y = N
  )
) +
  
  geom_col(
    fill = "#4878A8",
    width = 0.65
  ) +
  
  geom_text(
    aes(label = N),
    vjust = -0.4,
    size = 4
  ) +
  
  scale_y_continuous(
    breaks = function(x) {
      seq(0, ceiling(max(x)), by = 1)
    },
    expand = expansion(
      mult = c(0, 0.25)
    )
  ) +
  
  labs(
    title = "Distribuição das idades",
    subtitle = paste0(
      "Respostas válidas: n = ",
      sum(tabela_idade$N)
    ),
    x = "Idade (anos)",
    y = "Número de participantes"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank()
  )

print(grafico_idade)

ggsave(
  file.path(pasta_q2, "01_idade.png"),
  plot = grafico_idade,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 7. GRÁFICO 2 — SEXO
# ============================================================

tabela_sexo <- contar(
  base_pre,
  "Sexo"
)

grafico_sexo <- criar_grafico(
  tabela_sexo,
  "Distribuição por sexo"
)

print(grafico_sexo)

ggsave(
  file.path(pasta_q2, "02_sexo.png"),
  plot = grafico_sexo,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 8. GRÁFICO 3 — ESCOLARIDADE
# ============================================================

tabela_escolaridade <- contar(
  base_pre,
  "Escolaridade"
)

grafico_escolaridade <- criar_grafico(
  tabela_escolaridade,
  "Escolaridade dos participantes"
)

print(grafico_escolaridade)

ggsave(
  file.path(pasta_q2, "03_escolaridade.png"),
  plot = grafico_escolaridade,
  width = 9,
  height = 5,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 9. GRÁFICO 4 — TIMIDEZ DECLARADA
# ============================================================
#
# Pergunta respondida antes do experimento.
# Representa a autopercepção dos participantes.
# Não constitui avaliação clínica de timidez.

coluna_timidez <- localizar_q2(
  dados_perfil_pre,
  "pessoa tímida"
)

tabela_timidez <- contar(
  dados_perfil_pre,
  coluna_timidez
)

grafico_timidez <- criar_grafico(
  tabela_timidez,
  "Timidez declarada antes do experimento"
)

print(grafico_timidez)

ggsave(
  file.path(pasta_q2, "04_timidez.png"),
  plot = grafico_timidez,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 10. GRÁFICO 5 — EXPERIÊNCIA COM AMONG US
# ============================================================
#
# Mostra se os participantes já haviam jogado
# Among Us antes das sessões experimentais.

coluna_among <- localizar_q2(
  dados_perfil_pre,
  "já jogou o Among"
)

tabela_among <- contar(
  dados_perfil_pre,
  coluna_among
)

grafico_among <- criar_grafico(
  tabela_among,
  "Experiência anterior com Among Us"
)

print(grafico_among)

ggsave(
  file.path(pasta_q2, "05_among_us.png"),
  plot = grafico_among,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 11. FIGURA AGRUPADA — PERFIL DEMOGRÁFICO
# ============================================================
#
# Reúne três gráficos:
#
#        IDADE
#    SEXO | ESCOLARIDADE
#
# A figura pode ser utilizada na caracterização
# dos participantes do artigo.

figura_demografia <- (
  grafico_idade /
    (grafico_sexo | grafico_escolaridade)
) +
  plot_annotation(
    title = "Perfil demográfico dos participantes",
    subtitle = "Grupo experimental — pré-experimento"
  )

print(figura_demografia)

ggsave(
  file.path(
    pasta_q2,
    "06_perfil_demografico.png"
  ),
  plot = figura_demografia,
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 12. FIGURA AGRUPADA — PERFIL E EXPERIÊNCIA
# ============================================================
#
# Reúne:
# - Timidez declarada
# - Experiência anterior com Among Us
#
# Os dois gráficos ficam lado a lado.

figura_perfil <- (
  grafico_timidez | grafico_among
) +
  plot_annotation(
    title = "Perfil e experiência anterior dos participantes",
    subtitle = "Grupo experimental — pré-experimento"
  )

print(figura_perfil)

ggsave(
  file.path(
    pasta_q2,
    "07_perfil_experiencia.png"
  ),
  plot = figura_perfil,
  width = 14,
  height = 6,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 13. TABELAS RESUMIDAS
# ============================================================
#
# Mantém as frequências utilizadas nos gráficos.
# Úteis para consultar valores na redação do artigo.

tabelas_q2 <- list(
  Idade = tabela_idade,
  Sexo = tabela_sexo,
  Escolaridade = tabela_escolaridade,
  Timidez = tabela_timidez,
  Among_Us = tabela_among
)

cat("\n===== TABELAS DESCRITIVAS =====\n")

for (nome in names(tabelas_q2)) {
  
  cat("\n", nome, "\n")
  
  print(tabelas_q2[[nome]])
}


# ============================================================
# 14. CONFERIR RESULTADOS
# ============================================================

cat("\n===== SCRIPT Q2 CONCLUÍDO =====\n")

cat(
  "Participantes do grupo experimental:",
  nrow(base_pre),
  "\n"
)

cat("\nFiguras agrupadas:\n")
cat("06_perfil_demografico.png\n")
cat("07_perfil_experiencia.png\n")

cat("\nTodos os arquivos gerados:\n")

print(
  list.files(
    pasta_q2,
    pattern = "\\.png$"
  )
)

cat("\nPasta de saída:\n")
cat(normalizePath(pasta_q2), "\n")

# ============================================================
# 15. PERFIL DEMOGRÁFICO — GRUPO CONTROLE
# ============================================================
#
# OBJETIVO:
# Criar uma figura demográfica para o grupo controle,
# utilizando o mesmo estilo do grupo experimental.
#
# GRÁFICOS:
# 1. Distribuição das idades
# 2. Distribuição por sexo
# 3. Escolaridade
#
# FIGURA AGRUPADA:
#       IDADE
#   SEXO | ESCOLARIDADE
#
# Execute Q1 e Q2 antes desta seção.
# ============================================================

stopifnot(
  exists("dados_gerais"),
  exists("criar_grafico"),
  exists("pasta_q2")
)

library(dplyr)
library(ggplot2)
library(patchwork)

# ------------------------------------------------------------
# 15.1. SELECIONAR PARTICIPANTES DO CONTROLE
# ------------------------------------------------------------

# Usar apenas o pré-experimento para não contar
# participantes repetidos.

base_controle <- dados_gerais %>%
  filter(
    Grupo == "Controle",
    questionario == "Pre-experimento"
  ) %>%
  distinct(Nomes, .keep_all = TRUE)

cat(
  "\nParticipantes do grupo controle:",
  nrow(base_controle),
  "\n"
)

if (nrow(base_controle) == 0) {
  stop(
    "Nenhum participante do controle foi encontrado."
  )
}

# ------------------------------------------------------------
# 15.2. GRÁFICO DE IDADE
# ------------------------------------------------------------

tabela_idade_controle <- base_controle %>%
  mutate(
    Idade = suppressWarnings(
      as.numeric(as.character(Idade))
    )
  ) %>%
  filter(
    !is.na(Idade),
    is.finite(Idade),
    Idade >= 0
  ) %>%
  count(Idade, name = "N") %>%
  arrange(Idade)

grafico_idade_controle <- ggplot(
  tabela_idade_controle,
  aes(
    x = factor(Idade),
    y = N
  )
) +
  geom_col(
    fill = "#4878A8",
    width = 0.65
  ) +
  geom_text(
    aes(label = N),
    vjust = -0.4,
    size = 4
  ) +
  scale_y_continuous(
    breaks = function(x) {
      seq(0, ceiling(max(x)), by = 1)
    },
    expand = expansion(
      mult = c(0, 0.25)
    )
  ) +
  labs(
    title = "Distribuição das idades",
    subtitle = paste0(
      "Respostas válidas: n = ",
      sum(tabela_idade_controle$N)
    ),
    x = "Idade (anos)",
    y = "Número de participantes"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ------------------------------------------------------------
# 15.3. GRÁFICO DE SEXO
# ------------------------------------------------------------

tabela_sexo_controle <- contar(
  base_controle,
  "Sexo"
)

grafico_sexo_controle <- criar_grafico(
  tabela_sexo_controle,
  "Distribuição por sexo"
)

# ------------------------------------------------------------
# 15.4. GRÁFICO DE ESCOLARIDADE
# ------------------------------------------------------------

tabela_escolaridade_controle <- contar(
  base_controle,
  "Escolaridade"
)

grafico_escolaridade_controle <- criar_grafico(
  tabela_escolaridade_controle,
  "Escolaridade dos participantes"
)

# ------------------------------------------------------------
# 15.5. AGRUPAR OS TRÊS GRÁFICOS
# ------------------------------------------------------------

figura_demografia_controle <- (
  grafico_idade_controle /
  (
    grafico_sexo_controle |
    grafico_escolaridade_controle
  )
) +
  plot_annotation(
    title = "Perfil demográfico dos participantes",
    subtitle = "Grupo controle — pré-experimento"
  )

print(figura_demografia_controle)

# ------------------------------------------------------------
# 15.6. SALVAR FIGURA
# ------------------------------------------------------------

ggsave(
  file.path(
    pasta_q2,
    "08_perfil_demografico_controle.png"
  ),
  plot = figura_demografia_controle,
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------
# 15.7. CONFERIR RESULTADOS
# ------------------------------------------------------------

cat("\n===== PERFIL DEMOGRÁFICO CONTROLE =====\n")

cat("\nIdades:\n")
print(tabela_idade_controle)

cat("\nSexo:\n")
print(tabela_sexo_controle)

cat("\nEscolaridade:\n")
print(tabela_escolaridade_controle)

cat("\nFigura salva:\n")
cat("08_perfil_demografico_controle.png\n")

