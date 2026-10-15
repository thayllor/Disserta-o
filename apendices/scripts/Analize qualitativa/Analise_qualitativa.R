# ============================================================
# SCRIPT Q1 — PREPARAÇÃO DOS DADOS QUALITATIVOS
# ============================================================

# 1. INSTALAR E CARREGAR PACOTES
# readxl: leitura do Excel
# dplyr: organização e filtragem
# tidyr: tratamento de dados
# ggplot2: gráficos futuros
# writexl: exportação de resultados

pacotes <- c("readxl", "dplyr", "tidyr", "ggplot2", "writexl")

for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

invisible(lapply(pacotes, library, character.only = TRUE))

# ============================================================
# 2. IMPORTAR AS DUAS ABAS — CORRIGIDO
# ============================================================

arquivo <- file.choose()

# Primeira aba: cabeçalhos na linha 4
dados_gerais <- read_excel(
  arquivo,
  sheet = "Comparação controle",
  skip = 3
)

# Segunda aba: cabeçalhos na linha 2
dados_qualitativos <- read_excel(
  arquivo,
  sheet = "Comparação pre pos qualitativa",
  skip = 1
)

# Remover espaços extras dos cabeçalhos
names(dados_gerais) <- trimws(names(dados_gerais))
names(dados_qualitativos) <- trimws(names(dados_qualitativos))

# Conferir as colunas
print(names(dados_gerais)[1:10])
print(names(dados_qualitativos)[1:10])

# ============================================================
# 3. PADRONIZAR IDENTIFICAÇÃO
# ============================================================

# Remover espaços extras dos nomes das colunas.
names(dados_gerais) <- trimws(names(dados_gerais))
names(dados_qualitativos) <- trimws(names(dados_qualitativos))

# Padronizar textos usados para identificar participantes,
# grupos e momentos de aplicação.

padronizar <- function(base) {
  base %>%
    mutate(
      Nomes = trimws(as.character(Nomes)),
      Grupo = trimws(as.character(Grupo)),
      questionario = trimws(as.character(questionario))
    )
}

dados_gerais <- padronizar(dados_gerais)
dados_qualitativos <- padronizar(dados_qualitativos)


# ============================================================
# 4. IDENTIFICAR AS PERGUNTAS
# ============================================================

# Primeira aba: duas perguntas gerais após o IHS.
perguntas_gerais <- names(dados_gerais)[
  grepl(
    "participou de algum evento|desenvolveu suas habilidade",
    names(dados_gerais),
    ignore.case = TRUE
  )
]

# Segunda aba: idade, sexo e escolaridade.
perguntas_demograficas <- c(
  "Idade", "Sexo", "Escolaridade"
)

# Pergunta sobre a influência dos jogos multiplayer.
pergunta_crenca <- names(dados_qualitativos)[
  grepl(
    "Você acredita que jogos multiplayer",
    names(dados_qualitativos),
    ignore.case = TRUE
  )
]

# Perguntas de perfil aplicadas somente no Pré.
perguntas_perfil <- names(dados_qualitativos)[
  grepl(
    "pessoa tímida|joga vídeo games|com que frequência|já jogou o Among",
    names(dados_qualitativos),
    ignore.case = TRUE
  )
]

# Perguntas exclusivas do Pós experimental.
# Exclui identificadores, demografia, crença e perfil.
perguntas_experimento <- setdiff(
  names(dados_qualitativos),
  c(
    "ID", "nomes", "Grupo", "questionario",
    perguntas_demograficas,
    pergunta_crenca,
    perguntas_perfil
  )
)


# ============================================================
# 5. SEPARAR PARTICIPANTES POR GRUPO E MOMENTO
# ============================================================

# Segunda aba: grupo experimental.

qual_pre <- dados_qualitativos %>%
  filter(
    Grupo == "Experimento",
    questionario == "Pre-experimento"
  )

qual_pos <- dados_qualitativos %>%
  filter(
    Grupo == "Experimento",
    questionario != "Pre-experimento"
  )

# Primeira aba: ambos os grupos, somente Pós.

geral_pos <- dados_gerais %>%
  filter(
    Grupo %in% c("Experimento", "Controle"),
    questionario != "Pre-experimento"
  )


# ============================================================
# 6. CRIAR OS CINCO CONJUNTOS DE DADOS
# ============================================================

# A — Demografia e crença, Pré e Pós.
dados_pre_pos <- dados_qualitativos %>%
  filter(Grupo == "Experimento") %>%
  select(
    Nomes, Grupo, questionario,
    all_of(perguntas_demograficas),
    all_of(pergunta_crenca)
  )

# B — Perfil e hábitos, somente Pré.
dados_perfil_pre <- qual_pre %>%
  select(
    Nomes,
    all_of(perguntas_perfil)
  )

# C — Experiência durante as sessões, somente Pós.
dados_experimento_pos <- qual_pos %>%
  select(
    Nomes,
    all_of(perguntas_experimento)
  )

# D — Perguntas gerais, Experimental e Controle, Pós.
dados_gerais_pos <- geral_pos %>%
  select(
    Nomes, Grupo,
    all_of(perguntas_gerais)
  )

# E — Respostas abertas.
# Identificadas pelo conteúdo das perguntas.
perguntas_abertas <- unique(c(
  perguntas_gerais[
    grepl("evento ou situação", perguntas_gerais,
          ignore.case = TRUE)
  ],
  perguntas_experimento[
    grepl(
      "descreva como|evento ou situação|espaço é livre",
      perguntas_experimento,
      ignore.case = TRUE
    )
  ]
))

# Organizar respostas abertas de ambas as abas.
dados_qualitativos_abertos <- bind_rows(
  dados_gerais_pos %>%
    select(
      Nomes, Grupo,
      any_of(perguntas_abertas)
    ) %>%
    pivot_longer(
      cols = -c(Nomes, Grupo),
      names_to = "Pergunta",
      values_to = "Resposta"
    ),

  dados_experimento_pos %>%
    mutate(Grupo = "Experimento") %>%
    select(
      Nomes, Grupo,
      any_of(perguntas_abertas)
    ) %>%
    pivot_longer(
      cols = -c(Nomes, Grupo),
      names_to = "Pergunta",
      values_to = "Resposta"
    )
) %>%
  filter(
    !is.na(Resposta),
    trimws(as.character(Resposta)) != ""
  )


# ============================================================
# 7. CONTAR RESPOSTAS VÁLIDAS POR PERGUNTA
# ============================================================

# N_Validos: quantidade de respostas preenchidas.
# N_Ausentes: quantidade de respostas vazias ou NA.
# Percentual: proporção de respostas válidas na base.

contar_respostas <- function(base, perguntas) {

  base %>%
    select(all_of(perguntas)) %>%
    summarise(
      across(
        everything(),
        ~ sum(!is.na(.x) &
                trimws(as.character(.x)) != "")
      )
    ) %>%
    pivot_longer(
      everything(),
      names_to = "Pergunta",
      values_to = "N_Validos"
    ) %>%
    mutate(
      N_Ausentes = nrow(base) - N_Validos,
      Percentual = if (nrow(base) > 0) {
        round(100 * N_Validos / nrow(base), 1)
      } else {
        NA_real_
      }
    )
}

# Contagem separada por momento e conjunto.

respostas_demografia_pre <- contar_respostas(
  qual_pre, perguntas_demograficas
)

respostas_demografia_pos <- contar_respostas(
  qual_pos, perguntas_demograficas
)

respostas_crenca_pre <- contar_respostas(
  qual_pre, pergunta_crenca
)

respostas_crenca_pos <- contar_respostas(
  qual_pos, pergunta_crenca
)

respostas_perfil_pre <- contar_respostas(
  dados_perfil_pre, perguntas_perfil
)

respostas_experimento_pos <- contar_respostas(
  dados_experimento_pos, perguntas_experimento
)

# Para as duas perguntas gerais, contar por grupo.
respostas_gerais_pos <- dados_gerais_pos %>%
  group_by(Grupo) %>%
  group_modify(
    ~ contar_respostas(.x, perguntas_gerais)
  ) %>%
  ungroup()


# ============================================================
# 8. CONFERIR A PREPARAÇÃO
# ============================================================

cat("\n===== PARTICIPANTES =====\n")

cat("Experimental Pré:", nrow(qual_pre), "\n")
cat("Experimental Pós:", nrow(qual_pos), "\n")

cat("\nPerguntas gerais por grupo:\n")
print(table(dados_gerais_pos$Grupo))

cat("\n===== QUANTIDADE DE PERGUNTAS =====\n")

cat("Demográficas:", length(perguntas_demograficas), "\n")
cat("Crença Pré/Pós:", length(pergunta_crenca), "\n")
cat("Perfil Pré:", length(perguntas_perfil), "\n")
cat("Experimento Pós:", length(perguntas_experimento), "\n")
cat("Gerais Pós:", length(perguntas_gerais), "\n")

cat("\n===== RESPOSTAS VÁLIDAS =====\n")

print(respostas_crenca_pre)
print(respostas_crenca_pos)
print(respostas_perfil_pre)
print(respostas_experimento_pos)
print(respostas_gerais_pos)

cat("\nPreparação concluída!\n")



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
