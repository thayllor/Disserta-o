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