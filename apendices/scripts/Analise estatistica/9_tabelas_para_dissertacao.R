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
