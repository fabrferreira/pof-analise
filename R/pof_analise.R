################################################################################
################################################################################
################################# Pesquisa de Orçamentos Familiares ############
################################################################################
################################################################################

# Importando pacotes ------------------------------------------------------

library(tidyverse)
library(janitor)
library(survey)
library(srvyr)
library(gtsummary)

# Importando dados --------------------------------------------------------

## Morador

quest_morador <- readr::read_fwf(
  file = "data-raw/MORADOR.txt",
  readr::fwf_widths(
    widths = c(2, 4, 1, 9, 2, 1, 2, 2, 1, 2, 2, 4, 3, 1, 1,
               1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,
               1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1,
               2, 1, 2, 14, 14, 10, 1, 1, 20, 20, 20, 20),
    col_names = c(
      "uf", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA",
      "NUM_DOM", "NUM_UC", "COD_INFORMANTE", "V0306", "V0401",
      "V04021", "V04022", "V04023", "V0403", "V0404", "V0405", 
      "V0406", "V0407","V0408", "V0409", "V0410", "V0411", "V0412",
      "V0413", "V0414", "V0415", "V0416", "V041711", "V041712",
      "V041721", "V041722", "V041731", "V041732", "V041741", "V041742",
      "V0418", "V0419", "V0420", "V0421", "V0422", "V0423", "V0424",
      "V0425", "V0426", "V0427", "V0428", "V0429", "V0430", "ANOS_ESTUDO",
      "PESO", "PESO_FINAL", "RENDA_TOTAL", "INSTRUCAO", "COMPOSICAO",
      "PC_RENDA_DISP", "PC_RENDA_MONET", "PC_RENDA_NAO_MONET","PC_DEDUCAO"
      )
    ),
  na = c(" ")
  ) |> 
  janitor::clean_names() |> 
  dplyr::mutate(
    cod_uc = as.numeric(paste0(cod_upa, num_dom, num_uc)),
    cod_pessoa = as.numeric(paste0(cod_uc, cod_informante))
    )

## Domicílio

quest_domicilio <- readr::read_fwf(
  file = "data-raw/DOMICILIO.txt",
  readr::fwf_widths(
    widths = c(2, 4, 1, 9, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1,
               1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 14, 14, 1),
    col_names = c(
      "uf", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", 
      "NUM_DOM", "V0201", "V0202", "V0203", "V0204", "V0205",
      "V0206", "V0207", "V0208", "V0209", "V02101", "V02102",
      "V02103", "V02104", "V02105", "V02111", "V02112", "V02113",
      "V0212", "V0213", "V02141", "V02142", "V0215", "V02161", 
      "V02162", "V02163", "V02164", "V0217", "V0219", "V0220",
      "V0221", "PESO", "PESO_FINAL", "V6199"
      )
    ),
  na = c(" ")) |> 
  janitor::clean_names() |>
  dplyr::mutate(cod_dom = as.numeric(paste0(cod_upa, num_dom)))


# Fazendo o join os dados -------------------------------------------------------

pof_raw <- dplyr::inner_join(
  x = quest_morador,
  y = quest_domicilio,
  dplyr::join_by(
    "cod_upa", "uf", "peso", "peso_final",
    "estrato_pof", "num_dom", "tipo_situacao_reg"
    )
  )

# Criando variáveis -------------------------------------------------------

pof_clean <- pof_raw |>
  dplyr::mutate(
    uf = factor(
      dplyr::case_when(
        uf == "11" ~ "Rondônia",
        uf == "12" ~ "Acre",
        uf == "13" ~ "Amazonas",
        uf == "14" ~ "Roraima",
        uf == "15" ~ "Pará",
        uf == "16" ~ "Amapá",
        uf == "17" ~ "Tocantins",
        uf == "21" ~ "Maranhão",
        uf == "22" ~ "Piauí",
        uf == "23" ~ "Ceará",
        uf == "24" ~ "Rio Grande Do Norte",
        uf == "25" ~ "Paraíba",
        uf == "26" ~ "Pernambuco",
        uf == "27" ~ "Alagoas",
        uf == "28" ~ "Sergipe",
        uf == "29" ~ "Bahia",
        uf == "31" ~ "Minas Gerais",
        uf == "32" ~ "Espirito Santo",
        uf == "33" ~ "Rio De Janeiro",
        uf == "35" ~ "São Paulo",
        uf == "41" ~ "Paraná",
        uf == "42" ~ "Santa Catarina",
        uf == "43" ~ "Rio Grande Do Sul",
        uf == "50" ~ "Mato Grosso Do Sul",
        uf == "51" ~ "Mato Grosso",
        uf == "52" ~ "Goiás",
        uf == "53" ~ "Distrito Federal"
        )
      ),
    regiao = factor(
      dplyr::case_when(
        uf %in%  c("Bahia", "Piauí", "Maranhão", "Rio Grande Do Norte", "Sergipe", "Alagoas", "Ceará", "Paraíba", "Pernambuco") ~ "Nordeste",
        uf %in% c("Tocantins", "Amazonas", "Pará", "Amapá", "Acre", "Roraima", "Rondônia") ~ "Norte",
        uf %in% c("São Paulo", "Minas Gerais", "Espírito Santo", "Rio de Janeiro") ~ "Sudeste",
        uf %in% c("Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul") ~ "Centro-Oeste",
        uf %in% c("Rio Grande do Sul", "Santa Catarina", "Paraná") ~ "Sul"
        )
      ),
    nivel_instrucao = factor(
      dplyr::case_when(
        instrucao == 1 ~ "Sem instrução",
        instrucao == 2 ~ "Ensino fundamental incompleto",
        instrucao == 3 ~ "Ensino fundamental completo",
        instrucao == 4 ~ "Ensino médio incompleto",
        instrucao == 5 ~ "Ensino médio completo",
        instrucao == 6 ~ "Ensino superior incompleto",
        instrucao == 7 ~ "Ensino superior completo"
        ),
      levels = c(
        "Sem instrução",
         "Ensino fundamental incompleto",
         "Ensino fundamental completo",
         "Ensino médio incompleto",
         "Ensino médio completo",
         "Ensino superior incompleto",
         "Ensino superior completo"
        ),
      ordered = TRUE
      ),
    sexo = factor(
      ifelse(v0404 == 1, "Homem", "Mulher")
      ),
    raca = factor(
      dplyr::case_when(
        v0405 == 1 ~ "Branca",
        v0405 == 2 ~ "Preta",
        v0405 == 3 ~ "Amarela",
        v0405 == 4 ~ "Parda",
        v0405 == 5 ~ "Indígena",
        v0405 == 9 ~ "Não declarado"
        )
      ),
    idade = v0403,
    renda_disp_fam_pc = pc_renda_disp,
    tipo_domicilio = factor(
      dplyr::case_when(
        v0201 == 1 ~ "Casa",
        v0201 == 2 ~ "Apartamento",
        v0201 == 3 ~ "Cortiço ou afins"
        )
      ),
    forma_abastecimento = factor(
      dplyr::case_when(
        v0207 == 1 ~ "Rede geral de abastecimento",
        v0207 == 2 ~ "Poço profundo ou artesiano",
        v0207 == 3 ~ "Poço raso ou freático",
        v0207 == 4 ~ "Fonte ou nascente",
        v0207 == 5 ~ "Água da chuva armazenada",
        v0207 == 6 ~ "Outra forma de abastecimento"
        )
      ),
    forma_escoadouro = factor(
      dplyr::case_when(
        v0212 == 1 ~ "Rede geral ou fossa ligada à rede",
        v0212 == 2 ~ "Fossa não ligada à rede",
        v0212 == 3 ~ "Vala",
        v0212 == 4 ~ "Rio, lago ou mar",
        v0212 == 5 ~ "Outra forma"
        )
      ),
    forma_escoadouro_modificado = factor(
      dplyr::case_when(
        v0212 == 1 ~ "Rede geral ou fossa ligada à rede",
        v0212 == 2 ~ "Fossa não ligada à rede",
        TRUE ~ "Outra forma"
        )
      ),
    tipo_situacao_reg = factor(
      ifelse(tipo_situacao_reg == 1, "Urbano", "Rural")
      ),
    tipo_destino_lixo = factor(
      dplyr::case_when(
        v0213 == 1 ~ "Coletado diretamente por serviço de limpeza",
        v0213 == 2 ~ "Coletado em caçamba de serviço de limpeza",
        v0213 == 3 ~ "Queimado (na propriedade)",
        v0213 == 4 ~ "Enterrado (na propriedade)",
        v0213 == 5 ~ "Jogado em terreno baldio ou logradouro",
        v0213 == 6 ~ "Outro destino"
        )
      ),
    posse_domicilio = factor(
      dplyr::case_when(
        v0217 == 1 ~ "Próprio, já pago",
        v0217 == 2 ~ "Próprio, ainda pagando",
        v0217 == 3 ~ "Alugado",
        v0217 == 4 ~ "Cedido por empregador",
        v0217 == 5 ~ "Cedido por familiar",
        v0217 == 6 ~ "Cedido de outra forma",
        v0217 == 7 ~ "Outra condição"
        )
      ),
    rede_eletrica_geral = ifelse(v02141 == 1, "Sim", "Não"),
    niv_ins_alimentar = factor(
      dplyr::case_when(
        v6199 == 1 ~ "Segurança",
        v6199 == 2 ~ "Insegurança leve",
        v6199 == 3 ~ "Insegurança moderada",
        v6199 == 4 ~ "Insegurança grave"
        ),
      levels = c(
        "Insegurança grave",
        "Insegurança moderada",
        "Insegurança leve",
        "Segurança"
        ),
      ordered = TRUE
      )
) 

# Formato amostral complexo -----------------------------------------------

pof_nordeste <- pof_clean |> 
  dplyr::filter(regiao == "Nordeste") |> 
  srvyr::as_survey_design(
    ids =  cod_upa, 
    strata = estrato_pof, 
    weights = peso_final, 
    nest = TRUE
    )

# Análise exploratória ----------------------------------------------------

## População da região Nordeste, nesta amostra, corresponde ao total de
## 56,5 milhões de pessoas, aproximadamente

pop_total <- pof_nordeste |> 
  srvyr::survey_count()





# Preparação dos dados ----------------------------------------------------

pof_nordeste <- pof_nordeste |>
 dplyr:: mutate(
    ins_alim = ifelse(niv_ins_alimentar != "Segurança", 1, 0),
    renda_familiar_total = renda_total,
    homem = ifelse(sexo == "Homem", 1, 0),
    raca_branca = ifelse(raca == "Branca", 1, 0),
    negro = ifelse(raca == "Preta", 1, 0),
    homem_branco = ifelse(sexo == "Homem" & raca == "Branca", 1, 0),
    mulher_branca = ifelse(sexo == "Mulher" & raca == "Branca", 1, 0),
    homem_preto_pardo = ifelse(sexo == "Homem" & raca %in% c("Preta", "Parda"), 1, 0),
    mulher_preta_parda = ifelse(sexo == "Mulher" & raca %in% c("Preta", "Parda"), 1, 0),
    anos_instrucao = instrucao,
    nivel_instrucao = nivel_instrucao,
    comp_familiar = composicao,
    idade = idade,
    tipo_zona = ifelse(tipo_situacao_reg == "Urbana", 1, 0),
    abastecimento_agua = ifelse(forma_abastecimento == "Rede geral de abastecimento", 1, 0),
    esgotamento_sanitario = ifelse(forma_escoadouro == "Rede geral ou fossa ligada à rede", 1, 0),
    destino_lixo = ifelse(tipo_destino_lixo == "Coletado diretamente por serviço de limpeza", 1, 0),
    energia_eletrica = ifelse(rede_eletrica_geral == "Sim", 1, 0)
    )

# Estimação econométrica --------------------------------------------------

mod_logit <- survey::svyglm(
  formula = ins_alim ~ renda_familiar_total + homem + sexo * raca +
    anos_instrucao + idade + tipo_zona + composicao,
  family = binomial(link = "logit"),
  design = pof_nordeste
  )

# Análise -----------------------------------------------------------------

gtsummary::tbl_regression(mod_logit, exponentiate = TRUE)




