rm(list = ls())
gc()

#Atualiza o dataframe para São Caetano do Sul
library(tidytable)
library(readr)
library(geobr)
library(lubridate)
library(stringr)

## Setting the working directory, not needed if you open via .rproj
# setwd(getwd())

#Abre bases de dados de São Caetano e une em único arquivo
arquivos <- list.files("Data/Datasets_SCS/", pattern = ".xlsx", full.names = T)
# setwd("Datasets_SCS/")

## Carregar CID-10
cid10 <- read_delim("Data/Datasets_SCS/cid10.csv", delim = ";", 
                    escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                    trim_ws = TRUE) %>% 
  select(SUBCAT, DESCRICAO) %>% 
  rename(CID10 = SUBCAT)

## Read all the .xls files
SCS <- lapply(arquivos, function(x){
  x<-read_excel(x)
}) %>% 
  bind_rows()

#Carrega os dados de bairro de SCS
bairros <- read_neighborhood(year = 2010)
bairrosSCS <- bairros %>% 
  filter(name_muni == "São Caetano Do Sul") %>% 
  distinct(code_neighborhood, .keep_all = TRUE) %>% 
  select(name_neighborhood, code_neighborhood, geom) %>% 
  mutate(code_neighborhood = as.numeric(code_neighborhood))

#Carrega os dados de população dos bairros de SCS
pop_scs_bairro_estimada <- read.csv("Data/Datasets_SCS/pop_scs_bairro_estimada.csv", sep = ";") %>% 
  select(code_neighborhood, populacao_total)

#Renomeia as variáveis do banco para padronizar
SCS <- SCS %>% 
  rename(id_atendimento = `Cód. Atend.`, dt_atendimento = `Data Atend.`,
         tx_unidade_organizacional = Hospital, tx_diagnostico = `Hipótese Diagnóstica`,
         tx_codigo = CID, id_paciente = Prontuario, cs_sexo = Sexo,
         nascimento = `Data Nasc.`, tx_cidade = Cidade, tx_bairro = Bairro,
         tx_estado = Estado, queixa = `Queixa Principal`
  ) %>%
  relocate("id_atendimento", "dt_atendimento", "tx_unidade_organizacional", "tx_diagnostico",
            "tx_codigo", "id_paciente", "cs_sexo", "nascimento", "tx_cidade", "tx_bairro",
            "tx_estado", "queixa"
  )

#Ajusta a data de atendimento e nascimento
SCS <- SCS %>% 
  mutate(# Formatando variável de data de atendimento
    dt_atendimento = as_date(dt_atendimento, format = "%d/%m/%Y"),
    # Formatando variável de data de nascimento
    nascimento =  as_date(nascimento, format = "%d/%m/%Y"), 
    # Criando epiweek
    sem_atend = epiweek(dt_atendimento)) %>% 
  mutate(
    # Criando variável de idade continua
    age = trunc((nascimento %--% dt_atendimento) / years(1))) %>% 
  mutate(
    #Cria variáveis idade e faixa etária
    groupage = cut(age, 
                   breaks = c(0,5,seq(10,80, 10), 120), 
                   right = T),
    # Transformando variável sexo em factor
    cs_sexo = as.factor(cs_sexo)
  ) %>% 
  rename(name_muni = tx_cidade, 
         name_neighborhood = tx_bairro) %>% 
  filter(name_muni == "SAO CAETANO DO SUL") %>% 
  mutate(code_neighborhood = as.numeric(case_when(
    grepl("^Barcelona", name_neighborhood, ignore.case = T) ~ "354880705003",
    grepl("^Boa",  name_neighborhood, ignore.case = T) ~ "354880705005",
    grepl("^Centro",  name_neighborhood, ignore.case = T) ~ "354880705015",
    grepl("^Cer",  name_neighborhood, ignore.case = T) ~ "354880705012",
    grepl("^Funda",  name_neighborhood, ignore.case = T) ~ "354880705001",
    grepl("^Jardim",  name_neighborhood, ignore.case = T) ~ "354880705008",
    grepl("^Mau",  name_neighborhood, ignore.case = T) ~ "354880705007",
    grepl("^Nova",  name_neighborhood, ignore.case = T) ~ "354880705006",
    grepl("Pico",  name_neighborhood, ignore.case = T) ~ "354880705011",
    grepl("^Osvaldo",  name_neighborhood, ignore.case = T) ~ "354880705010",
    grepl("^Prosperidade",  name_neighborhood, ignore.case = T) ~ "354880705002",
    grepl("Maria",  name_neighborhood, ignore.case = T) ~ "354880705004",
    grepl("Paula",  name_neighborhood, ignore.case = T) ~ "354880705014",
    grepl("^Santo",  name_neighborhood, ignore.case = T) ~ "354880705013", 
    grepl("Jose",  name_neighborhood, ignore.case = T) ~ "354880705009"
  ))) %>% 
  #Cria variáveis para agrupar hipóteses diagnósticas por tipo SCS
  ## Testar com termos para Febres indeterminadas, Respiratórias em geral e Gastroenterite, termos para Exantemáticas
  mutate(febre = if_else(grepl("febr|malária|malaria|dengue|zika|chik|^sf", queixa, ignore.case = T), 1, 0))%>% 
  mutate(ofidi = if_else(grepl("ofid|ofíd|serpen", queixa, ignore.case = T), 1, 0)) %>% 
  mutate(covid = if_else(grepl("sars|covid|coronav", queixa, ignore.case = T), 1, 0)) %>% 
  mutate(malaria = if_else(grepl("malária|malari", queixa, ignore.case = T), 1, 0)) %>% 
  mutate(geca = if_else(grepl("geca|gastro|dor abdominal", queixa, ignore.case = T), 1, 0)) %>% 
  ## juntando por bairros
  inner_join(pop_scs_bairro_estimada, by = "code_neighborhood") %>% 
  filter(sem_atend <= epiweek(today()))

#Baixa stopwords em português Brasil
stopwords <- stopwords <- read.delim(
  file = "http://www.labape.com.br/rprimi/ds/stopwords.txt", 
  header = FALSE,
  col.names = "palavras") 

#Remove espaços em branco das stop words
stopwords <- stopwords %>% 
  mutate(palavras = str_replace_all(string = palavras, pattern = " ", repl = ""))

## Salvando
save(cid10, file = "Data/cid10.RData")
save(SCS, file = "Data/SCS.RData")
save(bairrosSCS, file = "Data/bairrosSCS.RData")
save(stopwords, file = "Data/stopwords.RData")

## Removendo bairros
rm(bairros, bairrosSCS, pop_scs_bairro_estimada)

#