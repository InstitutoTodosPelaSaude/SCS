rm(list = ls())
gc()

#Atualiza o dataframe para São Caetano do Sul
library(tidytable)
library(readr)
library(geobr)
library(lubridate)
library(stringr)
library(readxl)

## Setting the working directory
setwd("~/Desktop/repos/SCS/")

## Loading databases
load("Data/SCS.RData")
load("Data/bairrosSCS.RData")
load("Data/stopwords.RData")
load("Data/cid10.RData")

## No axis
# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

## Maps
sp_state<-read_state(code_state = "SP")

sp_rm<-read_metro_area(year = 2018) |> 
  filter(abbrev_state == "SP", name_metro == "RM São Paulo") |> 
  mutate(cores = as.factor(case_when(name_metro == "RM São Paulo" ~ 'firebrick2', 
                                     name_metro != "RM São Paulo" ~ 'grey90'))) |> 
  st_as_sf()

plt_sp_rm<-ggplot()+
  geom_sf(data = sp_state,
          show.legend = F)+
  geom_sf(data = sp_rm, 
          aes(fill = cores),
          show.legend = F)+
  scale_fill_manual(values = c('firebrick2', 'grey90'))+
  theme_void()
plt_sp_rm

ggsave(filename = "Outputs/state_sp_rm_destacada.png", 
       plot = plt_sp_rm, 
       width = 11, 
       height = 9, 
       dpi = 100)

sp_rm_scs <- read_metro_area(year = 2018) |> 
  filter(abbrev_state == "SP", name_metro == "RM São Paulo") |> 
  mutate(cores = as.factor(case_when(name_muni == "São Caetano Do Sul" ~ 'firebrick2', 
                                     name_muni != "São Caetano Do Sul" ~ 'grey90'))) |> 
  st_as_sf()

plt_sp_rm_scs<-ggplot()+
  geom_sf(data = sp_rm_scs,
          aes(fill = cores), 
          show.legend = F)+
  scale_fill_manual(values = c('firebrick2', 'grey90'))+
  theme_void()+
  no_axis
plt_sp_rm_scs

ggsave(filename = "Outputs/rm_sp_scs_destacada.png", 
       plot = plt_sp_rm_scs, 
       width = 11, 
       height = 9, 
       dpi = 100)

### Atendimentos Hospital - Total 2022
AtendHosp <- SCS %>%
  select.(dt_atendimento, tx_unidade_organizacional) %>% 
  filter.(year(dt_atendimento)==2022) %>%
  summarise.(n = n(), .by = tx_unidade_organizacional) %>%
  filter.(tx_unidade_organizacional == "HMEAS") %>%
  select.(n)

### Atendimentos Hospital - Média diária 2022
MedAtendHosp <- SCS %>%
  select.(dt_atendimento, tx_unidade_organizacional) %>% 
  filter.(year(dt_atendimento)==2022) %>%
  mutate.(day = day(dt_atendimento)) %>% 
  summarise.(n = n(), .by = c("tx_unidade_organizacional", "day")) %>%
  filter.(tx_unidade_organizacional=="HMEAS") %>%
  summarise.(AtendMed = mean(n)) %>%
  round()

### Atendimentos UPA - Total 2022
AtendUPA <- SCS %>%
  select.(dt_atendimento, tx_unidade_organizacional) %>% 
  filter.(year(dt_atendimento)==2022) %>%
  summarise.(n = n(), .by = tx_unidade_organizacional) %>%
  filter.(tx_unidade_organizacional == "UPA") %>%
  select.(n)

### Atendimentos UPA - Média diária 2022
MedAtendHosp <- SCS %>%
  select.(dt_atendimento, tx_unidade_organizacional) %>% 
  filter.(year(dt_atendimento)==2022) %>%
  mutate.(day = day(dt_atendimento)) %>% 
  summarise.(n = n(), .by = c("tx_unidade_organizacional", "day")) %>%
  filter.(tx_unidade_organizacional=="UPA") %>%
  summarise.(AtendMed = mean(n)) %>%
  round()

### Casos de febre - Total 2022
SCSFebre <- SCS %>% 
  filter.(year(dt_atendimento) == 2022) %>% 
  filter.(febre == 1) %>% 
  summarise.(febres = n(), .by = febre) %>% 
  select.(febres)

### Casos de febre - Média diária 2022
SCSFebMed <- SCS %>%
  select.(dt_atendimento, febre) %>% 
  filter.(year(dt_atendimento)==2022) %>%
  filter.(febre == 1) %>%
  mutate.(day = day(dt_atendimento)) %>% 
  summarise.(n = n(), .by = day) %>%
  summarise.(AtendMed = mean(n)) %>%
  round()

### Dist. de atendimentos por sexo e idade
SCSagem <- SCS %>% 
  filter.(year(dt_atendimento)==2022) %>%
  filter.(cs_sexo == "M") %>% 
  summarise.(Masculino = sum(febre == 1), .by = groupage)
SCSagef <- SCS %>% 
  filter.(year(dt_atendimento)==2022) %>%
  filter.(cs_sexo == "F") %>%
  summarise.(Feminino = sum(febre == 1), .by = groupage)
SCSage <- merge(SCSagem, SCSagef, by = "groupage")
SCSage <- SCSage %>% 
  arrange.(SCSage$groupage) %>% 
  relocate.(groupage, .after = Feminino)
SCSage$groupage <- as.character(SCSage$groupage)
SCSagePerc <- SCSage %>% 
  mutate.(popt = Masculino + Feminino) %>% 
  mutate.(PopPerc = popt / sum(popt) * 100) %>% 
  mutate.(PopPercM = Masculino / sum(popt) * 100) %>%
  mutate.(PopPercF = Feminino / sum(popt) * 100) %>% 
  select.(PopPercM, PopPercF, groupage)

pyramid(SCSagePerc, Clab = "Faixa etária", Llab = "Masculino (%)", Rlab = "Feminino (%)", 
        Lcol = "#4d77bb", Rcol = "#ff6555",
        main = "Pirâmide etária de atendimentos - S. Caetano do Sul (2022)")

### Dist. de atendimentos no tempo
sem_atual <- epiweek(max(SCS$dt_atendimento))
SCSTemp <- SCS %>% 
  filter.(year(dt_atendimento) == 2022) %>%
  mutate(epiweek = epiweek(dt_atendimento)) |> 
  summarise.(n = n(), .by = c(epiweek, tx_unidade_organizacional)) %>%
  spread(tx_unidade_organizacional, n) %>% 
  arrange.(epiweek)
colunas <- c(Hospital = "#ff6555", UPA = "#4d77bb")

ggplot(SCSTemp) +
  geom_line(aes(x=epiweek, y = HMEAS, colour="Hospital"), size = 3) +
  geom_line(aes(x=epiweek, y = UPA, colour="UPA"), size = 3) +
  scale_colour_manual(values =  colunas) +
  theme(legend.position = c(0.95, 0.15),
        legend.justification = c("right", "top")) +
  theme_classic() +
  labs(
    title = "Atendimentos por semana",
    subtitle = "(2022)",
    caption = "Dados do MV - Prontuário",
    tag = "",
    x = "Tempo (semana epidemiológica)",
    y = "Número de atendimentos (N)",
    colour = "Legenda")

### Taxa de febre por bairro de S. Caetano do Sul
SCSbairro <- SCS %>%
  filter.(name_muni=="SAO CAETANO DO SUL") %>%
  summarise.(febre = sum(febre == 1, pop = max(populacao_total)), 
             .by = c("code_neighborhood", "populacao_total")) %>%
  arrange.(code_neighborhood) %>%
  mutate.(popsem = populacao_total/sem_atual) %>% 
  mutate.(TaxaFebre = (febre/populacao_total) * (1000)) %>% 
  inner_join.(bairrosSCS, .by = "code_neighborhood") %>% 
  st_as_sf()

#FFFFFF, , #91bfdb, , #fc8d59, #b54545, 

plt_incidence <- SCSbairro |> 
  ggplot() +
  geom_sf(aes(fill = TaxaFebre), 
          size = .15) +
  colorspace::scale_fill_binned_sequential(palette = "ag_GrnYl",
                                           n.breaks = 10,
                                           guide = guide_colorbar(title = "Fever Incidence rate (per 1.000 hab.)",
                                                                  title.position = "bottom",
                                                                  ticks = F, 
                                                                  label.position = "top",
                                                                  barwidth = 20)) +
  geom_sf_text(aes(label = name_neighborhood), 
               colour = "grey0", 
               size = 3.5) +
  xlab("") +  ylab("") +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.title.align = 0.5)
plt_incidence

ggsave(filename = "Outputs/fever_incidence_scs.png", 
       plot = plt_incidence, 
       width = 11, 
       height = 9, 
       dpi = 100)

sem_atual <- epiweek(max(week(SCS$dt_atendimento)))
SCSfebhist <- SCS %>% 
  filter.(year(dt_atendimento) == 2022) %>% 
  filter.(sem_atend<sem_atual) %>% 
  summarise.(feb = sum(febre == 1), .by = sem_atend) %>%
  arrange.(sem_atend) %>%
  sapply(function(x) {
    replace(x, is.na(x) | is.infinite(x), 0)
  })

SCSfebhist <- as.data.frame(SCSfebhist)
cols <- c(Media = "#4D77BB", Febre2022 = "#FF0066")

fever_scs<-ggplot(SCSfebhist) +
  #geom_ribbon(aes(x = sem_atend, ymin = LowB,ymax=HigB), fill="#4D77BB", alpha=0.5) +
  #geom_line(aes(x=sem_atend, y = FebreMean, colour="Media"), lwd=1.5) +
  geom_line(aes(x=sem_atend, y = feb, colour="#FFEA00"), show.legend = F, lwd=1.5) +
  theme(legend.position = c(0.95, 0.15),
        legend.justification = c("right", "top")) +
  theme_classic() +
  labs(
    title = "",
    # subtitle = "2022",
    caption = "",
    x = "Epiweek",
    y = "Counts of Fever relating cases (N)",
    colour = "Legenda")
fever_scs

ggsave(filename = "Outputs/fever_time_series_scs.png", 
       plot = fever_scs, 
       width = 18, 
       height = 9, 
       dpi = 100)

library(patchwork)

patchwork_scs<-(((plt_sp_rm_scs + 
                    inset_element(plt_sp_rm, 
                                  .8, .8, 1, 1, 
                                  align_to = "full")) + plt_incidence) / 
                  (fever_scs))+
  plot_annotation(tag_levels = 'A')
patchwork_scs

ggsave(filename = "Outputs/patchwork_fever_scs.png", 
       plot = patchwork_scs, 
       width = 11, 
       height = 9, 
       dpi = 200)
