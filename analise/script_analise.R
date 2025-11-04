library(tidyverse); library(survey); library(ggeffects); library(svyVGAM);
library(patchwork); library(gtsummary); library(prais)

# themes

theme_avp <- function() {
  theme_classic(base_size=10) +
    theme(axis.title.x = element_text(size = 9),
          axis.title.y = element_text(size = 9),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(),
          panel.grid.major.y = element_line())
}

theme_sharp2 <- function() {
  theme_classic(base_size = 12) +
    theme(panel.grid.major.x = element_line(),
          plot.tag = element_text(face="bold"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank())
}

# banco

load("banco_reduzido.Rdata")

vigitel <- vigitel |> 
  mutate(q9 = if_else(q9 %in% c(777, 888), NA_real_, q9),
         q11 = if_else(q11 %in% c(777, 888), NA_real_, q11),
    
    escolaridade_cat_custom = case_when(q8a %in% c(1, 2, 3, 8) ~ "Não completou 1º grau",
                                  q8a == 4 ~ "1º grau ou equivalente completo",
                                  q8a == 5 ~ "2º ou equivalente completo",
                                  q8a %in% c(6, 7) ~ "3º grau completo e/ou pós-graduação",
                                  q8a %in% c(777, 888) ~ NA_character_),
         
         escolaridade_cat_num = case_when(q8_anos <= 4 ~ "0-4 anos", 
                                           q8_anos >= 5 & q8_anos <=8 ~ "5-8 anos",
                                           q8_anos >= 9 & q8_anos <=11 ~ "9-11 anos",
                                           q8_anos >= 12 ~ "12 ou mais anos",
                                           q8_anos %in% c(777, 888) ~ NA_character_),
         
         escolaridade_cat_num2 = case_when(q8_anos < 1 ~ "0",
                                          q8_anos >= 1 & q8_anos <=4 ~ "1-4 anos", 
                                           q8_anos >= 5 & q8_anos <=8 ~ "5-8 anos",
                                           q8_anos >= 9 & q8_anos <=11 ~ "9-11 anos",
                                           q8_anos >= 12 ~ "12 ou mais anos",
                                          q8_anos %in% c(777, 888) ~ NA_character_),
         
         trajeto_trabalho = case_when(q47 == 2 ~ 0,
                                      q50 %in% c(1, 2) ~ 1,
                                      q50 == 3 ~ 0),
    
         q51 = as.numeric(q51),
         q54 = as.numeric(q54),
         
         duracao_trajeto_trabalho = case_when(trajeto_trabalho == 0 ~ 0,
                                              TRUE ~ q51),
         
         trajeto_curso = case_when(q52 == 888 ~ 0,
                                   q52 == 2 ~ 0,
                                   q53 == 3 ~ 0,
                                   q53 %in% c(1, 2) ~ 1),
         
         duracao_trajeto_curso = case_when(trajeto_curso == 0 ~ 0,
                                           TRUE ~ q54),
    
         q51medio = case_when(
           duracao_trajeto_trabalho == 0 ~ 0,
           duracao_trajeto_trabalho == 1 ~ 4.5,
           duracao_trajeto_trabalho == 2 ~ 14.5,
           duracao_trajeto_trabalho == 3 ~ 24.5,
           duracao_trajeto_trabalho == 4 ~ 34.5,
           duracao_trajeto_trabalho == 5 ~ 44.5,
           duracao_trajeto_trabalho == 6 ~ 54.5,
           duracao_trajeto_trabalho == 7 ~ 60,
            TRUE ~ NA_real_
            ),
         q54medio = case_when(
           duracao_trajeto_curso == 0 ~ 0,
           duracao_trajeto_curso == 1 ~ 4.5,
           duracao_trajeto_curso == 2 ~ 14.5,
           duracao_trajeto_curso == 3 ~ 24.5,
           duracao_trajeto_curso == 4 ~ 34.5,
           duracao_trajeto_curso == 5 ~ 44.5,
           duracao_trajeto_curso == 6 ~ 54.5,
           duracao_trajeto_curso == 7 ~ 60,
            TRUE ~ NA_real_
            ),
         
         duracao_desloc = q51medio + q54medio,
         
         ativamente_fisico_desloc_novo = if_else(duracao_desloc >= 30, "sim", "não"),
         ativamente_fisico_desloc_num = if_else(duracao_desloc >= 30, 1, 0),
         
         cat_ativ = case_when(duracao_desloc == 0 ~ "Não realiza",
                              duracao_desloc > 0 & duracao_desloc < 4 ~ ">0 e ≤30 minutos",
                              duracao_desloc >= 4 & duracao_desloc < 7 ~ ">30 e ≤60 minutos",
                              duracao_desloc >= 7 ~ ">60 minutos"),
         
         macro_reg = case_when(
          cidade %in% c(1, 9, 11, 13, 15, 19, 22, 23, 25) ~ "Nordeste",
          cidade %in% c(2, 3, 4, 12, 14, 16, 18, 20) ~ "Norte",
          cidade %in% c(3, 21, 24, 26) ~ "Sudeste",
          cidade %in% c(27, 5, 6, 10) ~ "Centro-Oeste",
          cidade %in% c(8, 17, 7) ~ "Sul"),
         
         cat_ativ = ordered(cat_ativ, 
                             levels = c("Não realiza",
                                        ">0 e ≤30 minutos",
                                        ">30 e ≤60 minutos",
                                        ">60 minutos")),
    
    q69 = case_when(ano_edicao %in% c(2009, 2010) & q69 == 1 ~ "branca",
                    ano_edicao %in% c(2009, 2010) & q69 == 2 ~ "preta",
                    ano_edicao %in% c(2009, 2010) & q69 == 3 ~ "parda",
                    ano_edicao %in% c(2009, 2010) & q69 == 4 ~ "amarela",
                    ano_edicao %in% c(2009, 2010) & q69 == 5 ~ "indigena",
                    ano_edicao %in% c(2009, 2010) & q69 == 777 ~ "nao_sabe",
                    ano_edicao %in% c(2009, 2010) & q69 == 888 ~ "nao_respondeu",
                    
                    ano_edicao %in% c(2011:2023) & q69 == 1 ~ "branca",
                    ano_edicao %in% c(2011:2023) & q69 == 2 ~ "preta",
                    ano_edicao %in% c(2011:2023) & q69 == 3 ~ "amarela",
                    ano_edicao %in% c(2011:2023) & q69 == 4 ~ "parda",
                    ano_edicao %in% c(2011:2023) & q69 == 5 ~ "indigena",
                    ano_edicao %in% c(2011:2023) & q69 == 777 ~ "nao_sabe",
                    ano_edicao %in% c(2011:2023) & q69 == 888 ~ "nao_respondeu",
                    ),
    
    cor_2 = case_when(q69 %in% c("preta", "parda") ~ "negros",
                      q69 %in% c("branca", "amarela") ~ "brancos",
                      TRUE ~ q69)) |> 
  rename(ano = ano, idade = q6, sexo = q7, faixa_etaria = fet,  
         grau_escolar = q8a, peso_kg = q9, altura_cm = q11, faz_exercicio = q42, 
         trabalha = q47, anda_a_pe = q48, carrega_peso = q49, faz_trajeto_a_pe = q50, 
         faz_curso_escola = q52, fumante = q60, ex_fumante = q64, cor = q69,
         estado_saude = q74, pressao_alta = q75, diabetes = q76, colesterol_trig = q78,
         posse_plano = q88) |> 
  
  mutate(across(c(faixa_etaria, escolaridade_cat_custom, escolaridade_cat_num, escolaridade_cat_num2, 
                  grau_escolar, faz_exercicio, trabalha,
                  anda_a_pe, carrega_peso, faz_trajeto_a_pe, faz_curso_escola,
                  fumante, ex_fumante, cor, cor_2, estado_saude, pressao_alta, diabetes,
                  colesterol_trig, posse_plano), as.factor))

vigitel |> group_by(escolaridade_cat_num) |> count()
vigitel |> group_by(escolaridade_cat_num2) |> count()
vigitel |> group_by(cor, ano) |> count()

vigi_table <- vigitel |> select(ano, ano_edicao, peso_kg, altura_cm, faixa_etaria, sexo, cor, 
                     escolaridade_cat_num, macro_reg, trabalha, 
                     faz_exercicio, carrega_peso, anda_a_pe, faz_trajeto_a_pe, 
                     faz_curso_escola, fumante, pressao_alta, diabetes, colesterol_trig, pesorake,
                     duracao_desloc, ativamente_fisico_desloc_novo, ativamente_fisico_desloc_num, 
                     cat_ativ)
##

vigi_design <- svydesign(id = ~ 1, 
                         strata = NULL,
                         weights= ~ pesorake,
                         data=vigitel)

vigi_table_design <- svydesign(id = ~ 1, 
                         strata = NULL,
                         weights= ~ pesorake,
                         data=vigi_table)

vigi_design <- subset(vigi_design, !is.na(ativamente_fisico_desloc_num))
vigi_design <- subset(vigi_design, !is.na(duracao_desloc))

vigi_table_design <- subset(vigi_table_design, !is.na(ativamente_fisico_desloc_num))
vigi_table_design <- subset(vigi_table_design, !is.na(duracao_desloc))

vigi_design <- update(ativamente_fisico_desloc = ativamente_fisico_desloc_novo, vigi_design)
vigi_design <- update(ano = ano_edicao, vigi_design) # corrigindo variável de ano

vigi_table_design <- update(ativamente_fisico_desloc = ativamente_fisico_desloc_novo, vigi_table_design)
vigi_table_design <- update(ano = ano_edicao, vigi_table_design) # corrigindo variável de ano

## pergunta 1) existe tendencias ao longo dos anos no % de indivíduos que atingem >150 min?
# qual a influencia das macroregioes?

# modelo

m1 <- svyglm(ativamente_fisico_desloc_num ~ ano, family = quasibinomial(), design=vigi_design)
summary(m1)
#tbl_regression(m1, exponentiate=T)

m1_spline <- svyglm(ativamente_fisico_desloc_num ~ ns(ano, knots=c(2015, 2020, 2021)), family = quasibinomial(), design=vigi_design)
summary(m1_spline)
car::Anova(m1_spline)

anova(m1, m1_spline)
AIC(m1, m1_spline)

m1_spline_pred <- predict(m1_spline, type="response") |> 
  as_tibble() |> 
  mutate(ano = vigi_design$variables$ano,
         conf.low = response - (SE * 1.96),
         conf.high = response + (SE * 1.96)) |> group_by(ano) |> slice(1) |> 
  ungroup() |> 
  mutate(macro_reg = "Geral")

m2 <- svyglm(ativamente_fisico_desloc_num ~ ano + macro_reg, family = quasibinomial(), design=vigi_design)
summary(m2)
car::Anova(m2, type="II")

m2_spline <- svyglm(ativamente_fisico_desloc_num ~ ns(ano, knots=c(2015, 2020, 2021)) + macro_reg, family = quasibinomial(), design=vigi_design)
car::Anova(m2_spline, type="II")

m2_1 <- svyglm(ativamente_fisico_desloc_num ~ ano * macro_reg, family = quasibinomial(), design=vigi_design)
summary(m2_1)
car::Anova(m2_1, type="II")

m2_1_spline <- svyglm(ativamente_fisico_desloc_num ~ ns(ano, knots=c(2015, 2020, 2021)) * macro_reg, family = quasibinomial(), design=vigi_design)
car::Anova(m2_1_spline, type="II")

AIC(m2, m2_spline, m2_1, m2_1_spline)

m2_1_spline_pred <- predict(m2_1_spline, type="response") |> 
  as_tibble() |> 
  mutate(ano = vigi_design$variables$ano,
         macro_reg = vigi_design$variables$macro_reg,
         conf.low = response - (SE * 1.96),
         conf.high = response + (SE * 1.96)) |> group_by(ano, macro_reg) |> slice(1) |> 
  ungroup()

# prop e CIs por ano

perg1_props <- svyby(
  formula = ~ativamente_fisico_desloc, 
  by = ~ano, 
  design = vigi_design,
  FUN = svyciprop, 
  method = "logit",  # Logit transform for CI
  vartype = "ci"     # Return confidence intervals
)

perg1_props <- perg1_props |> as_tibble() |> 
  mutate(macro_reg = "Geral") |> 
  mutate(estimate = paste0(round(ativamente_fisico_desloc, 3), " (", round(ci_l, 3), "; ", 
                           round(ci_u,3 ), ")"))

# writexl::write_xlsx(perg1_props, "proporcoes_e_IC_ativos_fisicamente_desloc.xlsx")

perg1_trend <- ggpredict(m1, terms=c("ano [all]")) |> as_tibble() 

(
plot_geral <- ggplot(perg1_props, aes(x=ano, y=ativamente_fisico_desloc,
                        ymin=ci_l, ymax=ci_u)) +
  geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
  facet_wrap(~macro_reg) +
  geom_pointrange() +
  geom_line() +
  geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
            nudge_y=.01, size=2.5) +
  geom_line(inherit.aes=F,
            data=perg1_trend,
            aes(x=x, y=predicted), alpha=.6,
            color="gray20") +
  geom_ribbon(inherit.aes=F,
            data=perg1_trend,
            aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high), alpha=.1,
            fill="gray20", color=NA) +
  scale_x_continuous(breaks=seq(2009, 2023, 1),
                     labels=c(seq(2009, 2021, 1), "", "2023")) +
  scale_y_continuous(limits=c(0, .25),
                     labels=paste0(seq(0, 25, 5), "%")) +
  labs(x="Ano", y="Indivíduos ativos no deslocamento") +
  theme_avp() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
)

(
  plot_geral_sp <- ggplot(perg1_props, aes(x=ano, y=ativamente_fisico_desloc,
                                        ymin=ci_l, ymax=ci_u)) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    facet_wrap(~macro_reg) +
    geom_pointrange() +
    geom_line() +
    geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
              nudge_y=.01, size=2.5) +
    geom_line(inherit.aes=F,
              data=m1_spline_pred,
              aes(x=ano, y=response, ymin=conf.low, ymax=conf.high), alpha=.6,
              color="gray20") +
    geom_ribbon(inherit.aes=F,
                data=m1_spline_pred,
                aes(x=ano, y=response, ymin=conf.low, ymax=conf.high), alpha=.1,
                fill="gray20", color=NA) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .25),
                       labels=paste0(seq(0, 25, 5), "%")) +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

# macroregioes

perg1_regioes_props <- svyby(
  formula = ~ativamente_fisico_desloc, 
  by = ~ano + macro_reg, 
  design = vigi_design,
  FUN = svyciprop, 
  method = "logit",  # Logit transform for CI
  vartype = "ci"     # Return confidence intervals
)

perg1_regioes_trend <- ggpredict(m2, terms=c("ano [all]", "macro_reg")) |> as_tibble() 

perg1_regioes_props <- perg1_regioes_props |> as_tibble() |> 
  mutate(estimate = paste0(round(ativamente_fisico_desloc, 3), " (", round(ci_l, 3), "; ", 
                           round(ci_u,3 ), ")"))

# writexl::write_xlsx(perg1_regioes_props, "MACROREG_proporcoes_e_IC_ativos_fisicamente_desloc.xlsx")

# modelo PW
# 
# m1_macroreg_PW <- prais_winsten(
#   ativamente_fisico_desloc ~ ano + macro_reg,
#   data=perg1_regioes_props,
#   index='ano')
# 
# summary(m1_macroreg_PW)
# 
# m1_macroreg_PW_int <- prais_winsten(
#   ativamente_fisico_desloc ~ ano * macro_reg,
#   data=perg1_regioes_props,
#   index='ano')
# 
# summary(m1_macroreg_PW_int)

(
plot_sudeste <- ggplot(subset(perg1_regioes_props, macro_reg == "Sudeste"), 
                       aes(x=ano, y=ativamente_fisico_desloc,
                        ymin=ci_l, ymax=ci_u, color=macro_reg)) +
  geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
  facet_wrap(~macro_reg) +
  geom_pointrange(color="#bfde19") +
  geom_line(color="#bfde19") +
  geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
            nudge_y=.01, size=2.5, color="black") +
  geom_line(inherit.aes=F,
            data=subset(perg1_regioes_trend, group == "Sudeste"),
            aes(x=x, y=predicted), alpha=.5,
            color="#bfde19") +
  geom_ribbon(inherit.aes=F,
              data=subset(perg1_regioes_trend, group == "Sudeste"),
              aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high), alpha=.05,
              fill="#bfde19", color=NA) +
  scale_x_continuous(breaks=seq(2009, 2023, 1),
                     labels=c(seq(2009, 2021, 1), "", "2023")) +
  scale_y_continuous(limits=c(0, .25),
                     labels=paste0(seq(0, 25, 5), "%")) +
  labs(x="Ano", y="Indivíduos ativos no deslocamento") +
  guides(color="none") +
  theme_avp() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
)

(
  plot_sul <- ggplot(subset(perg1_regioes_props, macro_reg == "Sul"), 
                         aes(x=ano, y=ativamente_fisico_desloc,
                             ymin=ci_l, ymax=ci_u, color=macro_reg)) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    facet_wrap(~macro_reg) +
    geom_pointrange(color="#3db85a") +
    geom_line( color="#3db85a") +
    geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
              nudge_y=.01, size=2.5, color="black") +
    geom_line(inherit.aes=F,
              data=subset(perg1_regioes_trend, group == "Sul"),
              aes(x=x, y=predicted), alpha=.5,
              color="#3db85a") +
    geom_ribbon(inherit.aes=F,
                data=subset(perg1_regioes_trend, group == "Sul"),
                aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high), alpha=.05,
                fill="#3db85a", color=NA) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .25),
                       labels=paste0(seq(0, 25, 5), "%")) +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    guides(color="none") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

(
  plot_norte <- ggplot(subset(perg1_regioes_props, macro_reg == "Norte"), 
                         aes(x=ano, y=ativamente_fisico_desloc,
                             ymin=ci_l, ymax=ci_u, color=macro_reg)) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    facet_wrap(~macro_reg) +
    geom_pointrange(color="#1f9075") +
    geom_line(color="#1f9075") +
    geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
              nudge_y=.01, size=2.5, color="black") +
    geom_line(inherit.aes=F,
              data=subset(perg1_regioes_trend, group == "Norte"),
              aes(x=x, y=predicted), alpha=.5,
              color="#1f9075") +
    geom_ribbon(inherit.aes=F,
                data=subset(perg1_regioes_trend, group == "Norte"),
                aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high), alpha=.05,
                fill="#1f9075", color=NA) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .25),
                       labels=paste0(seq(0, 25, 5), "%")) +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    guides(color="none") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

(
  plot_nordeste <- ggplot(subset(perg1_regioes_props, macro_reg == "Nordeste"), 
                         aes(x=ano, y=ativamente_fisico_desloc,
                             ymin=ci_l, ymax=ci_u, color=macro_reg)) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    facet_wrap(~macro_reg) +
    geom_pointrange(color="#2a487a") +
    geom_line(color="#2a487a") +
    geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
              nudge_y=.01, size=2.5, color="black") +
    geom_line(inherit.aes=F,
              data=subset(perg1_regioes_trend, group == "Nordeste"),
              aes(x=x, y=predicted), alpha=.5,
              color="#2a487a") +
    geom_ribbon(inherit.aes=F,
                data=subset(perg1_regioes_trend, group == "Nordeste"),
                aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high), alpha=.05,
                fill="#2a487a", color=NA) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .25),
                       labels=paste0(seq(0, 25, 5), "%")) +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    guides(color="none") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

(
  plot_centro <- ggplot(subset(perg1_regioes_props, macro_reg == "Centro-Oeste"), 
                        aes(x=ano, y=ativamente_fisico_desloc,
                            ymin=ci_l, ymax=ci_u, color=macro_reg)) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    facet_wrap(~macro_reg) +
    geom_pointrange(color="#361865") +
    geom_line(color="#361865") +
    geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
              nudge_y=.01, size=2.5, color="black") +
    geom_line(inherit.aes=F,
              data=subset(perg1_regioes_trend, group == "Centro-Oeste"),
              aes(x=x, y=predicted), alpha=.5,
              color="#361865") +
    geom_ribbon(inherit.aes=F,
                data=subset(perg1_regioes_trend, group == "Centro-Oeste"),
                aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high), alpha=.05,
                fill="#361865", color=NA) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .25),
                       labels=paste0(seq(0, 25, 5), "%")) +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    guides(color="none") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

(plot_geral + plot_norte + plot_nordeste + plot_centro + plot_sudeste + plot_sul) +
  plot_annotation(tag_levels = "A")

# ggsave("figuras/plot_ativos_no_deslocamento_por_regioes_spline.png", dpi=600, 
#        units="in", height=7, width=12)

# macro regioes em spline

(
  plot_sudeste_sp <- ggplot(subset(perg1_regioes_props, macro_reg == "Sudeste"), 
                         aes(x=ano, y=ativamente_fisico_desloc,
                             ymin=ci_l, ymax=ci_u, color=macro_reg)) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    facet_wrap(~macro_reg) +
    geom_pointrange(color="#bfde19") +
    geom_line(color="#bfde19") +
    geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
              nudge_y=.01, size=2.5, color="black") +
    geom_line(inherit.aes=F,
              data=subset(m2_1_spline_pred, macro_reg == "Sudeste"),
              aes(x=ano, y=response), alpha=.5,
              color="#bfde19") +
    geom_ribbon(inherit.aes=F,
                data=subset(m2_1_spline_pred, macro_reg == "Sudeste"),
                aes(x=ano, y=response, ymin=conf.low, ymax=conf.high), alpha=.15,
                fill="#bfde19", color=NA) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .25),
                       labels=paste0(seq(0, 25, 5), "%")) +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    guides(color="none") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

(
  plot_sul_sp <- ggplot(subset(perg1_regioes_props, macro_reg == "Sul"), 
                     aes(x=ano, y=ativamente_fisico_desloc,
                         ymin=ci_l, ymax=ci_u, color=macro_reg)) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    facet_wrap(~macro_reg) +
    geom_pointrange(color="#3db85a") +
    geom_line( color="#3db85a") +
    geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
              nudge_y=.01, size=2.5, color="black") +
    geom_line(inherit.aes=F,
              data=subset(m2_1_spline_pred, macro_reg == "Sul"),
              aes(x=ano, y=response), alpha=.5,
              color="#3db85a") +
    geom_ribbon(inherit.aes=F,
                data=subset(m2_1_spline_pred, macro_reg == "Sul"),
                aes(x=ano, y=response, ymin=conf.low, ymax=conf.high), alpha=.15,
                fill="#3db85a", color=NA) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .25),
                       labels=paste0(seq(0, 25, 5), "%")) +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    guides(color="none") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

(
  plot_norte_sp <- ggplot(subset(perg1_regioes_props, macro_reg == "Norte"), 
                       aes(x=ano, y=ativamente_fisico_desloc,
                           ymin=ci_l, ymax=ci_u, color=macro_reg)) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    facet_wrap(~macro_reg) +
    geom_pointrange(color="#1f9075") +
    geom_line(color="#1f9075") +
    geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
              nudge_y=.01, size=2.5, color="black") +
    geom_line(inherit.aes=F,
              data=subset(m2_1_spline_pred, macro_reg == "Norte"),
              aes(x=ano, y=response), alpha=.5,
              color="#1f9075") +
    geom_ribbon(inherit.aes=F,
                data=subset(m2_1_spline_pred, macro_reg == "Norte"),
                aes(x=ano, y=response, ymin=conf.low, ymax=conf.high), alpha=.15,
                fill="#1f9075", color=NA) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .25),
                       labels=paste0(seq(0, 25, 5), "%")) +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    guides(color="none") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

(
  plot_nordeste_sp <- ggplot(subset(perg1_regioes_props, macro_reg == "Nordeste"), 
                          aes(x=ano, y=ativamente_fisico_desloc,
                              ymin=ci_l, ymax=ci_u, color=macro_reg)) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    facet_wrap(~macro_reg) +
    geom_pointrange(color="#2a487a") +
    geom_line(color="#2a487a") +
    geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
              nudge_y=.01, size=2.5, color="black") +
    geom_line(inherit.aes=F,
              data=subset(m2_1_spline_pred, macro_reg == "Nordeste"),
              aes(x=ano, y=response), alpha=.5,
              color="#2a487a") +
    geom_ribbon(inherit.aes=F,
                data=subset(m2_1_spline_pred, macro_reg == "Nordeste"),
                aes(x=ano, y=response, ymin=conf.low, ymax=conf.high), alpha=.15,
                fill="#2a487a", color=NA) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .25),
                       labels=paste0(seq(0, 25, 5), "%")) +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    guides(color="none") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

(
  plot_centro_sp <- ggplot(subset(perg1_regioes_props, macro_reg == "Centro-Oeste"), 
                           aes(x=ano, y=ativamente_fisico_desloc,
                               ymin=ci_l, ymax=ci_u, color=macro_reg)) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    facet_wrap(~macro_reg) +
    geom_pointrange(color="#361865") +
    geom_line(color="#361865") +
    geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
              nudge_y=.01, size=2.5, color="black") +
    geom_line(inherit.aes=F,
              data=subset(m2_1_spline_pred, macro_reg == "Centro-Oeste"),
              aes(x=ano, y=response), alpha=.5,
              color="#361865") +
    geom_ribbon(inherit.aes=F,
                data=subset(m2_1_spline_pred, macro_reg == "Centro-Oeste"),
                aes(x=ano, y=response, ymin=conf.low, ymax=conf.high), alpha=.15,
                fill="#361865", color=NA) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .25),
                       labels=paste0(seq(0, 25, 5), "%")) +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    guides(color="none") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

(plot_geral_sp + plot_norte_sp + plot_nordeste_sp + plot_centro_sp + 
    plot_sudeste_sp + plot_sul_sp) +
  plot_annotation(tag_levels = "A")

# ggsave("figuras/plot_ativos_no_deslocamento_por_regioes_spline.png", dpi=600, 
#        units="in", height=7, width=12)

# ## pergunta 2) existem tendencias ao longo dos anos nos 4 grupos de atv fisica de deslocamento?
# 
# vigi_design <- update(vigi_design, ano_2 = ano/1000)
# 
# m3_prop <- svyolr(cat_ativ ~ ano_2, design=vigi_design) 
# m3_prop_piecewise <- svyolr(cat_ativ ~ bs(ano_2, knots = c(2.020, 2.021), degree=1), design=vigi_design) 
# 
# car::Anova(m3_prop_piecewise, type="II")
# 
# m3_predictions <- predict(m3_prop, type="probs") |> as_tibble() |> 
#   mutate(ano = vigi_design$variables$ano) |> 
#   group_by(ano) |> summarise(across(everything(), mean)) |> 
#   pivot_longer(2:5, names_to='categoria', values_to='prob')
# 
# m3_predictions <- predict(m3_prop_piecewise, type="probs") |> as_tibble() |> 
#   mutate(ano = vigi_design$variables$ano) |> 
#   group_by(ano) |> summarise(across(everything(), mean)) |> 
#   pivot_longer(2:5, names_to='categoria', values_to='prob')
# 
# ggplot(m3_predictions, aes(x=ano, y=prob, color=categoria)) + 
#   geom_point() +
#   geom_line() +
#   scale_x_continuous(breaks=seq(2009, 2022, 1)) +
#   theme_bw()
# 
# # medias e dps para visualizacoes
# 
# perg2_props <- svyby(
#   formula = ~cat_ativ, 
#   by = ~ano, 
#   design = vigi_design,
#   FUN = svymean, 
#   method = "logit",  # Logit transform for CI
#   vartype = "ci"     # Return confidence intervals
# )
# 
# names(perg2_props) <- c("ano", "nao_realiza", "min_0_30", "min_30_60", "min_60",
#                         "nao_realiza_conf_low", "min_0_30_conf_low", "min_30_60_conf_low", "min_60_conf_low",
#                         "nao_realiza_conf_high", "min_0_30_conf_high", "min_30_60_conf_high", "min_60_conf_high")
# 
# perg2_props_main <- perg2_props |> 
#   pivot_longer(
#     cols = starts_with("nao_realiza") | starts_with("min_"),
#     names_to = "category",
#     values_to = "proportion",
#     names_pattern = "^(.*)$"  # Match all column names
#   ) |> 
#   filter(!stringr::str_detect(category, "conf_low|conf_high")) 
# 
# perg2_props_low <- perg2_props |> 
#   pivot_longer(
#     cols = starts_with("nao_realiza") | starts_with("min_"),
#     names_to = "category",
#     values_to = "proportion",
#     names_pattern = "^(.*)$"  # Match all column names
#   ) |> 
#   filter(stringr::str_detect(category, "conf_low")) |> 
#   rename(conf_low = proportion)
# 
# perg2_props_high <- perg2_props |> 
#   pivot_longer(
#     cols = starts_with("nao_realiza") | starts_with("min_"),
#     names_to = "category",
#     values_to = "proportion",
#     names_pattern = "^(.*)$"  # Match all column names
#   ) |> 
#   filter(stringr::str_detect(category, "conf_high")) |> 
#   rename(conf_high = proportion)
# 
# perg2_props_main$conf_low <- perg2_props_low$conf_low
# perg2_props_main$conf_high <- perg2_props_high$conf_high
# 
# perg2_props_main <- perg2_props_main |> 
#   mutate(category = fct_relevel(category,
#                                 "nao_realiza",
#                                 "min_0_30",
#                                 "min_30_60",
#                                 "min_60")) |> 
#   mutate(escolaridade_cat_num = "Geral")
# 
# (
# grupos_geral <- ggplot(perg2_props_main, aes(x=ano, y=proportion, ymin=conf_low, ymax=conf_high,
#                              color=category, fill=category)) +
#   facet_wrap(~escolaridade_cat_num) +
#   geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
#   geom_pointrange(aes(shape=category), size=0.25) +
#   scale_color_manual(values=c("#3db85a", "#1f9075", "#2a487a", "#361865"),
#                      labels=c("Não realiza",
#                               ">0 e ≤30 min.",
#                               ">30 e ≤60 min.",
#                               ">60 min.")) +
#   scale_fill_manual(values=c("#3db85a", "#1f9075", "#2a487a", "#361865"),
#                     labels=c("Não realiza",
#                              ">0 e ≤30 min.",
#                              ">30 e ≤60 min.",
#                              ">60 min.")) +
#   scale_x_continuous(breaks=seq(2009, 2023, 1),
#                      labels=c(seq(2009, 2021, 1), "", "2023")) +
#   scale_y_continuous(limits=c(0, .9),
#                      labels=paste0(seq(0, 90, 10), "%"),
#                      breaks=seq(0, .9, .1)) +
#   scale_shape_manual(values=c(21, 22, 23, 24),
#                      labels=c("Não realiza",
#                               ">0 e ≤30 min.",
#                               ">30 e ≤60 min.",
#                               ">60 min.")) +
#   geom_text(aes(y=if_else(category == "min_60", conf_low - .02, conf_high + 0.015), 
#                 label=paste0(round(proportion , 2)*100, "%")),
#             size=2.5, color="black") +
#   geom_line() +
#   theme_avp() +
#   labs(x="Ano", y="Indivíduos",
#        color="Deslocamento", fill="Deslocamento", shape="Deslocamento") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1))
# )
# 
# # ggsave("figuras/plot_categorias_atividade_deslocamento.png", dpi=600, 
# #        units="in", height=4, width=6)
# 
# # ggsave("figuras/plot_categorias_atividade_deslocamento.pdf", dpi=600, 
# #        units="in", height=4, width=6, useDingbats=FALSE)
# 
# perg2_props_main |>  
#   mutate(estimate = paste0(round(proportion , 3), " (", 
#                            round(conf_low, 3), "; ", 
#                            round(conf_high,3 ), ")")) |> 
#   writexl::write_xlsx("grupos_desloc_tendencia_anual.xlsx")
# 
# ## mesma relacao mas olhando para niveis de escolaridade
# 
# m3_prop_piecewise_esc <- svyolr(cat_ativ ~ bs(ano_2, knots = c(2.020, 2.021), degree=1) *
#                               escolaridade_cat_num, design=vigi_design) 
# 
# summary(m3_prop_piecewise_esc)
# car::Anova(m3_prop_piecewise_esc, "II")

# # figura
# 
# perg2_props_esc <- svyby(
#   formula = ~cat_ativ, 
#   by = ~ ano + escolaridade_cat_num, 
#   design = vigi_design,
#   FUN = svymean, 
#   method = "logit",  # Logit transform for CI
#   vartype = "ci"     # Return confidence intervals
# )
# 
# names(perg2_props_esc) <- c("ano", "escolaridade_cat_num", "nao_realiza", "min_0_30", "min_30_60", "min_60",
#                         "nao_realiza_conf_low", "min_0_30_conf_low", "min_30_60_conf_low", "min_60_conf_low",
#                         "nao_realiza_conf_high", "min_0_30_conf_high", "min_30_60_conf_high", "min_60_conf_high")
# 
# perg2_props_main_esc <- perg2_props_esc |> 
#   pivot_longer(
#     cols = starts_with("nao_realiza") | starts_with("min_"),
#     names_to = "category",
#     values_to = "proportion",
#     names_pattern = "^(.*)$"  # Match all column names
#   ) |> 
#   filter(!stringr::str_detect(category, "conf_low|conf_high")) 
# 
# perg2_props_low_esc <- perg2_props_esc |> 
#   pivot_longer(
#     cols = starts_with("nao_realiza") | starts_with("min_"),
#     names_to = "category",
#     values_to = "proportion",
#     names_pattern = "^(.*)$"  # Match all column names
#   ) |> 
#   filter(stringr::str_detect(category, "conf_low")) |> 
#   rename(conf_low = proportion)
# 
# perg2_props_high_esc <- perg2_props_esc |> 
#   pivot_longer(
#     cols = starts_with("nao_realiza") | starts_with("min_"),
#     names_to = "category",
#     values_to = "proportion",
#     names_pattern = "^(.*)$"  # Match all column names
#   ) |> 
#   filter(stringr::str_detect(category, "conf_high")) |> 
#   rename(conf_high = proportion)
# 
# perg2_props_main_esc$conf_low <- perg2_props_low_esc$conf_low
# perg2_props_main_esc$conf_high <- perg2_props_high_esc$conf_high
# 
# perg2_props_main_esc <- perg2_props_main_esc |> 
#   mutate(category = fct_relevel(category,
#                                 "nao_realiza",
#                                 "min_0_30",
#                                 "min_30_60",
#                                 "min_60"))
# 
# ## plot por grupo
# 
# (
# grupos_0_4 <- ggplot(subset(perg2_props_main_esc,
#                             escolaridade_cat_num == "0-4 anos"), aes(x=ano, y=proportion, 
#                                                ymin=conf_low, ymax=conf_high,
#                              color=category, fill=category)) +
#   facet_wrap(~escolaridade_cat_num) +
#   geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
#   geom_pointrange(aes(shape=category), size=0.25) +
#   scale_color_manual(values=c("#3db85a", "#1f9075", "#2a487a", "#361865"),
#                      labels=c("Não realiza",
#                               ">0 e ≤30 min.",
#                               ">30 e ≤60 min.",
#                               ">60 min.")) +
#   scale_fill_manual(values=c("#3db85a", "#1f9075", "#2a487a", "#361865"),
#                     labels=c("Não realiza",
#                              ">0 e ≤30 min.",
#                              ">30 e ≤60 min.",
#                              ">60 min.")) +
#   scale_x_continuous(breaks=seq(2009, 2023, 1),
#                      labels=c(seq(2009, 2021, 1), "", "2023")) +
#   scale_y_continuous(limits=c(0, .9),
#                      labels=paste0(seq(0, 90, 10), "%"),
#                      breaks=seq(0, .9, .1)) +
#   scale_shape_manual(values=c(21, 22, 23, 24),
#                      labels=c("Não realiza",
#                               ">0 e ≤30 min.",
#                               ">30 e ≤60 min.",
#                               ">60 min.")) +
#   geom_text(aes(y=if_else(category == "min_60", conf_low - .02, conf_high + 0.015), 
#                 label=paste0(round(proportion , 2)*100, "%")),
#             size=2, color="black") +
#   geom_line() +
#   theme_avp() +
#   labs(x="Ano", y="Indivíduos",
#        color="Deslocamento", fill="Deslocamento", shape="Deslocamento") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1))
# )
# 
# (
#   grupos_5_8 <- ggplot(subset(perg2_props_main_esc,
#                               escolaridade_cat_num == "5-8 anos"), aes(x=ano, y=proportion, 
#                                                                        ymin=conf_low, ymax=conf_high,
#                                                                        color=category, fill=category)) +
#     facet_wrap(~escolaridade_cat_num) +
#     geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
#     geom_pointrange(aes(shape=category), size=0.25) +
#     scale_color_manual(values=c("#3db85a", "#1f9075", "#2a487a", "#361865"),
#                        labels=c("Não realiza",
#                                 ">0 e ≤30 min.",
#                                 ">30 e ≤60 min.",
#                                 ">60 min.")) +
#     scale_fill_manual(values=c("#3db85a", "#1f9075", "#2a487a", "#361865"),
#                       labels=c("Não realiza",
#                                ">0 e ≤30 min.",
#                                ">30 e ≤60 min.",
#                                ">60 min.")) +
#     scale_x_continuous(breaks=seq(2009, 2023, 1),
#                        labels=c(seq(2009, 2021, 1), "", "2023")) +
#     scale_y_continuous(limits=c(0, .9),
#                        labels=paste0(seq(0, 90, 10), "%"),
#                        breaks=seq(0, .9, .1)) +
#     scale_shape_manual(values=c(21, 22, 23, 24),
#                        labels=c("Não realiza",
#                                 ">0 e ≤30 min.",
#                                 ">30 e ≤60 min.",
#                                 ">60 min.")) +
#     geom_text(aes(y=if_else(category == "min_60", conf_low - .02, conf_high + 0.015), 
#                   label=paste0(round(proportion , 2)*100, "%")),
#               size=2, color="black") +
#     geom_line() +
#     theme_avp() +
#     labs(x="Ano", y="Indivíduos",
#          color="Deslocamento", fill="Deslocamento", shape="Deslocamento") +
#     theme(axis.text.x = element_text(angle = 45, hjust=1))
# )
# 
# (
#   grupos_9_11 <- ggplot(subset(perg2_props_main_esc,
#                               escolaridade_cat_num == "9-11 anos"), aes(x=ano, y=proportion, 
#                                                                        ymin=conf_low, ymax=conf_high,
#                                                                        color=category, fill=category)) +
#     facet_wrap(~escolaridade_cat_num) +
#     geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
#     geom_pointrange(aes(shape=category), size=0.25) +
#     scale_color_manual(values=c("#3db85a", "#1f9075", "#2a487a", "#361865"),
#                        labels=c("Não realiza",
#                                 ">0 e ≤30 min.",
#                                 ">30 e ≤60 min.",
#                                 ">60 min.")) +
#     scale_fill_manual(values=c("#3db85a", "#1f9075", "#2a487a", "#361865"),
#                       labels=c("Não realiza",
#                                ">0 e ≤30 min.",
#                                ">30 e ≤60 min.",
#                                ">60 min.")) +
#     scale_x_continuous(breaks=seq(2009, 2023, 1),
#                        labels=c(seq(2009, 2021, 1), "", "2023")) +
#     scale_y_continuous(limits=c(0, .9),
#                        labels=paste0(seq(0, 90, 10), "%"),
#                        breaks=seq(0, .9, .1)) +
#     scale_shape_manual(values=c(21, 22, 23, 24),
#                        labels=c("Não realiza",
#                                 ">0 e ≤30 min.",
#                                 ">30 e ≤60 min.",
#                                 ">60 min.")) +
#     geom_text(aes(y=if_else(category == "min_60", conf_low - .02, conf_high + 0.015), 
#                   label=paste0(round(proportion , 2)*100, "%")),
#               size=2, color="black") +
#     geom_line() +
#     theme_avp() +
#     labs(x="Ano", y="Indivíduos",
#          color="Deslocamento", fill="Deslocamento", shape="Deslocamento") +
#     theme(axis.text.x = element_text(angle = 45, hjust=1))
# )
# 
# (
#   grupos_12 <- ggplot(subset(perg2_props_main_esc,
#                               escolaridade_cat_num == "12 ou mais anos"), aes(x=ano, y=proportion, 
#                                                                        ymin=conf_low, ymax=conf_high,
#                                                                        color=category, fill=category)) +
#     facet_wrap(~escolaridade_cat_num) +
#     geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
#     geom_pointrange(aes(shape=category), size=0.25) +
#     scale_color_manual(values=c("#3db85a", "#1f9075", "#2a487a", "#361865"),
#                        labels=c("Não realiza",
#                                 ">0 e ≤30 min.",
#                                 ">30 e ≤60 min.",
#                                 ">60 min.")) +
#     scale_fill_manual(values=c("#3db85a", "#1f9075", "#2a487a", "#361865"),
#                       labels=c("Não realiza",
#                                ">0 e ≤30 min.",
#                                ">30 e ≤60 min.",
#                                ">60 min.")) +
#     scale_x_continuous(breaks=seq(2009, 2023, 1),
#                        labels=c(seq(2009, 2021, 1), "", "2023")) +
#     scale_y_continuous(limits=c(0, .9),
#                        labels=paste0(seq(0, 90, 10), "%"),
#                        breaks=seq(0, .9, .1)) +
#     scale_shape_manual(values=c(21, 22, 23, 24),
#                        labels=c("Não realiza",
#                                 ">0 e ≤30 min.",
#                                 ">30 e ≤60 min.",
#                                 ">60 min.")) +
#     geom_text(aes(y=if_else(category == "min_60", conf_low - .02, conf_high + 0.015), 
#                   label=paste0(round(proportion , 2)*100, "%")),
#               size=2, color="black") +
#     geom_line() +
#     theme_avp() +
#     labs(x="Ano", y="Indivíduos",
#          color="Deslocamento", fill="Deslocamento", shape="Deslocamento") +
#     theme(axis.text.x = element_text(angle = 45, hjust=1))
# )
# 
# ((grupos_0_4 + grupos_5_8) / (grupos_9_11 + grupos_12)) +
#   plot_annotation(tag_levels = "A") +
#   plot_layout(guides="collect") &
#   theme(legend.position='bottom')

#ggsave("figuras/plot_categorias_atividade_deslocamento_todos.png", dpi=600, 
#       units="in", height=8, width=8)

#ggsave("figuras/plot_categorias_atividade_deslocamento_todos.pdf", dpi=600, 
#       units="in", height=8, width=8, useDingbats=FALSE)

## perguntas adicionais

# por sexo

perg_sexo_props <- svyby(
  formula = ~ativamente_fisico_desloc, 
  by = ~ ano + sexo, 
  design = vigi_design,
  FUN = svyciprop, 
  method = "logit",  # Logit transform for CI
  vartype = "ci"     # Return confidence intervals
)

model_desloc_sexo1 <- svyglm(ativamente_fisico_desloc_num ~ ano + sexo, family = quasibinomial(), design=vigi_design)

summary(model_desloc_sexo1)
car::Anova(model_desloc_sexo1, type="II")

model_desloc_sexo2 <- svyglm(ativamente_fisico_desloc_num ~ ano * sexo, family = quasibinomial(), design=vigi_design)

summary(model_desloc_sexo2)
car::Anova(model_desloc_sexo2, type="II")

perg_sexo_trend <- ggpredict(model_desloc_sexo2, terms=c("ano [all]", "sexo")) |> as_tibble() 

model_desloc_sexo2 <- svyglm(ativamente_fisico_desloc_num ~ ns(ano, knots=c(2015, 2020, 2021))
                             * sexo, family = quasibinomial(), design=vigi_design)

car::Anova(model_desloc_sexo2, type="II")

perg_sexo_props <- perg_sexo_props |> mutate(sexo = if_else(sexo==1, "Homens", "Mulheres"))

(
  plot_sexo <- ggplot(perg_sexo_props, 
                     aes(x=ano, y=ativamente_fisico_desloc,
                         ymin=ci_l, ymax=ci_u, color=sexo)) +
    geom_line(aes(group=sexo), position=position_dodge(width=.4), alpha=.5) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    geom_pointrange(position=position_dodge(width=.4), size=.35) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .35),
                       breaks=seq(0, .35, .05),
                       labels=paste0(seq(0, 35, 5), "%")) +
    scale_color_manual(values=c('#1f78b4', '#ff7f00'),
                       name="Sexo") +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

# reporting

perg_sexo_props_report <- perg_sexo_props |> 
  mutate(across(ativamente_fisico_desloc:ci_u, .f = ~ .x*100)) |> 
  mutate(estimate = paste0(round(ativamente_fisico_desloc, 1),
                           " (", round(ci_l, 1),"; ", round(ci_u, 1),")")) |> 
  select(ano, sexo, estimate) |> 
  pivot_wider(names_from=ano, values_from=estimate)

# writexl::write_xlsx(perg_sexo_props_report, "figuras\\sexo_props.xlsx")

# por faixa etaria

vigi_design_faixa <- subset(vigi_design, !is.na(faixa_etaria))

perg_faixa_et_props <- svyby(
  formula = ~ativamente_fisico_desloc, 
  by = ~ ano + faixa_etaria, 
  design = vigi_design_faixa,
  FUN = svyciprop, 
  method = "logit",  # Logit transform for CI
  vartype = "ci"     # Return confidence intervals
)

model_desloc_faixa1 <- svyglm(ativamente_fisico_desloc_num ~ ano + faixa_etaria, family = quasibinomial(), design=vigi_design_faixa)

summary(model_desloc_faixa1)
car::Anova(model_desloc_faixa1, type="II")

model_desloc_faixa2 <- svyglm(ativamente_fisico_desloc_num ~ ano * faixa_etaria, family = quasibinomial(), design=vigi_design_faixa)

model_desloc_faixa2_sp <- svyglm(ativamente_fisico_desloc_num ~ ns(ano, knots=c(2015, 2020, 2021)) * 
                                   faixa_etaria, family = quasibinomial(), design=vigi_design_faixa)
summary(model_desloc_faixa2)
car::Anova(model_desloc_faixa2, type="II")

anova(model_desloc_faixa2, model_desloc_faixa2_sp)
AIC(model_desloc_faixa2, model_desloc_faixa2_sp)
car::Anova(model_desloc_faixa2, type="II")

model_desloc_faixa2_pred <- predict(model_desloc_faixa2_sp, type="response") |> 
  as_tibble() |> 
  mutate(ano = vigi_design_faixa$variables$ano,
         faixa_etaria = vigi_design_faixa$variables$faixa_etaria,
         conf.low = response - (SE * 1.96),
         conf.high = response + (SE * 1.96)) |> group_by(ano, faixa_etaria) |> slice(1) |> 
  ungroup()

perg_faixa_et_props <- perg_faixa_et_props |> mutate(faixa_etaria = case_when(faixa_etaria == 1 ~ "18 a 24 anos",
                                                       faixa_etaria == 2 ~ "25 a 34 anos",
                                                       faixa_etaria == 3 ~ "35 a 44 anos",
                                                       faixa_etaria == 4 ~ "45 a 54 anos",
                                                       faixa_etaria == 5 ~ "55 a 64 anos",
                                                       faixa_etaria == 6 ~ "65 anos e mais"))

(
  plot_faixa_etaria <- ggplot(perg_faixa_et_props, 
                     aes(x=ano, y=ativamente_fisico_desloc,
                         ymin=ci_l, ymax=ci_u, color=faixa_etaria)) +
    geom_line(aes(group=faixa_etaria), position=position_dodge(width=.4), alpha=.5) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    geom_pointrange(position=position_dodge(width=.4), size=.35) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .35),
                       breaks=seq(0, .35, .05),
                       labels=paste0(seq(0, 35, 5), "%")) +
    scale_color_manual(values=c("#B4DE2CFF", "#6DCD59FF", "#35B779FF", "#1F9E89FF", "#31688EFF", "#443A83FF"),
                       name="Faixa etária") +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

# reporting

perg_faixa_et_props_report <- perg_faixa_et_props |> 
  mutate(across(ativamente_fisico_desloc:ci_u, .f = ~ .x*100)) |> 
  mutate(estimate = paste0(round(ativamente_fisico_desloc, 1), 
                           " (", round(ci_l, 1),"; ", round(ci_u, 1),")")) |> 
  select(ano, faixa_etaria, estimate) |> 
  pivot_wider(names_from=ano, values_from=estimate)

# writexl::write_xlsx(perg_faixa_et_props_report, "figuras\\faixa_etaria_props.xlsx")

# escolaridade

vigi_design_esc <- subset(vigi_design, !is.na(escolaridade_cat_num))

perg_escolaridade_props <- svyby(
  formula = ~ativamente_fisico_desloc, 
  by = ~ ano + escolaridade_cat_num, 
  design = vigi_design_esc,
  FUN = svyciprop, 
  method = "logit",  # Logit transform for CI
  vartype = "ci"     # Return confidence intervals
)

ggplot(perg_escolaridade_props, aes(x=ano, y=ativamente_fisico_desloc,
                                    color=escolaridade_cat_num)) + geom_point() + geom_line()

model_esc1 <- svyglm(ativamente_fisico_desloc_num ~ ano + escolaridade_cat_num, family = quasibinomial(), design=vigi_design_esc)

summary(model_esc1)
car::Anova(model_esc1, type="II")

model_esc2 <- svyglm(ativamente_fisico_desloc_num ~ ano * escolaridade_cat_num, family = quasibinomial(), design=vigi_design_esc)

summary(model_esc2)
car::Anova(model_esc2, type="II")

model_esc2_sp <- svyglm(ativamente_fisico_desloc_num ~ ns(ano, knots=c(2015, 2020, 2021)) * 
                          escolaridade_cat_num, family = quasibinomial(), design=vigi_design_esc)

AIC(model_esc1, model_esc2, model_esc2_sp)
anova(model_esc2, model_esc2_sp)

model_esc2_sp_pred <- predict(model_esc2_sp, type="response") |> 
  as_tibble() |> 
  mutate(ano = vigi_design_esc$variables$ano,
         escolaridade_cat_num = vigi_design_esc$variables$escolaridade_cat_num,
         conf.low = response - (SE * 1.96),
         conf.high = response + (SE * 1.96)) |> group_by(ano, escolaridade_cat_num) |> slice(1) |> 
  ungroup()

ggplot(model_esc2_sp_pred, aes(x=ano, y=response, ymin=conf.low, ymax=conf.high, color=escolaridade_cat_num,
                               fill=escolaridade_cat_num)) +
  geom_line() +
  geom_ribbon(alpha=.25)

perg_escolaridade_props <- perg_escolaridade_props |> 
  mutate(escolaridade_cat_num = if_else(escolaridade_cat_num == "12 ou mais anos", "12 anos e mais", escolaridade_cat_num)) |> 
  mutate(escolaridade_cat_num = fct_relevel(
  escolaridade_cat_num, "0-4 anos", "5-8 anos", "9-11 anos", "12 anos e mais"
))

(
  plot_escolaridade <- ggplot(perg_escolaridade_props, 
                     aes(x=ano, y=ativamente_fisico_desloc,
                         ymin=ci_l, ymax=ci_u, color=escolaridade_cat_num)) +
    geom_line(aes(group=escolaridade_cat_num), position=position_dodge(width=.4), alpha=.5) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    geom_pointrange(position=position_dodge(width=.4), size=.35) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .35),
                       breaks=seq(0, .35, .05),
                       labels=paste0(seq(0, 35, 5), "%")) +
    scale_color_manual(values=c('#1f78b4', '#ff7f00', '#33a02c', '#6a3d9a'),
                       name="Escolaridade") +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

# reporting

perg_escolaridade_props_report <- perg_escolaridade_props |> 
  mutate(across(ativamente_fisico_desloc:ci_u, .f = ~ .x*100)) |> 
  mutate(estimate = paste0(round(ativamente_fisico_desloc,1 ),
                           " (", round(ci_l, 1),"; ", round(ci_u, 1),")")) |> 
  select(ano, escolaridade_cat_num, estimate) |> 
  pivot_wider(names_from=ano, values_from=estimate)

# writexl::write_xlsx(perg_escolaridade_props_report, "figuras\\escolaridade_props.xlsx")

# por cor/raça

# tirar não respondentes, indigenas e amarelos

vigi_design_cor <- subset(vigi_design, cor != "nao_respondeu")
vigi_design_cor <- subset(vigi_design_cor, cor != "nao_sabe")
vigi_design_cor <- subset(vigi_design_cor, cor != "indigena")
vigi_design_cor <- subset(vigi_design_cor, cor != "amarela")

perg_cor_props <- svyby(
  formula = ~ativamente_fisico_desloc, 
  by = ~ ano + cor, 
  design = vigi_design_cor,
  FUN = svyciprop, 
  method = "logit",  # Logit transform for CI
  vartype = "ci"     # Return confidence intervals
)

model_desloc_cor1 <- svyglm(ativamente_fisico_desloc_num ~ ano + cor, family = quasibinomial(), design=vigi_design_cor)

summary(model_desloc_cor1)
car::Anova(model_desloc_cor1, type="II")

model_desloc_cor2_linear <- svyglm(ativamente_fisico_desloc_num ~ ano * cor, family = quasibinomial(), design=vigi_design_cor)

summary(model_desloc_cor2_linear)
car::Anova(model_desloc_cor2_linear, type="II")

model_desloc_cor2_spline <- svyglm(ativamente_fisico_desloc_num ~ ns(ano, knots=c(2015, 2020, 2021))
                             * cor, family = quasibinomial(), design=vigi_design_cor)

summary(model_desloc_cor2_spline)
car::Anova(model_desloc_cor2_spline, type="II")

AIC(model_desloc_cor2_linear, model_desloc_cor2_spline)

anova(model_desloc_cor2_linear, model_desloc_cor2_spline)

perg_cor_trend <- predict(model_desloc_cor2_spline, type="response") |> 
  as_tibble() |> 
  mutate(ano = vigi_design_cor$variables$ano,
         cor = vigi_design_cor$variables$cor,
         conf.low = response - (SE * 1.96),
         conf.high = response + (SE * 1.96)) |> group_by(ano, cor) |> slice(1) |> 
  ungroup() 

# perg_cor_props_editado <- perg_cor_props |> filter(cor != 'indigena') |> 
#   filter(!(ano == 2009 & cor == "parda")) |> 
#   filter(!(ano == 2010 & cor == "parda")) 

(
  plot_cor <- ggplot(perg_cor_props, aes(x=ano, y=ativamente_fisico_desloc,
                          ymin=ci_l, ymax=ci_u, color=cor),
                      fill=cor) +
    geom_line(aes(group=cor), position=position_dodge(width=.4), alpha=.5) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    geom_pointrange(position=position_dodge(width=.4), size=.35) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .35),
                       breaks=seq(0, .35, .05),
                       labels=paste0(seq(0, 35, 5), "%")) +
    scale_color_manual(values=c('#1f78b4', '#ff7f00', '#33a02c'),
                       name="Cor", labels=c("Branca", "Parda", "Preta")) +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

# reporting

perg_cor_props_report <- perg_cor_props |>
  mutate(across(ativamente_fisico_desloc:ci_u, .f = ~ .x*100)) |>
  mutate(estimate = paste0(round(ativamente_fisico_desloc, 1),
                           " (", round(ci_l, 1),"; ", round(ci_u, 1),")")) |>
  select(ano, cor, estimate) |>
  pivot_wider(names_from=ano, values_from=estimate)

#writexl::write_xlsx(perg_cor_props_report, "figuras\\cor_props.xlsx")

# entire panel

(plot_sexo + plot_faixa_etaria + plot_escolaridade + plot_cor) +
  plot_annotation(tag_levels = "A")

#ggsave("figuras/plot_ativos_variaveis_sociodemograficas.png", dpi=600,
#      units="in", height=7, width=11)

# figura raça/cor com todas as categorias, menos os que não responderam.

vigi_design_cor_2 <- subset(vigi_design, cor != "nao_respondeu")
vigi_design_cor_2 <- subset(vigi_design_cor_2, cor != "nao_sabe")

perg_cor_props_2 <- svyby(
  formula = ~ativamente_fisico_desloc, 
  by = ~ ano + cor, 
  design = vigi_design_cor_2,
  FUN = svyciprop, 
  method = "logit",  # Logit transform for CI
  vartype = "ci"     # Return confidence intervals
)

perg_cor_props_2 <- perg_cor_props_2 |> 
  mutate(cor = case_when(cor == "branca" ~ "Branca",
                         cor == "amarela" ~ "Amarela",
                         cor == "preta" ~ "Preta",
                         cor == "parda" ~ "Parda",
                         cor == "indigena" ~ "Indígena"),
         cor = fct_relevel(cor, "Branca", "Preta", "Parda", "Indígena", "Amarela"))

(
  plot_cor <- ggplot(perg_cor_props_2, aes(x=ano, y=ativamente_fisico_desloc,
                                         ymin=ci_l, ymax=ci_u, color=cor),
                     fill=cor) +
    facet_wrap(~cor) +
    geom_line(aes(group=cor), position=position_dodge(width=.4), alpha=.5) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    geom_pointrange(position=position_dodge(width=.4), size=.35) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .5),
                       breaks=seq(0, .5, .05),
                       labels=paste0(seq(0, 50, 5), "%")) +
    scale_color_manual(values=c('#1f78b4', '#ff7f00', '#33a02c', '#6a3d9a', "red"),
                       name="Cor") +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

#ggsave("figuras/plot_suplementar_todas_racas.png", dpi=600,
#      units="in", height=6, width=9)

# tabela 1

dat <- vigi_design$variables |> 
  select(ano, peso_kg, altura_cm,  sexo, faixa_etaria, cor, 
         escolaridade_cat_num, macro_reg, trabalha, 
         faz_curso_escola, anda_a_pe, faz_trajeto_a_pe, ativamente_fisico_desloc_num, 
         duracao_desloc)

table_1_total <- dat |> 
  tbl_summary(by="ano",
              statistic = list(all_continuous() ~ "{mean} ({sd})"))

#table_1_total |> as_hux_xlsx("figuras/tabela_1.xlsx") 

table_1_total_svy <- vigi_table_design |> 
  tbl_svysummary(by="ano",
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p} %"))

#table_1_total_svy |> as_hux_xlsx("figuras/tabela_1_pesada.xlsx") 

################################################################################