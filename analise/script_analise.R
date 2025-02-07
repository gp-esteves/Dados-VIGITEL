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
  mutate(trajeto_trabalho = case_when(q47 == 2 ~ "não",
                                      q50 %in% c(1, 2) ~ "sim",
                                      q50 == 3 ~ "não"),
         
         duracao_trajeto_trabalho = case_when(trajeto_trabalho == "não" ~ 0,
                                              TRUE ~ q51),
         
         trajeto_curso = case_when(q52 == 888 ~ "não",
                                   q52 == 2 ~ "não",
                                   q53 == 3 ~ "não",
                                   q53 %in% c(1, 2) ~ "sim"),
         
         duracao_trajeto_curso = case_when(trajeto_curso == "não" ~ 0,
                                           TRUE ~ q54),
         
         soma_ambos_trajetos = duracao_trajeto_trabalho + duracao_trajeto_curso,
         
         ativamente_fisico_desloc = if_else(soma_ambos_trajetos >= 4, "sim", "não"),
         ativamente_fisico_desloc_num = if_else(soma_ambos_trajetos >= 4, 1, 0),
         ativamente_fisico_desloc_novo = case_when(
           
           duracao_trajeto_trabalho == 0 & duracao_trajeto_curso >= 4 ~ "sim",
           duracao_trajeto_trabalho == 0 & duracao_trajeto_curso < 4 ~ "não",
           
           duracao_trajeto_trabalho == 1 & duracao_trajeto_curso >= 3 ~ "sim", # polemico?
           duracao_trajeto_trabalho == 1 & duracao_trajeto_curso < 3 ~ "não",
           
           duracao_trajeto_trabalho == 2 & duracao_trajeto_curso >= 2 ~ "sim",
           duracao_trajeto_trabalho == 2 & duracao_trajeto_curso < 2 ~ "não",
           
           duracao_trajeto_trabalho == 3 & duracao_trajeto_curso >= 1 ~ "sim",
           duracao_trajeto_trabalho == 3 & duracao_trajeto_curso < 1 ~ "não",
           
           ##
           
           duracao_trajeto_curso == 0 & duracao_trajeto_trabalho >= 4 ~ "sim",
           duracao_trajeto_curso == 0 & duracao_trajeto_trabalho < 4 ~ "não",
           
           duracao_trajeto_curso == 1 & duracao_trajeto_trabalho >= 3 ~ "sim", # polemico?
           duracao_trajeto_curso == 1 & duracao_trajeto_trabalho < 3 ~ "não",
           
           duracao_trajeto_curso == 2 & duracao_trajeto_trabalho >= 2 ~ "sim",
           duracao_trajeto_curso == 2 & duracao_trajeto_trabalho < 2 ~ "não",
           
           duracao_trajeto_curso == 3 & duracao_trajeto_trabalho >= 1 ~ "sim",
           duracao_trajeto_curso == 3 & duracao_trajeto_trabalho < 1 ~ "não",
           
           ##
           
           duracao_trajeto_trabalho >= 4 ~ "sim",
           duracao_trajeto_curso >= 4 ~ "sim"),
         
         cat_ativ = case_when(soma_ambos_trajetos == 0 ~ "Não realiza",
                              soma_ambos_trajetos > 0 & soma_ambos_trajetos < 4 ~ ">0 e ≤30 minutos",
                              soma_ambos_trajetos >= 4 & soma_ambos_trajetos < 7 ~ ">30 e ≤60 minutos",
                              soma_ambos_trajetos >= 7 ~ ">60 minutos"),
         
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
                                        ">60 minutos"))) |> 
  rename(ano = ano, idade = q6, sexo = q7, faixa_etaria = fet, escolaridade = fesc, 
         grau_escolar = q8a, peso_kg = q9, altura_cm = q11, faz_exercicio = q42, 
         trabalha = q47, anda_a_pe = q48, carrega_peso = q49, faz_trajeto_a_pe = q50, 
         faz_curso_escola = q52, fumante = q60, ex_fumante = q64, cor = q69,
         estado_saude = q74, pressao_alta = q75, diabetes = q76, colesterol_trig = q78,
         posse_plano = q88) |> 
  mutate(across(c(faixa_etaria, escolaridade, grau_escolar, faz_exercicio, trabalha,
                  anda_a_pe, carrega_peso, faz_trajeto_a_pe, faz_curso_escola,
                  fumante, ex_fumante, cor, estado_saude, pressao_alta, diabetes,
                  colesterol_trig, posse_plano), as.factor))

vigitel |> group_by(ano) |> count()

##

vigi_design <- svydesign(id = ~ 1, 
                         strata = NULL,
                         weights= ~ pesorake,
                         data=vigitel)

vigi_design <- subset(vigi_design, !is.na(ativamente_fisico_desloc_num))
vigi_design <- subset(vigi_design, !is.na(soma_ambos_trajetos))

## pergunta 1) existe tendencias ao longo dos anos no % de indivíduos que atingem >150 min?
# qual a influencia das macroregioes?

# modelo

m1 <- svyglm(ativamente_fisico_desloc_num ~ ano, family = quasibinomial(), design=vigi_design)
summary(m1)
tbl_regression(m1, exponentiate=T)


m2 <- svyglm(ativamente_fisico_desloc_num ~ ano + macro_reg, family = quasibinomial(), design=vigi_design)
summary(m2)
tbl_regression(m2, exponentiate=T)

car::Anova(m2, type="II")

m2_1 <- svyglm(ativamente_fisico_desloc_num ~ ano * macro_reg, family = quasibinomial(), design=vigi_design)
summary(m2_1)
tbl_regression(m2_1, exponentiate=T)

car::Anova(m2_1, type="II")

ggpredict(m1, terms=c("ano [all]")) |> as_tibble() |> 
  ggplot(aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, color=group)) +
  geom_line() +
  geom_ribbon(alpha=.20, color=NA) +
  geom_pointrange() +
  scale_x_continuous(breaks=seq(2009, 2022, 1)) +
  #scale_color_manual(labels=c("1" = "Homens",
  #                           "2" = "Mulheres"),
  #                 values=c("darkblue", "orange")) +
  labs(x="Anos", y="Tempo de deslocamento diário ao trabalho") +
  theme_bw()

ggpredict(m2, terms=c("ano [all]", "macro_reg")) |> as_tibble() |> 
  ggplot(aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, color=group)) +
  geom_line() +
  #geom_ribbon(alpha=.20, color=NA) +
  geom_pointrange() +
  scale_x_continuous(breaks=seq(2009, 2022, 1)) +
  #scale_color_manual(labels=c("1" = "Homens",
  #                           "2" = "Mulheres"),
  #                 values=c("darkblue", "orange")) +
  labs(x="Anos", y="Tempo de deslocamento diário ao trabalho") +
  theme_bw()

# avalicao dos residuos

# vigitel_m <- vigi_design$variables
# 
# vigitel_m$residuos_m1 <- resid(m1)
# vigitel_m$residuos_m2 = resid(m2)
# 
# ggplot(vigitel_m, aes(x=ano, y=residuos_m1)) + geom_point(shape=".")

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
  mutate(macro_reg = "Geral")

perg1_trend <- ggpredict(m1, terms=c("ano [all]")) |> as_tibble() 

# modelo PW

perg1_props

m1_PW <- prais_winsten(
  ativamente_fisico_desloc ~ ano,
  data=perg1_props,
  index='ano')

summary(m1_PW)

(
plot_geral <- ggplot(perg1_props, aes(x=ano, y=ativamente_fisico_desloc,
                        ymin=ci_l, ymax=ci_u)) +
  geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
  facet_wrap(~macro_reg) +
  geom_pointrange() +
  geom_line() +
  geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
            nudge_y=.01, size=3) +
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

# macroregioes

perg1_regioes_props <- svyby(
  formula = ~ativamente_fisico_desloc, 
  by = ~ano + macro_reg, 
  design = vigi_design,
  FUN = svyciprop, 
  method = "logit",  # Logit transform for CI
  vartype = "ci"     # Return confidence intervals
)

perg1_regioes_props <- perg1_regioes_props |> as_tibble()

perg1_regioes_trend <- ggpredict(m2, terms=c("ano [all]", "macro_reg")) |> as_tibble() 

# modelo PW

m1_macroreg_PW <- prais_winsten(
  ativamente_fisico_desloc ~ ano + macro_reg,
  data=perg1_regioes_props,
  index='ano')

summary(m1_macroreg_PW)

m1_macroreg_PW_int <- prais_winsten(
  ativamente_fisico_desloc ~ ano * macro_reg,
  data=perg1_regioes_props,
  index='ano')

summary(m1_macroreg_PW_int)

(
plot_sudeste <- ggplot(subset(perg1_regioes_props, macro_reg == "Sudeste"), 
                       aes(x=ano, y=ativamente_fisico_desloc,
                        ymin=ci_l, ymax=ci_u, color=macro_reg)) +
  geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
  facet_wrap(~macro_reg) +
  geom_pointrange(color="#bfde19") +
  geom_line(color="#bfde19") +
  geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
            nudge_y=.01, size=3, color="black") +
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
              nudge_y=.01, size=3, color="black") +
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
              nudge_y=.01, size=3, color="black") +
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
              nudge_y=.01, size=3, color="black") +
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
              nudge_y=.01, size=3, color="black") +
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

ggsave("figuras/plot_ativos_no_deslocamento_por_regioes.png", dpi=600, 
       units="in", height=7, width=12)

## pergunta 2) existem tendencias ao longo dos anos nos 4 grupos de atv fisica de deslocamento?

m3_prop <- svy_vglm(cat_ativ ~ ano, family=propodds(), design=vigi_design) 

summary(m3_prop)

m3_predictions <- predict(m3_prop$fit, type="response") |> as_tibble() |> 
  mutate(ano = vigi_design$variables$ano) |> 
  group_by(ano) |> summarise(across(everything(), mean)) |> 
  pivot_longer(2:5, names_to='categoria', values_to='prob')

ggplot(m3_predictions, aes(x=ano, y=prob, color=categoria)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=seq(2009, 2022, 1)) +
  theme_bw()

# medias e dps para visualizacoes

perg2_props <- svyby(
  formula = ~cat_ativ, 
  by = ~ano, 
  design = vigi_design,
  FUN = svymean, 
  method = "logit",  # Logit transform for CI
  vartype = "ci"     # Return confidence intervals
)

names(perg2_props) <- c("ano", "nao_realiza", "min_0_30", "min_30_60", "min_60",
                        "nao_realiza_conf_low", "min_0_30_conf_low", "min_30_60_conf_low", "min_60_conf_low",
                        "nao_realiza_conf_high", "min_0_30_conf_high", "min_30_60_conf_high", "min_60_conf_high")

perg2_props_main <- perg2_props |> 
  pivot_longer(
    cols = starts_with("nao_realiza") | starts_with("min_"),
    names_to = "category",
    values_to = "proportion",
    names_pattern = "^(.*)$"  # Match all column names
  ) |> 
  filter(!stringr::str_detect(category, "conf_low|conf_high")) 

perg2_props_low <- perg2_props |> 
  pivot_longer(
    cols = starts_with("nao_realiza") | starts_with("min_"),
    names_to = "category",
    values_to = "proportion",
    names_pattern = "^(.*)$"  # Match all column names
  ) |> 
  filter(stringr::str_detect(category, "conf_low")) |> 
  rename(conf_low = proportion)

perg2_props_high <- perg2_props |> 
  pivot_longer(
    cols = starts_with("nao_realiza") | starts_with("min_"),
    names_to = "category",
    values_to = "proportion",
    names_pattern = "^(.*)$"  # Match all column names
  ) |> 
  filter(stringr::str_detect(category, "conf_high")) |> 
  rename(conf_high = proportion)

perg2_props_main$conf_low <- perg2_props_low$conf_low
perg2_props_main$conf_high <- perg2_props_high$conf_high

perg2_props_main <- perg2_props_main |> 
  mutate(category = fct_relevel(category,
                                "nao_realiza",
                                "min_0_30",
                                "min_30_60",
                                "min_60"))

ggplot(perg2_props_main, aes(x=ano, y=proportion, ymin=conf_low, ymax=conf_high,
                             color=category, fill=category)) +
  geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
  geom_pointrange(aes(shape=category)) +
  scale_color_manual(values=c("#3db85a", "#1f9075", "#2a487a", "#361865"),
                     labels=c("Não realiza",
                              ">0 e ≤30 min.",
                              ">30 e ≤60 min.",
                              ">60 min.")) +
  scale_fill_manual(values=c("#3db85a", "#1f9075", "#2a487a", "#361865"),
                    labels=c("Não realiza",
                             ">0 e ≤30 min.",
                             ">30 e ≤60 min.",
                             ">60 min.")) +
  scale_x_continuous(breaks=seq(2009, 2023, 1),
                     labels=c(seq(2009, 2021, 1), "", "2023")) +
  scale_y_continuous(limits=c(0, .82),
                     labels=paste0(seq(0, 80, 10), "%"),
                     breaks=seq(0, .8, .1)) +
  scale_shape_manual(values=c(21, 22, 23, 24),
                     labels=c("Não realiza",
                              ">0 e ≤30 min.",
                              ">30 e ≤60 min.",
                              ">60 min.")) +
  geom_text(aes(y=if_else(category == "min_60", conf_low - .02, conf_high + 0.015), 
                label=paste0(round(proportion , 2)*100, "%")),
            size=2.5, color="black") +
  geom_line() +
  theme_avp() +
  labs(x="Ano", y="Porcentagem de indivíduos",
       color="Deslocamento", fill="Deslocamento", shape="Deslocamento") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggsave("figuras/plot_categorias_atividade_deslocamento.png", dpi=600, 
       units="in", height=4, width=6)

## perguntas adicionais??

# por sexo

perg_sexo_props <- svyby(
  formula = ~ativamente_fisico_desloc, 
  by = ~ano + sexo, 
  design = vigi_design,
  FUN = svyciprop, 
  method = "logit",  # Logit transform for CI
  vartype = "ci"     # Return confidence intervals
)

(
  plot_centro <- ggplot(perg_sexo_props, 
                        aes(x=ano, y=ativamente_fisico_desloc,
                            ymin=ci_l, ymax=ci_u, color=as.factor(sexo))) +
    geom_vline(xintercept=2020, linetype="dashed", alpha=.5) +
    geom_pointrange() +
    geom_line(aes(group=as.factor(sexo))) +
    geom_text(aes(y=ci_u, label=paste0(round(ativamente_fisico_desloc, 2)*100, "%")),
              nudge_y=.01, size=3) +
    # geom_line(inherit.aes=F,
    #           data=subset(perg1_regioes_trend, group == "Centro-Oeste"),
    #           aes(x=x, y=predicted), alpha=.5) +
    # geom_ribbon(inherit.aes=F,
    #             data=subset(perg1_regioes_trend, group == "Centro-Oeste"),
    #             aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high), alpha=.05,
    #             fill="#361865", color=NA) +
    scale_x_continuous(breaks=seq(2009, 2023, 1),
                       labels=c(seq(2009, 2021, 1), "", "2023")) +
    scale_y_continuous(limits=c(0, .25),
                       labels=paste0(seq(0, 25, 5), "%")) +
    labs(x="Ano", y="Indivíduos ativos no deslocamento") +
    guides(color="none") +
    theme_avp() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
)

# por faixa etaria

perg_faixa_et_props <- svyby(
  formula = ~ativamente_fisico_desloc, 
  by = ~ano + sexo, 
  design = vigi_design,
  FUN = svymean, 
  method = "logit",  # Logit transform for CI
  vartype = "ci"     # Return confidence intervals
)

# tabela 1

table_1_total <- vigi_design$variables |> select(ano, faixa_etaria, sexo, escolaridade, macro_reg, 
                                peso_kg, altura_cm, cor, trabalha, faz_exercicio, 
                                carrega_peso, anda_a_pe, faz_trajeto_a_pe, 
                                faz_curso_escola, fumante, pressao_alta, diabetes, 
                                colesterol_trig) |> 
  tbl_summary(by="ano")

table_1_total |> as_hux_xlsx("figuras/tabela_1.xlsx") 

##