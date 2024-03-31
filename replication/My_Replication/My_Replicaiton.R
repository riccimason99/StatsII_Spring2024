# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()
## load required packages
## note: you can install packages from CRAN
## using install.packages("name_of_package")
install.packages("rio")
library(rio)
library(tidyverse)
install.packages("ggthemes")
library(ggthemes)
install.packages("cem")
library(cem)
install.packages("lubridate")
library(lubridate)
install.packages("rms")
library(rms)
install.packages("lme4")
library(lme4)
install.packages("texreg")
library(texreg)
install.packages("effects")
library(effects)
install.packages("ggeffects")
library(ggeffects)
install.packages("nnet")
library(nnet)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("stargazer")
library(stargazer)



## This script was run successfully with the following versions 
## of the packages above:

# attached base packages:
#     [1] tcltk     stats     graphics  grDevices utils     datasets  methods  
# [8] base     
# 
# other attached packages:
#     [1] nnet_7.3-12     ggeffects_0.3.2 effects_4.0-2   carData_3.0-1  
# [5] texreg_1.36.23  lme4_1.1-18-1   Matrix_1.2-14   rms_5.1-2      
# [9] SparseM_1.77    Hmisc_4.1-1     Formula_1.2-2   survival_2.41-3
# [13] lubridate_1.7.4 cem_1.1.19      lattice_0.20-35 ggthemes_3.4.2 
# [17] forcats_0.3.0   stringr_1.3.1   dplyr_0.7.6     purrr_0.2.5    
# [21] readr_1.1.1     tidyr_0.8.1     tibble_1.4.2    ggplot2_3.0.0  
# [25] tidyverse_1.2.1 rio_0.5.10     

## select plotting scheme
install.packages("devtools")
devtools::install_version("Matrix", version = "1.2-14")
devtools::install_version("lme4", version = "1.1-18-1")



theme_set(ggthemes::theme_few())

## load raw data and combine to one data frame

data_raw_football <- rio::import("/Users/riccimason99/Downloads/gaa_results_football.xlsx") %>% 
  mutate(sport = "Gaelic Football")
data_raw_hurling <- rio::import("/Users/riccimason99/Downloads/gaa_results_hurling.xlsx") %>% 
  mutate(sport = "Hurling")
# Combinde the data
data_raw <- bind_rows(data_raw_football, data_raw_hurling) 

data_select <- data_raw %>% 
    select(Date:Margin, sport) %>% 
    mutate(game_id = paste(Date, Team_1, Team_2, sep = "_")) %>% 
    mutate(result = if_else(Sc_1 > Sc_2, "Team 1 Won",
                            if_else(Sc_1 == Sc_2, "Draw", 
                                    "Team 2 Won")))

## transform data to long format

data_long <- data_select %>% 
    select(-c(P_1, P_2, G_1, G_2)) %>% 
    gather(key = score_type, value = score_team, -c(game_id, sport, Date, result, Grade, Team_1, Team_2, Elo_1, Elo_2, Home, Margin)) %>% 
    arrange(game_id)

## create dummy for competition and recode ground 
data_long_modified <- data_long %>% 
    mutate(competition_dummy = if_else(Grade %in% c("All-Ireland", "Provincial", "Qualifier"), "Championship", "Other")) %>% 
    mutate(match_place = if_else(Home == "Y", "Home/Away", "Neutral")) %>% 
    mutate(team = if_else(score_type == "Sc_1", Team_1, Team_2)) %>% 
    mutate(elo_team = if_else(score_type == "Sc_1", Elo_1, Elo_2)) %>% 
    mutate(elo_diff_team_1 = Elo_1 - Elo_2) %>% 
    mutate(elo_diff_team_2 = Elo_2 - Elo_1) %>% 
    mutate(elo_diff = if_else(score_type == "Sc_1", elo_diff_team_1, elo_diff_team_2)) %>% 
    mutate(result = if_else(result == "Team 1 Won" & score_type == "Sc_1", "winner",
                            if_else(result == "Team 2 Won" & score_type == "Sc_2", "winner",
                                    if_else(result == "Team 1 Won" & score_type == "Sc_2", "loser",
                                            if_else(result == "Team 2 Won" & score_type == "Sc_1", "loser", "draw"))))) %>% 
    mutate(match_place = if_else(score_type == "Sc_1" & Home == "Y", "Home",
                                 if_else(score_type == "Sc_2" & Home == "Y", "Away", "Neutral Ground"))) %>% 
    mutate(Elo_1_no_home_adv = if_else(Home == "Y", Elo_1 - 100, Elo_1)) %>% 
    mutate(elo_adjusted_diff_team_1 = Elo_1_no_home_adv - Elo_2) %>% 
    mutate(elo_adjusted_diff_team_2 = Elo_2 - Elo_1_no_home_adv) %>% 
    mutate(elo_diff_adjusted = if_else(score_type == "Sc_1", elo_adjusted_diff_team_1, elo_adjusted_diff_team_2)) %>% 
    mutate(competition_dummy_factor = as.factor(competition_dummy))

## recode result to numberic and transform some variables to factors

data_long_dummy <- data_long_modified %>% 
    mutate(result_dummy = if_else(result == "winner", 1, 0)) %>% 
    mutate(result_num = if_else(result == "winner", 1, 
                                if_else(result == "loser", 0, 0.5))) %>% 
    mutate(result = as.factor(result)) %>% 
    mutate(game_id = as.factor(game_id),
           team = as.factor(team),
           match_place = as.factor(match_place), 
           sport = as.factor(sport),
           year = as.factor(lubridate::year(Date))) %>% 
    mutate(Grade = as.factor(Grade)) %>% 
    mutate(elo_diff_log = log(elo_diff + 1)) %>% 
    mutate(elo_diff_adjusted_log = log(elo_diff_adjusted + 1))  

View(data_long_dummy)

## only keep the favourite team for each match in the dataset
data_long_one_team <- data_long_dummy %>% 
    group_by(game_id) %>% 
    arrange(-elo_diff_adjusted) %>% 
    filter(row_number() == 1) %>% 
    mutate(team_sport = as.factor(paste(team, sport, sep = "_")))


data_long_one_team$team_sport <- as.factor(data_long_one_team$team_sport)

## Calculate home advantage (with bootstrapped confidence intervals) ----

cases_home_advantage <- data_long_dummy %>%
    filter(match_place != "Neutral Ground") %>% 
    filter(match_place == "Home") %>% 
    group_by(sport, competition_dummy) %>% 
    count() 

boot_home_advantage <- data_long_dummy %>%
    filter(match_place != "Neutral Ground") %>% 
    ungroup() %>% 
    group_by(sport, competition_dummy, match_place) %>%
    do(data.frame(rbind(smean.cl.boot(.$result_num, na.rm = TRUE)))) %>% 
    filter(match_place == "Home") %>% 
    mutate(label_axis = paste(sport, competition_dummy, sep = ": "))

boot_home_advantage <- boot_home_advantage %>% 
    bind_cols(cases_home_advantage)

View(boot_home_advantage)
# Rename 'competition_dummy...2' to 'competition_dummy'
names(boot_home_advantage)[names(boot_home_advantage) == "competition_dummy...2"] <- "competition_dummy"
names(boot_home_advantage)


# Optionally remove 'competition_dummy...9' if it's redundant
boot_home_advantage$competition_dummy...9 <- NULL


## Figure 1 ----
fig_1 <- ggplot(data = boot_home_advantage, aes(x = sport...8, 
                             y = Mean, ymin = Lower, ymax = Upper)) +
    geom_pointrange(position = position_dodge(width = 0.2)) +
    coord_flip() + 
    facet_wrap(~competition_dummy, scales = "free_y") +
    geom_text(aes(label = n), nudge_x = 0.25, colour = "grey50", size = 3) +
    labs(x = NULL, y = "Home advantage") + 
    geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted") +
    scale_y_continuous(limits = c(0.4, 0.8), breaks = c(seq(0.4, 0.8, 0.1))) + 
    theme(axis.text = element_text(colour = "black"),
          strip.text = element_text(size = 11, face = "bold"))

fig_1
## Run regression models for paper

data_long_one_team_dummy <- data_long_one_team %>% 
    mutate(team_sport = paste(team, sport, sep = "_")) %>% 
    filter(result %in% c("winner", "loser")) %>% 
    mutate(team_sport = as.factor(team_sport))
summary(data_long_one_team_dummy)
View(data_long_one_team_dummy)




## Model 1 (Table 1) ----
glmer_total <- glmer(result_dummy ~  competition_dummy_factor * match_place *
                         sport + 
                         elo_diff_adjusted_log +year + (1 | team_sport),
                     family = binomial,  
                     data = data_long_one_team_dummy)
summary(glmer_total)

stargazer(glmer_total, type = "latex")


########################################################################
# TWIST
###########
# list of teams in northern Ireland
values_to_keep <- c("Antrim", "Armagh", "Derry", "Down", "Fermanagh", "Tyrone")

# subset teams that are in the north
north_df <- data_long_one_team_dummy %>%
  filter(Team_1 %in% values_to_keep | Team_2 %in% values_to_keep)
View(north_df)
# remove games where two northern Ireland teams play each other
north_df <- north_df %>%
  filter(!(Team_1 %in% values_to_keep & Team_2 %in% values_to_keep))

# run model on football and hurling teams in north
twist <- glmer(result_dummy ~  competition_dummy_factor * match_place *
                 sport + 
                 elo_diff_adjusted_log +year + (1 | team_sport),
               family = binomial,  
               data = north_df)

# run model on football teams in north
twist_football <- glmer(result_dummy ~ 
                          elo_diff_adjusted_log + 
                          competition_dummy_factor * match_place +  
                          year + (1 | team),
                        family = binomial(link="logit"),  
                        data = filter(north_df,
                                      sport == "Gaelic Football"))


# run model on football teams in north
twist_hurling <- glmer(result_dummy ~ 
                          elo_diff_adjusted_log + 
                          competition_dummy_factor * match_place +  
                          year + (1 | team),
                        family = binomial(link="logit"),  
                        data = filter(north_df,
                                      sport == "Hurling"))


# New Table
htmlreg(list(twist, twist_football, twist_hurling),
        custom.model.names = c("(1) Combined", "(2) Football", "(3) Hurling"),
        custom.coef.names = coef_names, single.row = TRUE,
        include.aic=FALSE, include.bic=FALSE, include.variance=FALSE,
        custom.gof.names = c("Log likelihood", "N", "N (Team/Sport)", "N (Team)"),
        file = "2_twist_table.html")

# OG Table
htmlreg(list(twist, glmer_football, glmer_hurling),
        custom.model.names = c("(1) Combined", "(2) Football", "(3) Hurling"),
        custom.coef.names = coef_names, single.row = TRUE,
        include.aic=FALSE, include.bic=FALSE, include.variance=FALSE,
        custom.gof.names = c("Log likelihood", "N", "N (Team/Sport)", "N (Team)"),
        file = "twist_table.html")




####################################################################################




## Model 2 (Table 1) ----
glmer_football <- glmer(result_dummy ~ 
                            elo_diff_adjusted_log + 
                            competition_dummy_factor * match_place +  
                            year + (1 | team),
                        family = binomial(link="logit"),  
                        data = filter(data_long_one_team_dummy,
                                      sport == "Gaelic Football"))
summary(glmer_football)

## Model 3 (Table 1) ----
glmer_hurling <- glmer(result_dummy ~ elo_diff_adjusted_log + 
                           competition_dummy_factor * match_place + 
                           year + (1 | team),
                       family = binomial(link = "logit"),  
                       data = filter(data_long_one_team_dummy, 
                                     sport == "Hurling"))
summary(glmer_hurling)


## Table 1 ----
coef_names <- c("(Intercept)", 
                "Other Competition (ref: Championship)",
                "Home (ref: Away)",
                "Neutral Ground (ref: Away)",
                "Hurling (ref: Gaelic Football)",
                "Elo Difference (log)",
                "2010", "2011", "2012", "2013", "2014", "2015",
                "2016", "2017", "2018", 
                "Other Competition * Home",
                "Other Competition * Neutral Ground",
                "Hurling * Other Competition",
                "Hurling * Home", 
                "Hurling * Neutral Ground", 
                "Other competition * Home * Hurling",
                "Other competition * Neutral Ground * Hurling")

## Screenreg output
screenreg(list(glmer_total, glmer_football, glmer_hurling),
          custom.model.names = c("Combined", "Football", "Hurling"),
          include.aic=FALSE, include.bic=FALSE, include.variance=FALSE,
          custom.gof.names = c("Log likelihood", "N", "N (Team/Sport)", "N (Team)"))


## HTML output
htmlreg(list(glmer_total, glmer_football, glmer_hurling),
        custom.model.names = c("(1) Combined", "(2) Football", "(3) Hurling"),
        custom.coef.names = coef_names, single.row = TRUE,
        include.aic=FALSE, include.bic=FALSE, include.variance=FALSE,
        custom.gof.names = c("Log likelihood", "N", "N (Team/Sport)", "N (Team)"),
        file = "reg_table.html")


## Figure 2 ----

data_effect_place <- Effect(c("match_place", "sport"), xlevels = 100, 
                            glmer_total) %>% 
    as.data.frame()

effect_match_place <- ggplot(data_effect_place, 
                             aes(x = match_place, 
                                 y = fit, ymin = lower, ymax = upper)) + 
    geom_pointrange() + 
    labs(x = NULL, y = "Predicted probability of winning") +
    geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red") +
    facet_wrap(~sport) + 
    scale_y_continuous(limits = c(0.4, 1), breaks = c(seq(0.4, 1, 0.1))) +
    theme(axis.text = element_text(colour = "black"),
          strip.text = element_text(size = 11, face = "bold"))
effect_match_place



## create data frame for matching (excluding draws)

data_matching <- data_long_one_team %>% 
    filter(result != "draw") %>% 
    ungroup() %>% 
    mutate(match_place_num = as.numeric(match_place)) %>% 
    mutate(result_dummy = as.factor(result_dummy),
           result_dummy_num = as.numeric(result_dummy),
           result_dummy_num = car::recode(result_dummy_num, "1=0;2=1"),
           team_sport = as.numeric(team_sport)) %>% 
    select(match_place, 
           match_place_num, 
           sport, 
           result_dummy_num, 
           competition_dummy_factor, elo_diff_adjusted_log) %>% 
    as.data.frame()


## models excluding away (neutral -> home)    

data_matching_not_away <- data_matching %>% 
    filter(match_place != "Away") %>% 
    select(-match_place) %>% 
    as.data.frame()

data_matching_not_away$sport

q1.grp <- list(c("Hurling"), c("Gaelic Football"))

mat_home_neutral_weight <- cem(treatment = "match_place_num", 
                               data = data_matching_not_away, 
                               grouping = list(sport = q1.grp),
                               drop = "result_dummy_num")


est_home_neutral_weight <- att(mat_home_neutral_weight, result_dummy_num ~ match_place_num, data = data_matching_not_away, model = "logit")

est_home_neutral_weight_ivs <- att(mat_home_neutral_weight, 
                                   result_dummy_num ~ match_place_num +  
                                       sport  + 
                                       competition_dummy_factor + 
                                       elo_diff_adjusted_log, 
                                   data = data_matching_not_away,
                                   model = "logit")

mat_home_neutral_exact <- cem(treatment = "match_place_num", 
                              data = data_matching_not_away, 
                              drop = "result_dummy_num", keep.all = TRUE)

mat_home_neutral_exact <- k2k(mat_home_neutral_exact, 
                              data_matching_not_away, "euclidean", 1)

est_home_neutral_exact <- att(mat_home_neutral_exact, 
                              result_dummy_num ~ match_place_num, 
                              data = data_matching_not_away, model = "logit")

est_home_neutral_exact_ivs <- att(mat_home_neutral_exact, 
                                  result_dummy_num ~ match_place_num +  
                                      sport + 
                                      competition_dummy_factor + 
                                      elo_diff_adjusted_log, 
                                  data = data_matching_not_away,
                                  model = "logit")

data_matching_not_home <- data_matching %>% 
    filter(match_place != "Home") %>% 
    select(-match_place) %>% 
    mutate(match_place_num = car::recode(match_place_num, "3=2")) %>% 
    as.data.frame()

mat_away_neutral_weighted <- cem(treatment = "match_place_num", data = data_matching_not_home, drop = "result_dummy_num")

est_away_neutral_weight <- att(mat_away_neutral_weighted, 
                               result_dummy_num ~ match_place_num, 
                               data = data_matching_not_home, model = "logit")


est_away_neutral_weight_ivs <- att(mat_away_neutral_weighted, 
                                   result_dummy_num ~ match_place_num + 
                                       sport + 
                                       competition_dummy_factor +
                                       elo_diff_adjusted_log, 
                                   data = data_matching_not_home,
                                   model = "logit")


mat_away_neutral_exact <- cem(treatment = "match_place_num", 
                              data = data_matching_not_home, 
                              drop = "result_dummy_num", keep.all = TRUE)

mat_away_neutral_exact <- k2k(mat_away_neutral_exact, 
                              data_matching_not_home, "euclidean", 1)

est_away_neutral_exact <- att(mat_away_neutral_exact, 
                              result_dummy_num ~ match_place_num, 
                              data = data_matching_not_home, model = "logit")


est_away_neutral_exact_ivs <- att(mat_away_neutral_exact, 
                                  result_dummy_num ~ match_place_num + 
                                      sport +
                                      competition_dummy_factor + 
                                      elo_diff_adjusted_log, 
                                  data = data_matching_not_home,
                                  model = "logit")


## write function to extract model coefficients and add note on model
extract_cem <- function(model, note) {
    est <- model
    
    treatment_df <- data.frame(est$tab)
    treatment_df <- rowSums(treatment_df)
    percentage_matched <- paste0(round(treatment_df[2] / treatment_df[1] * 100, 1), "%")
    
    coefs <- data.frame(est$att.model)
    satt <- coefs$match_place_num[1]
    satt_se <- coefs$match_place_num[2]
    satt_pval <-  sprintf("%.4f", round(coefs$match_place_num[4], 4))
    
    df <- data.frame(
        `model` = as.character(note),
        `matchted` = percentage_matched,
        satt = round(satt, 3),
        `se` = round(satt_se, 3),
        `p-value` = satt_pval
        
    )
    rownames(df) <- c() 
    print(df)
}



model1a <- extract_cem(est_home_neutral_weight, note = "Home -> Neutral (weighted)")
model1b <- extract_cem(est_home_neutral_exact, note = "Home -> Neutral (balanced)")
model1c <- extract_cem(est_home_neutral_weight_ivs, note = "Home -> Neutral (weighted) with covariates")
model1d <- extract_cem(est_home_neutral_exact_ivs, note = "Home -> Neutral (balanced) with covariates")
model2a <- extract_cem(est_away_neutral_weight, note = "Away -> Neutral (weighted)")
model2b <- extract_cem(est_away_neutral_exact, note = "Away -> Neutral (balanced)")
model2c <- extract_cem(est_away_neutral_weight_ivs, note = "Away -> Neutral (weighted) with covariates")
model2d <- extract_cem(est_away_neutral_exact_ivs, note = "Away -> Neutral (balanced) with covariates")


output_cem <- bind_rows(model1a, model1b, model1c, model1d,  
                        model2a, model2b, model2c, model2d) %>% 
    rownames_to_column(var = "Model") %>% 
    mutate(Model = paste("Model", Model))
colnames(output_cem) <- c("Model", "Specification", "Matched observations", "SATT", "SE (SATT)", "p-value")


## prepare data frame for plot
output_cem_plot <- output_cem %>% 
    mutate(ci_low = SATT - 1.96 * `SE (SATT)`,
           ci_high = SATT + 1.96 * `SE (SATT)`) %>% 
    mutate(SATT = as.numeric(SATT)) %>% 
    mutate(model_type = ifelse(str_detect(Specification, "Home"), "From Home to Neutral", "From Away to Neutral")) %>% 
    mutate(specification_short = car::recode(Specification, 
                                             "'Home -> Neutral (weighted)'='Weighted';'Away -> Neutral (weighted)'='Weighted'; 'Home -> Neutral (balanced)'='Balanced'; 'Home -> Neutral (weighted) with covariates'='Weighted, with covariates'; 'Home -> Neutral (balanced) with covariates'='Balanced, with covariates';'Away -> Neutral (balanced)'='Balanced';
                                             'Away -> Neutral (weighted) with covariates'='Weighted, with covariates';'Neutral (balanced) with covariates'='Balanced, with covariates';
                                             'Away -> Neutral (balanced) with covariates'='Balanced, with covariates'")) %>% 
    mutate(labels_axis = paste0(specification_short, " (", `Matched observations`, ")"))


## Figure 3 ----
ggplot(output_cem_plot, aes(y = SATT, x = labels_axis)) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "red") +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high)) +
    coord_flip() + 
    facet_wrap(~model_type, nrow = 2, scales = "free_y") +
    labs(y = "Sample average treatment effect on the treated (SATT) and 95% CI",
         x = NULL) +
    theme(axis.text = element_text(colour = "black"),
          strip.text = element_text(size = 11, face = "bold"))


### Plots and Figures from Supplementary Material

boot_home_advantage_no_draws <- data_long_dummy %>%
  filter(result %in% c("winner", "loser")) %>% 
  filter(match_place != "Neutral Ground") %>% 
  ungroup() %>% 
  group_by(sport, competition_dummy, match_place) %>%
  do(data.frame(rbind(smean.cl.boot(.$result_dummy, na.rm = TRUE)))) %>% 
  filter(match_place == "Home") %>% 
  mutate(label_axis = paste(sport, competition_dummy, sep = ": "))


cases_home_advantage_no_draws <- data_long_dummy %>%
    filter(result %in% c("winner", "loser")) %>%
    filter(match_place != "Neutral Ground") %>%
    filter(match_place == "Home") %>%
    group_by(sport, competition_dummy) %>%
    count()

boot_home_advantage_no_draws <- boot_home_advantage_no_draws %>%
    bind_cols(cases_home_advantage_no_draws)
# Rename columns
names(boot_home_advantage_no_draws)[names(boot_home_advantage_no_draws) == "sport...1"] <- "sport"
names(boot_home_advantage_no_draws)[names(boot_home_advantage_no_draws) == "competition_dummy...2"] <- "competition_dummy"

names(boot_home_advantage_no_draws)[names(boot_home_advantage_no_draws) == "sport...1"] <- "sport"
names(boot_home_advantage_no_draws)[names(boot_home_advantage_no_draws) == "competition_dummy...2"] <- "competition_dummy"
# Delete unnecessary columns
columns_to_delete <- c("sport...1", "competition_dummy...2", "sport...8", "competition_dummy...9")
boot_home_advantage_no_draws <- boot_home_advantage_no_draws[, !(names(boot_home_advantage_no_draws) %in% columns_to_delete)]


colnames(boot_home_advantage_no_draws)
colnames(cases_home_advantage_no_draws)
str(boot_home_advantage_no_draws)

ggplot(data = boot_home_advantage_no_draws, aes(x = sport,
                                       y = Mean, ymin = Lower, ymax = Upper,color= competition_dummy))+
    geom_pointrange(position = position_dodge(width = 0.2),) +
    coord_flip() +
    facet_wrap(~competition_dummy, scales = "free_y") +
    geom_text(aes(label = n), nudge_x = 0.25, colour = "grey50", size = 3) +
    labs(x = NULL, y = "Home advantage") +
    geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted") +
    scale_y_continuous(limits = c(0.4, 0.8), breaks = c(seq(0.4, 0.8, 0.1))) +
    theme(axis.text = element_text(colour = "black"),
          strip.text = element_text(size = 11, face = "bold"))

## Figure A2 ----

sum_prop_boot <- data_long_one_team %>%
    filter(result %in% c("winner", "loser")) %>%
    ungroup() %>%
    group_by(sport, competition_dummy, match_place) %>%
    count() %>%
    mutate(games_label = paste("Games:", n, sep = " "))

set.seed(124)

data_prop <- data_long_one_team %>%
    ungroup() %>%
    filter(result %in% c("winner", "loser")) %>%
    group_by(sport, competition_dummy, match_place) %>%
    do(data.frame(rbind(smean.cl.boot(.$result_dummy, na.rm = TRUE)))) %>%
    bind_cols(sum_prop_boot)



# Rename columns
names(data_prop)[names(data_prop) == "sport...1"] <- "sport"
names(data_prop)[names(data_prop) == "competition_dummy...2"] <- "competition_dummy"
names(data_prop)[names(data_prop) == "match_place...3"] <- "match_place"



# Delete unnecessary columns
columns_to_delete <- c("sport...1","match_place...9"  , "competition_dummy...2", "sport...7", "competition_dummy...8")
data_prop <- data_prop[, !(names(data_prop) %in% columns_to_delete)]
colnames(data_prop)
colnames(sum_prop_boot)

ggplot(data = data_prop, aes(x = match_place,
               y = Mean, ymin = Lower, ymax = Upper)) +
    geom_pointrange(position = position_dodge(width = 0.2)) +
    facet_wrap(competition_dummy~sport, scales = "free_x") +
    geom_text(aes(label = n), nudge_x = 0.25, colour = "grey50", size = 3) +
    labs(x = NULL, y = "Proportion of wins") +
    geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted") +
    scale_y_continuous(limits = c(0.3, 1), breaks = c(seq(0.3, 1, 0.1))) +
    theme(axis.text = element_text(colour = "black"),
          strip.text = element_text(size = 11, face = "bold"))

## Figure A3 ----

sum_prop_boot_with_draw <- data_long_one_team %>%
    ungroup() %>%
    group_by(sport, competition_dummy, match_place) %>%
    count() %>%
    mutate(games_label = paste("Games:", n, sep = " "))

set.seed(124)
data_prop_with_draw <- data_long_one_team %>%
    ungroup() %>%
    group_by(sport, competition_dummy, match_place) %>%
    do(data.frame(rbind(smean.cl.boot(.$result_num, na.rm = TRUE)))) %>%
    bind_cols(sum_prop_boot_with_draw)

# Rename columns
names(data_prop_with_draw)[names(data_prop_with_draw) == "sport...1"] <- "sport"
names(data_prop_with_draw)[names(data_prop_with_draw) == "competition_dummy...2"] <- "competition_dummy"
names(data_prop_with_draw)[names(data_prop_with_draw) == "match_place...3"] <- "match_place"



# Delete unnecessary columns
columns_to_delete <- c("sport...1","match_place...9"  , "competition_dummy...2", "sport...7", "competition_dummy...8")
data_prop_with_draw <- data_prop_with_draw[, !(names(data_prop_with_draw) %in% columns_to_delete)]

colnames(data_prop_with_draw)
colnames(sum_prop_boot)

ggplot(data = data_prop_with_draw, aes(x = match_place,
                             y = Mean, ymin = Lower, ymax = Upper)) +
    geom_pointrange(position = position_dodge(width = 0.2)) +
    facet_wrap(competition_dummy~sport, scales = "free_x") +
    geom_text(aes(label = n), nudge_x = 0.25, colour = "grey50", size = 3) +
    labs(x = NULL, y = "Proportion of wins") +
    geom_hline(yintercept = 0.5, colour = "red", linetype = "dotted") +
    scale_y_continuous(limits = c(0.35, 1), breaks = c(seq(0.4, 1, 0.1))) +
    theme(axis.text = element_text(colour = "black"),
        strip.text = element_text(size = 11, face = "bold"))


## Repeat regression analysis for underdog
library(lme4)

data_long_one_team_underdog <- data_long_dummy %>% 
    group_by(game_id) %>% 
    arrange(-elo_diff_adjusted) %>% 
    filter(row_number() == 2)

data_long_one_team_underdog <- data_long_one_team_underdog %>% 
    mutate(team_sport = as.factor(paste(team, sport, sep = "_"))) %>% 
    filter(result %in% c("winner", "loser"))

## Model 1 (Table A1) ----
glmer_total_underdog <- glmer(result_dummy ~  competition_dummy_factor * match_place * sport + elo_diff_adjusted +year + (1 | team_sport),
                              family = binomial(link = "logit"),  
                              data = data_long_one_team_underdog)


## Model 2 (Table A1) ----
glmer_football_underdog <- glmer(result_dummy ~ elo_diff_adjusted + 
                                     competition_dummy_factor * match_place + 
                                     year + (1 | team),
                                 family = binomial(link = "logit"),  
                                 data = filter(data_long_one_team_underdog,
                                               sport == "Gaelic Football"))

## Model 3 (Table A1) ----
glmer_hurling_underdog <- glmer(result_dummy ~ elo_diff_adjusted + 
                                    competition_dummy_factor * match_place + year + (1 | team),
                                family = binomial(link = "logit"),  
                                data = filter(data_long_one_team_underdog, sport == "Hurling"))

## Create Table A2
coef_names_underdog <- c("(Intercept)", 
                         "Other Competition (ref: Championship)",
                         "Home (ref: Away)",
                         "Neutral Ground (ref: Away)",
                         "Hurling (ref: Gaelic Football)",
                         "Elo Difference (log)",
                         "2010", "2011", "2012", "2013", "2014", "2015",
                         "2016", "2017", "2018", 
                         "Other Competition * Home",
                         "Other Competition * Neutral Ground",
                         "Hurling * Other Competition",
                         "Hurling * Home", 
                         "Hurling * Neutral Ground", 
                         "Other competition * Home * Hurling",
                         "Other competition * Neutral Ground * Hurling")

## HTML output
htmlreg(list(glmer_total_underdog, glmer_football_underdog, glmer_hurling_underdog),
        custom.model.names = c("(1) Combined", "(2) Football", "(3) Hurling"),
        custom.coef.names = coef_names_underdog, single.row = TRUE,
        include.aic=FALSE, include.bic=FALSE, include.variance=FALSE,
        custom.gof.names = c("Log likelihood", "N", "N (Team/Sport)", "N (Team)"),
        file = "reg_table_underdog.html")


## Figure A4 ----

data_effect_place_underdog <- Effect(c("match_place", "sport"),
                                     glmer_total_underdog) %>% 
    as.data.frame()

effect_match_place_underdog <- ggplot(data_effect_place_underdog, 
                                      aes(x = match_place, 
                                          y = fit, ymin = lower,
                                          ymax = upper)) + 
    geom_pointrange() + 
    labs(x = NULL, y = "Predicted probability of winning") +
    geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red") +
    facet_wrap(~sport) +
    scale_y_continuous(limits = c(0, 0.6), breaks = c(seq(0, 0.6, 0.1))) +
    theme(axis.text = element_text(colour = "black"),
          strip.text = element_text(size = 11, face = "bold"))
effect_match_place_underdog


### Run multinomial regression

data_long_one_team$result <- factor(data_long_one_team$result,
                                    levels = c("loser", "winner", "draw"))



multinom_total <- multinom(result ~  competition_dummy_factor * match_place * sport + 
                               elo_diff_adjusted_log 
                           + year + team_sport,
                           data = data_long_one_team)

## Prepare effects plot for multinomial regression 
data_effect_place_multinom <- Effect(c("match_place", "sport", "competition_dummy_factor"),
                                     multinom_total) %>% 
    as.data.frame() %>% 
    select(match_place, sport, competition_dummy_factor, contains("prob"))


data_multiom_prob_plot <- data_effect_place_multinom %>% 
    select(match_place, sport, competition_dummy_factor, starts_with("prob.")) %>% 
    gather(key = prob_type, value = prob, 
           -c(sport, match_place, competition_dummy_factor)) 

data_multiom_lower_plot <- data_effect_place_multinom %>% 
    select(match_place, sport, competition_dummy_factor, contains("L.prob")) %>% 
    gather(key = lower_type, value = lower, 
           -c(sport, match_place, competition_dummy_factor))

data_multiom_upper_plot <- data_effect_place_multinom %>% 
    select(match_place, sport, competition_dummy_factor, contains("U.prob")) %>% 
    gather(key = lower_type, value = upper,
           -c(sport, match_place, competition_dummy_factor)) 

data_multinom_plot <- bind_cols(data_multiom_prob_plot,
                                data_multiom_lower_plot,
                                data_multiom_upper_plot) %>% 
    mutate(result_type = car::recode(prob_type, "'prob.draw'='Draw';'prob.winner'='Winner';'prob.loser'='Loser'")) 

data_multinom_plot$result_type <- factor(data_multinom_plot$result_type,
                                                  levels = c("Winner", "Draw", "Loser"))


## Figure A5 ----

colnames(data_multinom_plot)

# Rename columns
names(data_multinom_plot)[names(data_multinom_plot) == "sport...2"] <- "sport"
names(data_multinom_plot)[names(data_multinom_plot) == "competition_dummy_factor...3"] <- "competition_dummy_factor"
names(data_multinom_plot)[names(data_multinom_plot) == "match_place...1"] <- "match_place"



# Delete unnecessary columns
columns_to_delete <- c("sport...2", "match_place...9", "match_place...11", 
                       "competition_dummy_factor...3", "sport...7", "sport...12",
                       "competition_dummy_factor...8", "competition_dummy_factor...13")

data_multinom_plot <- data_multinom_plot[, !(names(data_multinom_plot) %in% columns_to_delete)]

colnames(data_multinom_plot)


ggplot(data_multinom_plot,                                
       aes(x = match_place, colour = competition_dummy_factor, 
           shape = competition_dummy_factor,
           y = prob, ymin = lower, ymax = upper)) + 
    geom_pointrange(position = position_dodge(width = 0.4)) + 
    labs(x = NULL, y = "Predicted probability") +
    geom_hline(yintercept = 0.5, linetype = "dotted", colour = "red") +
    scale_color_grey(name = "Competition", start = 0.3, end = 0.7) +
    scale_shape_discrete(name = "Competition") +
    facet_grid(sport~result_type) +
    theme(legend.position = "bottom", 
          axis.text = element_text(colour = "black"),
                strip.text = element_text(size = 11, face = "bold"))

