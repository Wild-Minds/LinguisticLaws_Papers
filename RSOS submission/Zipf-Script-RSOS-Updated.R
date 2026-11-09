# Packages
library(lme4)          # Fits mixed-effects models with lmer()
library(influence.ME)  # Leave-one-cluster-out influence diagnostics for lmer models
library(dplyr)         # Data manipulation (filter, %>%)
library(ggplot2)
library(patchwork)
library(sjPlot)

###############################################################################
# run_influence_screen_lmer()
#
# PURPOSE
#   Standardized pipeline you can apply to many datasets/models to:
#     1) Fit a full mixed-effects model (lmer)
#     2) Compute influence diagnostics by leaving out each level of a grouping factor
#     3) Compute DFBETAS for each left-out level
#     4) Flag "influential" levels using a cutoff rule (default: 2/sqrt(#levels))
#     5) Remove those levels from the data
#     6) Refit FULL and NULL models on the filtered data
#     7) Run an LRT (full vs null) and drop1() on the filtered full model
#
# ASSUMPTIONS
#   - You ALWAYS supply null_formula explicitly (no auto-building).
#   - 'group' is the random-effect factor whose levels you may remove (e.g., "Gesture_actionID").
#   - 'predictor' names fixed-effect term(s) whose DFBETAS will be used for flagging.
#
# IMPORTANT
#   - If lmerTest is attached, it can break influence.ME (deviance function extraction errors).
#     Best practice: do NOT load lmerTest in the same session.
###############################################################################

run_influence_screen_lmer <- function(
    dat,                                      # data.frame containing all variables used in the model
    full_formula = full_formula,                             # full model formula (fixed + random effects)
    null_formula = null_formula,                             # null model formula (must be provided; usually fixed intercept only + same random effects)
    group,                                    # name (string) of grouping variable to leave out level-by-level in influence()
    predictor,                                    # fixed-effect term(s) (character vector) to screen DFBETAS on
    dfbeta_cutoff = function(n_groups) 2 / sqrt(n_groups),  # cutoff rule; default is 2/sqrt(#levels)
    ...                                       # passed through to lme4::lmer() (e.g., REML=FALSE, control=..., etc.)
) {
  
  ###########################################################################
  # 0) Sanity checks: catch common errors early (before lmer()/influence())
  ###########################################################################
  
  # vars_needed_full: all variable names referenced in the FULL formula
  # - all.vars() returns names appearing in the formula (response + predictors)
  vars_needed_full <- all.vars(full_formula)
  
  # vars_needed_null: all variable names referenced in the NULL formula
  vars_needed_null <- all.vars(null_formula)
  
  # vars_needed: union of required variables across both models
  # - if anything is missing in dat, model fitting will fail or behave unexpectedly
  vars_needed <- union(vars_needed_full, vars_needed_null)
  
  # missing: which needed variables are not columns in dat?
  missing <- setdiff(vars_needed, names(dat))
  
  # If anything is missing, stop with a clear error.
  # This also prevents the classic "closure is not subsettable" issue when a name
  # (e.g., 'log') exists as a function but not as a column.
  if (length(missing) > 0) {
    stop(
      "These variables are in the formulas but not in dat: ",
      paste(missing, collapse = ", "),
      "\nCommon cause: a response named `log` but dat has no `log` column; then R uses log() the function."
    )
  }
  
  # Ensure the grouping variable used for influence is present in the dataset
  if (!group %in% names(dat)) {
    stop("group column not found in dat: ", group)
  }
  
  ###########################################################################
  # 1) Fit the FULL model on the original data
  ###########################################################################
  
  # m_full: lmer model fit using your full_formula and dat
  # - '...' lets you pass e.g. REML=FALSE or control=lmerControl(...)
  data.update <- dat
  m_full <- lme4::lmer(full_formula, data = data.update, control = lmerControl(optimizer = "bobyqa", 
                                                                               optCtrl = list(maxfun = 2e5)))
  
  ###########################################################################
  # 2) Compute influence diagnostics by leaving out EACH LEVEL of 'group'
  ###########################################################################
  
  # infl: influence object containing refits where each group level is dropped once
  # - group = group tells influence.ME which factor defines the "case" to delete
  infl <- influence.ME::influence(model = m_full, group = group, obs = F, delete = F)
  
  ###########################################################################
  # 3) Compute DFBETAS from the influence object
  ###########################################################################
  
  # dfb: matrix of DFBETAS
  # - rows correspond to levels of 'group' (each leave-one-level-out refit)
  # - columns correspond to fixed-effect coefficients in the model (including intercept)
  # - abs = FALSE keeps signed values (use abs() later so you can still inspect direction)
  dfb <- dfbetas(infl, abs = FALSE)
  
  # n_groups: number of deleted-case refits (i.e., number of levels of 'group')
  n_groups <- nrow(dfb)
  
  ###########################################################################
  # 4) Decide which levels are "influential" using a cutoff rule
  ###########################################################################
  
  # cut: numeric cutoff for DFBETAS (default: 2/sqrt(n_groups))
  # - because you pass a FUNCTION, you can standardize the pipeline but still swap rules
  cut <- dfbeta_cutoff(n_groups)
  
  # predictor_present: restrict predictor to coefficients that actually exist in dfb columns
  # - avoids typos causing cryptic indexing errors
  predictor_present <- intersect(predictor, colnames(dfb))
  
  # If none of the requested predictor terms exist, stop with a clear message.
  if (length(predictor_present) == 0) {
    stop(
      "None of the predictor term(s) were found in dfbetas columns.\n",
      "Requested: ", paste(predictor, collapse = ", "), "\n",
      "Available: ", paste(colnames(dfb), collapse = ", ")
    )
  }
  
  # flag_levels: which levels of 'group' exceed the cutoff for ANY predictor coefficient?
  # - dfb[, predictor_present, drop=FALSE] keeps a matrix even if length(predictor_present)==1
  # - abs(...) compares absolute DFBETAS magnitude to cut
  # - apply(..., 1, any) flags a row if any predictor coefficient crosses the threshold
  flag_levels <- rownames(dfb)[
    apply(abs(dfb[, predictor_present, drop = FALSE]) > cut, 1, any)
  ]
  
  ###########################################################################
  # 5) Filter the dataset to REMOVE flagged levels of 'group'
  ###########################################################################
  
  # dat_filt: same as dat, but excluding any rows where group is in flag_levels
  # - .data[[group]] lets us programmatically refer to a column whose name is in a string
  dat_filt <- dat %>%
    filter(!(.data[[group]] %in% flag_levels))
  
  data.update <- dat_filt
  
  ###########################################################################
  # 6) Refit FULL and NULL models on the filtered data
  ###########################################################################
  
  # m_full_filt: full model refit after removing influential group levels
  m_full_filt <- lme4::lmer(full_formula, data = data.update, control = lmerControl(optimizer = "bobyqa", 
                                                                                    optCtrl = list(maxfun = 2e5)))
  
  # m_null_filt: null model refit after removing influential group levels
  # - you supply null_formula explicitly (no auto-building here)
  m_null_filt <- lme4::lmer(null_formula, data = data.update, control = lmerControl(optimizer = "bobyqa", 
                                                                                    optCtrl = list(maxfun = 2e5)))
  
  ###########################################################################
  # 7) Model comparison + term tests on the filtered model
  ###########################################################################
  
  # lrt: likelihood-ratio test comparing filtered FULL vs filtered NULL
  # NOTE: For LRT-based fixed-effect comparisons, REML=FALSE is typically recommended.
  # If you want that, pass REML=FALSE via '...'.
  lrt <- anova(m_full_filt, m_null_filt)
  
  ###########################################################################
  # 8) Return a structured list so downstream code is standardized
  ###########################################################################
  list(
    # Influence screening details
    group = group,                       # which grouping factor was screened
    predictor = predictor_present,               # which predictor terms were actually used
    n_groups = n_groups,                 # number of group levels screened
    cutoff = cut,                        # DFBETAS cutoff used
    dfbetas = dfb,                       # full DFBETAS matrix (for inspection/plotting)
    flagged_levels = flag_levels,        # group levels flagged as influential
    n_flagged = length(flag_levels),     # how many levels were flagged
    
    # Data used for refits
    data_filtered = dat_filt,            # filtered dataset (after removing flagged levels)
    
    # Models (original + filtered)
    model_full_original = m_full,        # full model fit on original data
    influence_object = infl,             # influence.ME object (contains refits diagnostics)
    model_full_filtered = m_full_filt,   # full model fit on filtered data
    model_null_filtered = m_null_filt,   # null model fit on filtered data
    
    # Statistical summaries
    lrt = lrt                   # drop1() table on filtered full model
  )
}
capitalize_first <- function(x) {
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}
#Upload data----
library(dplyr)

MAu_data_anon<-read.csv(file.choose(), stringsAsFactors = T) #choose Mau_anon_data_ga.csv
MAu_data_anon_morph<-read.csv(file.choose(), stringsAsFactors = T) #choose Mau_anon_data_moprhs.csv

#Split MAU dataset based on moprhs and gestures

Waibira_Mau<-MAu_data_anon %>% filter(Social_unit %in% "Waibira")
Bossou_Mau<-MAu_data_anon %>% filter(Social_unit %in% "Bossou")
Sonso_Mau<-MAu_data_anon %>% filter(Social_unit %in% "Sonso")
Waibira_Mau$Mau_duration <-exp(Waibira_Mau$log)
Bossou_Mau$Mau_duration <-exp(Bossou_Mau$log)
Sonso_Mau$Mau_duration <-exp(Sonso_Mau$log)


Waibira_Mau_morph<-MAu_data_anon_morph %>% filter(Social_unit %in% "Waibira")
Bossou_Mau_morph<-MAu_data_anon_morph %>% filter(Social_unit %in% "Bossou")
Sonso_Mau_morph<-MAu_data_anon_morph %>% filter(Social_unit %in% "Sonso")
Waibira_Mau_morph$Mau_duration <-exp(Waibira_Mau_morph$log)
Bossou_Mau_morph$Mau_duration <-exp(Bossou_Mau_morph$log)
Sonso_Mau_morph$Mau_duration <-exp(Sonso_Mau_morph$log)

# Create model formulas
## MAU Action
full_formula_mau_action <- log ~ rFq_action + (1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal)
null_formula_mau_action <- log ~ 1 +(1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal)
## MAU Morph
full_formula_mau_morph <- log ~ rFq_morph + (1 | Signaller) + (1 | morphs_anon) + (1 | Goal)
null_formula_mau_morph <- log ~ 1 + (1 | Signaller) + (1 | morphs_anon) + (1 | Goal)
##PAU Action
full_formula_pau_action <- log ~ rFq_action  +(1 | Signaller)+ (1 | gesture_record_anon) + (1 | Goal)
null_formula_pau_action <- log ~ 1 + (1 | Signaller)+(1 | gesture_record_anon) + (1 | Goal)
##PAU Morph
full_formula_pau_morph <- log ~ rFq_morph+ (1 | Signaller)+ (1 | morphs_anon) + (1 | Goal)
null_formula_pau_morph <- log ~ 1 +(1 | Signaller)+ (1 | morphs_anon) + (1 | Goal)

#Waibira formulas for PAU
full_formula_pau_action_waibira <- log ~ rFq_action  + (1 | gesture_record_anon) + (1 | Goal)
null_formula_pau_action_waibira <- log ~ 1 +(1 | gesture_record_anon) + (1 | Goal)
##PAU Morph
full_formula_pau_morph_waibira <- log ~ rFq_morph+ (1 | morphs_anon) + (1 | Goal)
null_formula_pau_morph_waibira <- log ~ 1 + (1 | morphs_anon) + (1 | Goal)

check_diagnostics <- function(model) {
  par(mfrow = c(2,2))
  qqnorm(resid(model)); qqline(resid(model))
  hist(resid(model))
  plot(fitted(model), resid(model))
}

# Run Models ----
library(lme4)
### MAU Action----
####Sonso ----
res_sonso_MAU_action_anon <- 
  run_influence_screen_lmer(
    dat = Sonso_Mau,
    full_formula = full_formula_mau_action,
    null_formula = null_formula_mau_action,
    group = "gesture_record_anon",
    predictor = "rFq_action"
  )
data.update <- res_sonso_MAU_action$data_filtered

str(Sonso_Mau)
res_sonso_MAU_action_anon$n_flagged 
res_sonso_MAU_action_anon$flagged_levels 
res_sonso_MAU_action_anon$dfbetas 
tab_model(res_sonso_MAU_action_anon$model_full_original)
tab_model(res_sonso_MAU_action_anon$model_full_filtered) 
check_diagnostics(res_sonso_MAU_action_anon$model_full_filtered)
res_sonso_MAU_action_anon$lrt 
plot_dfbetas_lmer(res_sonso_MAU_action_anon,   predictors = "rFq_action")

#Check if the most used
summary_Sonso_Mau<-Sonso_Mau %>% group_by(gesture_record_anon) %>% summarise(n=n())

#Perform same on non-filterd dataset
sonso_mau_og<-lmer(full_formula_mau_action <- log ~ rFq_action + (1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Sonso_Mau)
sonso_mau_og_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Sonso_Mau)
anova(sonso_mau_og, sonso_mau_og_null) 

#Perform simple correlation
Sonso_Mau_filtered <-Sonso_Mau %>% filter(!gesture_record_anon %in% res_sonso_MAU_action$flagged_levels)
Sonso_Mau_filtered_median <- Sonso_Mau_filtered %>% group_by(gesture_record_anon) %>% mutate(median_duration = median(Mau_duration)) %>% ungroup()
Sonso_Mau_filtered_median<-Sonso_Mau_filtered_median %>% select(gesture_record_anon, rFq_action, median_duration) %>% distinct()
s_mau_act<-cor.test(Sonso_Mau_filtered_median$median_duration, Sonso_Mau_filtered_median$rFq_action, method = 'kendall') #z = 0.13432, p-value = 0.8931
s_mau_act <- data.frame(p.value = s_mau_act$p.value)
s_mau_act$unit <- "s_mau_act"

#Perform log on filterd dataset
sonso_mau_log<-lmer(log ~ log(rFq_action) + (1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Sonso_Mau_filtered)
sonso_mau_log_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Sonso_Mau_filtered)
anova(sonso_mau_log, sonso_mau_log_null) #no difference

####Waibira----
res_waibira_MAU_action <- 
  run_influence_screen_lmer(
    dat = Waibira_Mau,
    full_formula = full_formula_mau_action,
    null_formula = null_formula_mau_action,
    group = "gesture_record_anon",
    predictor = "rFq_action"
  )
data.update <- res_waibira_MAU_action$data_filtered

res_waibira_MAU_action$n_flagged 
res_waibira_MAU_action$flagged_levels 
res_waibira_MAU_action$lrt
check_diagnostics(res_waibira_MAU_action$model_full_filtered)
plot_dfbetas_lmer(res_waibira_MAU_action,   predictors = "rFq_action")
tab_model(res_waibira_MAU_action$model_full_original) 
tab_model(res_waibira_MAU_action$model_full_filtered) 

#Check if the most used
summary_Waibira_Mau<-Waibira_Mau %>% group_by(gesture_record_anon) %>% summarise(n=n())

#Perform same on non-filterd dataset
Waibira_mau_og<-lmer(full_formula_mau_action <- log ~ rFq_action + (1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Waibira_Mau)
Waibira_mau_og_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Waibira_Mau)
anova(Waibira_mau_og, Waibira_mau_og_null) 

#Perform simple correlation
Waibira_Mau_filtered <-Waibira_Mau %>% filter(!gesture_record_anon %in% res_waibira_MAU_action$flagged_levels)
Waibira_Mau_filtered_median <- Waibira_Mau_filtered %>% group_by(gesture_record_anon) %>% mutate(median_duration = median(Mau_duration)) %>% ungroup()
Waibira_Mau_filtered_median<-Waibira_Mau_filtered_median %>% select(gesture_record_anon, rFq_action, median_duration) %>% distinct()
w_mau_act<-cor.test(Waibira_Mau_filtered_median$median_duration, Waibira_Mau_filtered_median$rFq_action, method = 'kendall') #t = 0.023539, df = 29, p-value = 0.9814
w_mau_act <- data.frame(p.value = w_mau_act$p.value)
w_mau_act$unit <- "w_mau_act"

#Perform log on filterd dataset
waibira_mau_log<-lmer(log ~ log(rFq_action) + (1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Waibira_Mau_filtered)
waibira_mau_log_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Waibira_Mau_filtered)
anova(waibira_mau_log, waibira_mau_log_null) #no difference

####Bossou----
res_bossou_MAU_action <- 
  run_influence_screen_lmer(
    dat = Bossou_Mau,
    full_formula = full_formula_mau_action,
    null_formula = null_formula_mau_action,
    group = "gesture_record_anon",
    predictor = "rFq_action"
  )
data.update <- res_bossou_MAU_action$data_filtered

res_bossou_MAU_action$n_flagged 
res_bossou_MAU_action$flagged_levels 
res_bossou_MAU_action$lrt 
check_diagnostics(res_bossou_MAU_action$model_full_filtered)
tab_model(res_bossou_MAU_action$model_full_filtered, show.se = T)

plot_dfbetas_lmer(res_bossou_MAU_action,   predictors = "rFq_action")
tab_model(res_bossou_MAU_action$model_full_original) 
tab_model(res_bossou_MAU_action$model_full_filtered)

#Check if the most used
summary_Bossou_Mau<-Bossou_Mau %>% group_by(gesture_record_anon) %>% summarise(n=n())

#Perform same on non-filterd dataset
Bossou_mau_og<-lmer(log ~ rFq_action + (1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Bossou_Mau)
Bossou_mau_og_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Bossou_Mau)
anova(Bossou_mau_og, Bossou_mau_og_null) 
tab_model(Bossou_mau_og)

#Perform simple correlation
Bossou_Mau_filtered <-Bossou_Mau %>% filter(!gesture_record_anon %in% res_bossou_MAU_action$flagged_levels)
Bossou_Mau_filtered_median <- Bossou_Mau_filtered %>% group_by(gesture_record_anon) %>% mutate(median_duration = median(Mau_duration)) %>% ungroup()
Bossou_Mau_filtered_median<-Bossou_Mau_filtered_median %>% select(gesture_record_anon, rFq_action, median_duration) %>% distinct()
b_mau_act<-cor.test(Bossou_Mau_filtered_median$median_duration, Bossou_Mau_filtered_median$rFq_action,  method = 'kendall') #t = -2.5124, df = 44, p-value = 0.01573
b_mau_act <- data.frame(p.value = b_mau_act$p.value)
b_mau_act$unit <- "b_mau_act"


#Perform log on filterd dataset
bossou_mau_log<-lmer(log ~ log(rFq_action) + (1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Bossou_Mau_filtered)
Bossou_mau_log_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Bossou_Mau_filtered)
anova(bossou_mau_log, Bossou_mau_log_null) # 6 2066.0 2097.4 -1027.0    2054.0 7.4913  1     0.0062 **

tab_model(bossou_mau_log)

#Check sd variation -----
sdvariation<-Mau_Data %>% group_by(Lumped_Gs, Social_unit) %>% summarise(sd=sd(Mau_duration))
sdvariation$Type<-"MAU"
sdvariation_Pau<-Pau_Data %>% group_by(Lumped_Gs, Social_unit) %>% summarise(sd=sd(Ga_duration))
sdvariation_Pau$Type<-"PAU"
sdvariation_join<-bind_rows(sdvariation, sdvariation_Pau)

library(geom_point)
ggplot(sdvariation_join, aes(x=Social_unit, y=sd, color=Type))+geom_violin()+
  geom_point(position=position_jitterdodge(), aes(color = Type))+theme_classic()+
  labs(y = 'Standard Deviation (s)', x= 'Duration Type')


### MAU Morph----
####Sonso----
res_sonso_MAU_morph <- 
  run_influence_screen_lmer(
    dat =Sonso_Mau_morph,
    full_formula = full_formula_mau_morph,
    null_formula = null_formula_mau_morph,
    group = "morphs_anon",
    predictor = "rFq_morph"
  )
res_sonso_MAU_morph$n_flagged
res_sonso_MAU_morph$flagged_levels
res_sonso_MAU_morph$lrt 
data.update <- res_sonso_MAU_morph$data_filtered
check_diagnostics(res_sonso_MAU_morph$model_full_filtered)
plot_dfbetas_lmer(res_sonso_MAU_morph,   predictors = "rFq_morph")

tab_model(res_sonso_MAU_morph$model_full_original) 
tab_model(res_sonso_MAU_morph$model_full_filtered) 

#Check if the most used
summary_Sonso_Mau_morph<-Sonso_Mau_morph %>% group_by(morphs_anon) %>% summarise(n=n())

#Perform same on non-filterd dataset
Sonso_mau_og_morph<-lmer(log ~ rFq_morph + (1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Sonso_Mau_morph)
Sonso_mau_og_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Sonso_Mau_morph)
anova(Sonso_mau_og_morph, Sonso_mau_og_null) 
tab_model(sonso_mau_og)

#Perform simple correlation
Sonso_Mau_filtered_morph <-Sonso_Mau_morph %>% filter(!morphs_anon %in% res_sonso_MAU_morph$flagged_levels)
Sonso_Mau_filtered_median_morph <- Sonso_Mau_filtered_morph %>% group_by(morphs_anon) %>% 
  mutate(median_duration = median(Mau_duration)) %>%
  ungroup()%>% select(morphs_anon, rFq_morph, median_duration)%>% 
  distinct()
s_mau_morph<-cor.test( method = 'kendall', Sonso_Mau_filtered_median_morph$median_duration, Sonso_Mau_filtered_median_morph$rFq_morph) #t = 0.075185, df = 110, p-value = 0.9402
s_mau_morph <- data.frame(p.value = s_mau_morph$p.value)
s_mau_morph$unit <- "s_mau_morph"


#Perform log on filterd dataset
sonso_mau_log_morph<-lmer(log ~ log(rFq_morph) + (1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Sonso_Mau_filtered_morph)
sonso_mau_log_morph_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Sonso_Mau_filtered_morph)
anova(sonso_mau_log_morph, sonso_mau_log_morph_null) #no difference

#### Waibira -----
Waibira_Mau_morph$morphs_anon<-as.factor(Waibira_Mau_morph$morphs_anon)
res_waibira_MAU_morph <- 
  run_influence_screen_lmer(
    dat =Waibira_Mau_morph,
    full_formula = full_formula_mau_morph,
    null_formula = null_formula_mau_morph,
    group = "morphs_anon",
    predictor = "rFq_morph"
  )
data.update <- res_waibira_MAU_morph$data_filtered
plot_dfbetas_lmer(res_waibira_MAU_morph,   predictors = "rFq_morph")

res_waibira_MAU_morph$n_flagged
res_waibira_MAU_morph$flagged_levels 
res_waibira_MAU_morph$lrt
tab_model(res_waibira_MAU_morph$model_full_filtered, show.se = T)
check_diagnostics(res_waibira_MAU_morph$model_full_filtered)

tab_model(res_waibira_MAU_morph$model_full_original) 
tab_model(res_waibira_MAU_morph$model_full_filtered)


#Check if the most used
summary_Waibira_Mau_morph<-Waibira_Mau_morph %>% group_by(morphs_anon) %>% summarise(n=n())

#Perform same on non-filterd dataset
Waibira_mau_og_morph<-lmer(log ~ rFq_morph + (1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Waibira_Mau_morph)
Waibira_mau_og_morph_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Waibira_Mau_morph)
anova(Waibira_mau_og_morph, Waibira_mau_og_morph_null) 

#Perform simple correlation
Waibira_Mau_filtered_morph <-Waibira_Mau_morph %>% filter(!morphs_anon %in% res_waibira_MAU_morph$flagged_levels)
Waibira_Mau_filtered_median_morph <- Waibira_Mau_filtered_morph %>% group_by(morphs_anon) %>% 
  mutate(median_duration = median(Mau_duration)) %>%
  ungroup()%>% select(morphs_anon, rFq_morph, median_duration)%>% 
  distinct()
w_mau_morph<-cor.test( method = 'kendall',Waibira_Mau_filtered_median_morph$median_duration, Waibira_Mau_filtered_median_morph$rFq_morph) #t = 1.2736, df = 58, p-value = 0.2079
w_mau_morph <- data.frame(p.value = w_mau_morph$p.value)
w_mau_morph$unit <- "w_mau_morph"

#Perform log on filterd dataset
waibira_mau_log_morph<-lmer(log ~ log(rFq_morph) + (1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Waibira_Mau_filtered_morph)
waibira_mau_log_morph_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Waibira_Mau_filtered_morph)
anova(waibira_mau_log_morph, waibira_mau_log_morph_null) #waibira_mau_log_morph    4.07  1    0.04365 *
tab_model(waibira_mau_log_morph)


####Bossou----
res_bossou_MAU_morph <- 
  run_influence_screen_lmer(
    dat = Bossou_Mau_morph,
    full_formula = full_formula_mau_morph,
    null_formula = null_formula_mau_morph,
    group = "morphs_anon",
    predictor = "rFq_morph"
  )

res_bossou_MAU_morph$n_flagged 
res_bossou_MAU_morph$flagged_levels
res_bossou_MAU_morph$lrt 
data.update <- res_bossou_MAU_morph$data_filtered
plot_dfbetas_lmer(res_bossou_MAU_morph,   predictors = "rFq_morph")

check_diagnostics(res_bossou_MAU_morph$model_full_filtered)

tab_model(res_bossou_MAU_morph$model_full_original) 
tab_model(res_bossou_MAU_morph$model_full_filtered) 

#Check if the most used
summary_Bossou_Mau_morph<-Bossou_Mau_morph %>% group_by(morphs_anon) %>% summarise(n=n())

#Perform same on non-filterd dataset
Bossou_mau_og_morph<-lmer(log ~ rFq_morph + (1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Bossou_Mau_morph)
Bossou_mau_og_morph_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Bossou_Mau_morph)
anova(Bossou_mau_og_morph, Bossou_mau_og_morph_null) 

#Perform simple correlation
Bossou_Mau_filtered_morph <-Bossou_Mau_morph %>% filter(!morphs_anon %in% res_bossou_MAU_morph$flagged_levels)
Bossou_Mau_filtered_median_morph <- Bossou_Mau_filtered_morph %>% group_by(morphs_anon) %>% 
  mutate(median_duration = median(Mau_duration)) %>%
  ungroup()%>% select(morphs_anon, rFq_morph, median_duration)%>% 
  distinct()
b_mau_morph<-cor.test( method = 'kendall',Bossou_Mau_filtered_median_morph$median_duration, Bossou_Mau_filtered_median_morph$rFq_morph) #t = -1.1685, df = 91, p-value = 0.2456
b_mau_morph <- data.frame(p.value = b_mau_morph$p.value)
b_mau_morph$unit <- "b_mau_morph"


#Perform log on filterd dataset
bossou_mau_morph_log<-lmer(log ~ log(rFq_morph) + (1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Bossou_Mau_filtered_morph)
Bossou_mau_morph_log_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Bossou_Mau_filtered_morph)
anova(bossou_mau_morph_log, Bossou_mau_morph_log_null) # no difference  1.1604  1     0.2814

### PAU Action ----
Pau_Data_anon<-read.csv(file.choose(), stringsAsFactors = T) #choose Pau_anon_data_ga.csv
Pau_Data_anon_morph<-read.csv(file.choose(), stringsAsFactors = T) #choose Pau_anon_data_moprhs.csv

Waibira_pau<-Pau_Data_anon %>% filter(Social_unit %in% "Waibira")
Bossou_pau<-Pau_Data_anon %>% filter(Social_unit %in% "Bossou")
Sonso_pau<-Pau_Data_anon %>% filter(Social_unit %in% "Sonso")
Waibira_pau$Pau_duration <-exp(Waibira_pau$log)
Bossou_pau$Pau_duration <-exp(Bossou_pau$log)
Sonso_pau$Pau_duration <-exp(Sonso_pau$log)


Waibira_pau_morph<-Pau_Data_anon_morph %>% filter(Social_unit %in% "Waibira")
Bossou_pau_morph<-Pau_Data_anon_morph %>% filter(Social_unit %in% "Bossou")
Sonso_pau_morph<-Pau_Data_anon_morph %>% filter(Social_unit %in% "Sonso")
Waibira_pau_morph$Pau_duration <-exp(Waibira_pau_morph$log)
Bossou_pau_morph$Pau_duration <-exp(Bossou_pau_morph$log)
Sonso_pau_morph$Pau_duration <-exp(Sonso_pau_morph$log)

####Sonso----
res_sonso_PAU_action <-
  run_influence_screen_lmer(
    dat = Sonso_pau,
    full_formula = full_formula_pau_action,
    null_formula = null_formula_pau_action,
    group = "gesture_record_anon",
    predictor = "rFq_action"
  )
res_sonso_PAU_action$n_flagged 
res_sonso_PAU_action$flagged_levels
res_sonso_PAU_action$lrt 
data.update <- res_sonso_PAU_action$data_filtered

check_diagnostics(res_sonso_PAU_action$model_full_filtered)
plot_dfbetas_lmer(res_sonso_PAU_action,   predictors = "rFq_action")

tab_model(res_sonso_PAU_action$model_full_original) 
tab_model(res_sonso_PAU_action$model_full_filtered) 
#Check if the most used
summary_Sonso_pau<-Sonso_pau %>% group_by(gesture_record_anon) %>% summarise(n=n())

#Perform same on non-filterd dataset
Sonso_pau_og<-lmer(log ~ rFq_action+ (1 | Signaller) + (1 |gesture_record_anon ) + (1 | Goal), data = Sonso_pau)
Sonso_pau_og_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Sonso_pau)
anova(Sonso_pau_og, Sonso_pau_og_null)


#Perform simple correlation
Sonso_pau_filtered <-Sonso_pau %>% filter(!gesture_record_anon %in% res_sonso_PAU_action$flagged_levels)
Sonso_pau_filtered_median <- Sonso_pau_filtered %>% group_by(gesture_record_anon) %>% mutate(median_duration = median(Pau_duration)) %>% ungroup()
Sonso_pau_filtered_median<-Sonso_pau_filtered_median %>% select(gesture_record_anon, rFq_action, median_duration) %>% distinct()
s_pau_act<-cor.test( method = 'kendall',Sonso_pau_filtered_median$median_duration, Sonso_pau_filtered_median$rFq_action) #t = -0.049207, df = 51, p-value = 0.9609
s_pau_act <- data.frame(p.value = s_pau_act$p.value)
s_pau_act$unit <- "s_pau_act"

#Perform log on filterd dataset
sonso_pau_log<-lmer(log ~ log(rFq_action) + (1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Sonso_pau_filtered)
sonso_pau_log_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Sonso_pau_filtered)
anova(sonso_pau_log, sonso_pau_log_null) #no difference 1.5772  1     0.2092

####Waibira - remove signaller -----
res_waibira_PAU_action <-
  run_influence_screen_lmer(
    dat = Waibira_pau,
    full_formula = full_formula_pau_action_waibira,
    null_formula = null_formula_pau_action_waibira,
    group = "gesture_record_anon",
    predictor = "rFq_action"
  ) 

res_waibira_PAU_action$n_flagged 
res_waibira_PAU_action$flagged_levels 
res_waibira_PAU_action$lrt  
check_diagnostics(res_waibira_PAU_action$model_full_filtered)
plot_dfbetas_lmer(res_waibira_PAU_action,   predictors = "rFq_action")

tab_model(res_waibira_PAU_action$model_full_original) 
tab_model(res_waibira_PAU_action$model_full_filtered) 
#Check if the most used
summary_Waibira_pau<-Waibira_pau %>% group_by(gesture_record_anon) %>% summarise(n=n())

#Perform same on non-filterd dataset
Waibira_pau_og<-lmer(log ~ rFq_action + (1 |gesture_record_anon ) + (1 | Goal), data = Waibira_pau)
Waibira_pau_og_null<-lmer( log ~ 1  + (1 | gesture_record_anon) + (1 | Goal), data = Waibira_pau)
anova(Waibira_pau_og, Waibira_pau_og_null) 


#Perform simple correlation
Waibira_pau_filtered <-Waibira_pau %>% filter(!gesture_record_anon %in% res_waibira_PAU_action$flagged_levels)
Waibira_pau_filtered_median <- Waibira_pau_filtered %>% group_by(gesture_record_anon) %>% mutate(median_duration = median(Pau_duration)) %>% ungroup()
Waibira_pau_filtered_median<-Waibira_pau_filtered_median %>% select(gesture_record_anon, rFq_action, median_duration) %>% distinct()
w_pau_act<-cor.test( method = 'kendall',Waibira_pau_filtered_median$median_duration, Waibira_pau_filtered_median$rFq_action) #t = 0.23319, df = 30, p-value = 0.8172
w_pau_act <- data.frame(p.value = w_pau_act$p.value)
w_pau_act$unit <- "w_pau_act"


#Perform log on filterd dataset
waibira_pau_log<-lmer(log ~ log(rFq_action) + + (1 | gesture_record_anon) + (1 | Goal), data = Waibira_pau_filtered)
waibira_pau_log_null<-lmer( log ~ 1  + (1 | gesture_record_anon) + (1 | Goal), data = Waibira_pau_filtered)
anova(waibira_pau_log, waibira_pau_log_null) #no difference 0.2272  1     0.6336

####Bossou----
res_bossou_PAU_action <-
  run_influence_screen_lmer(
    dat = Bossou_pau,
    full_formula = full_formula_pau_action,
    null_formula = null_formula_pau_action,
    group = "gesture_record_anon",
    predictor = "rFq_action"
  )
res_bossou_PAU_action$n_flagged 
res_bossou_PAU_action$flagged_levels 
res_bossou_PAU_action$lrt 
plot_dfbetas_lmer(res_bossou_PAU_action, 'rFq_action')
check_diagnostics(res_bossou_PAU_action$model_full_filtered)
tab_model(res_bossou_PAU_action$model_full_filtered, show.se = T)
summary(res_bossou_PAU_action$model_full_filtered)

tab_model(res_bossou_PAU_action$model_full_original) 
tab_model(res_bossou_PAU_action$model_full_filtered) 
#Check if the most used
summary_Bossou_pau<-Bossou_pau %>% group_by(gesture_record_anon) %>% summarise(n=n())

#Perform same on non-filterd dataset
Bossou_pau_og<-lmer(log ~ rFq_action + (1 |Signaller )+ (1 |gesture_record_anon ) + (1 | Goal), data = Bossou_pau)
Bossou_pau_og_null<-lmer( log ~ 1  + (1 |Signaller )+ (1 | gesture_record_anon) + (1 | Goal), data = Bossou_pau)
anova(Bossou_pau_og, Bossou_pau_og_null) 

Bossou_pau_filtered <-Bossou_pau %>% filter(!gesture_record_anon %in% res_bossou_PAU_action$flagged_levels)
Bossou_pau_filtered_median <- Bossou_pau_filtered %>% group_by(gesture_record_anon) %>% mutate(median_duration = median(Pau_duration)) %>% 
  ungroup()%>% select(gesture_record_anon, rFq_action, median_duration) %>% distinct()
b_pau_act<-cor.test( method = 'kendall',Bossou_pau_filtered_median$median_duration, Bossou_pau_filtered_median$rFq_action) #t = 0.23319, df = 30, p-value = 0.8172
b_pau_act <- data.frame(p.value = b_pau_act$p.value)
b_pau_act$unit <- "b_pau_act"

#Perform log on filterd dataset
bossou_pau_log<-lmer(log ~ log(rFq_action) + (1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Bossou_pau_filtered)
Bossou_pau_log_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | gesture_record_anon) + (1 | Goal), data = Bossou_pau_filtered)
anova(bossou_pau_log, Bossou_pau_log_null) #  9.3129  1   0.002275 **
tab_model(bossou_pau_log)

### PAU Morph----
####Sonso----
res_sonso_PAU_morph <-
  run_influence_screen_lmer(
    dat = Sonso_pau_morph,
    full_formula = full_formula_pau_morph,
    null_formula = null_formula_pau_morph,
    group = "morphs_anon",
    predictor = "rFq_morph"
  )
res_sonso_PAU_morph$n_flagged
res_sonso_PAU_morph$flagged_levels 
res_sonso_PAU_morph$lrt 
check_diagnostics(res_sonso_PAU_morph$model_full_filtered)
plot_dfbetas_lmer(res_sonso_PAU_morph, 'rFq_morph')

tab_model(res_sonso_PAU_morph$model_full_original) 
tab_model(res_sonso_PAU_morph$model_full_filtered) 

#Check if the most used
summary_Sonso_pau_morph<-Sonso_pau_morph %>% group_by(morphs_anon) %>% summarise(n=n())

#Perform same on non-filterd dataset
Sonso_pau_morph_og<-lmer(log ~ rFq_morph + (1 |Signaller )+ (1 |morphs_anon ) + (1 | Goal), data = Sonso_pau_morph)
Sonso_pau_og_morph_null<-lmer( log ~ 1  + (1 |Signaller )+ (1 | morphs_anon) + (1 | Goal), data = Sonso_pau_morph)
anova(Sonso_pau_morph_og, Bossou_pau_og_morph_null) 


#Perform simple correlation
Sonso_pau_filtered_morph <-Sonso_pau_morph %>% filter(!morphs_anon %in% res_sonso_PAU_morph$flagged_levels)
Sonso_pau_filtered_morphs_median <- Sonso_pau_filtered_morph %>% group_by(morphs_anon) %>% mutate(median_duration = median(Pau_duration)) %>% 
  ungroup() %>% select(morphs_anon, rFq_morph, median_duration) %>% distinct()
s_pau_morph<-cor.test( method = 'kendall',Sonso_pau_filtered_morphs_median$median_duration, Sonso_pau_filtered_morphs_median$rFq_morph) #t = 0.093134, df = 112, p-value = 0.926
s_pau_morph <- data.frame(p.value = s_pau_morph$p.value)
s_pau_morph$unit <- "s_pau_morph"

#Perform log on filterd dataset
sonso_pau_log_morph<-lmer(log ~ log(rFq_morph) + (1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Sonso_pau_filtered_morph)
sonso_pau_log_morph_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Sonso_pau_filtered_morph)
anova(sonso_pau_log_morph, sonso_pau_log_morph_null) #no difference 0.1582  1     0.6908

####Waibira - remove signaller----
res_waibira_PAU_morph <-
  run_influence_screen_lmer(
    dat = Waibira_pau_morph,
    full_formula = full_formula_pau_morph,
    null_formula = null_formula_pau_morph,
    group = "morphs_anon",
    predictor = "rFq_morph"
  )

res_waibira_PAU_morph$n_flagged 
res_waibira_PAU_morph$flagged_levels 
res_waibira_PAU_morph$lrt 
tab_model(res_waibira_PAU_morph$model_full_filtered, show.se = T) 
check_diagnostics(res_waibira_PAU_morph$model_full_filtered)
plot_dfbetas_lmer(res_waibira_PAU_morph, 'rFq_morph')

tab_model(res_waibira_PAU_morph$model_full_original) 
tab_model(res_waibira_PAU_morph$model_full_filtered) 


#Check if the most used
summary_Waibira_pau_morph<-Waibira_pau_morph %>% group_by(morphs_anon) %>% summarise(n=n())

#Perform same on non-filterd dataset
Waibira_pau_morph_og<-lmer(log ~ rFq_morph + (1|Signaller)+(1 |morphs_anon ) + (1 | Goal), data = Waibira_pau_morph)
Waibira_pau_og_morph_null<-lmer( log ~ 1  + (1|Signaller)+(1 | morphs_anon) + (1 | Goal), data = Waibira_pau_morph)
anova(Waibira_pau_morph_og, Waibira_pau_og_morph_null)

#Perform simple correlation
Waibira_pau_filtered_morph <-Waibira_pau_morph %>% filter(!morphs_anon %in% res_waibira_PAU_morph$flagged_levels)
Waibira_pau_filtered_morphs_median <- Waibira_pau_filtered_morph %>% group_by(morphs_anon) %>% mutate(median_duration = median(Pau_duration)) %>% 
  ungroup() %>% select(morphs_anon, rFq_morph, median_duration) %>% distinct()
w_pau_morph<-cor.test( method = 'kendall',Waibira_pau_filtered_morphs_median$median_duration, Waibira_pau_filtered_morphs_median$rFq_morph) #t = 1.7247, df = 56, p-value = 0.09009
w_pau_morph <- data.frame(p.value = w_pau_morph$p.value)
w_pau_morph$unit <- "w_pau_morph"
#Perform log on filterd dataset
waibira_pau_log_morph<-lmer(log ~ log(rFq_morph) + (1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Waibira_pau_filtered_morph)
waibira_pau_log_morph_null<-lmer( log ~ 1 +(1 | Signaller) + (1 | morphs_anon) + (1 | Goal), data = Waibira_pau_filtered_morph)
anova(waibira_pau_log_morph, waibira_pau_log_morph_null) #w 5.671  1    0.01725 *    0.04365 *
tab_model(waibira_pau_log_morph)

####Bossou----
res_bossou_PAU_morph <-
  run_influence_screen_lmer(
    dat = Bossou_pau_morph,
    full_formula = full_formula_pau_morph_waibira,
    null_formula = null_formula_pau_morph_waibira,
    group = "morphs_anon",
    predictor = "rFq_morph"
  )
res_bossou_PAU_morph$n_flagged
res_bossou_PAU_morph$flagged_levels
res_bossou_PAU_morph$lrt 
check_diagnostics(res_bossou_PAU_morph$model_full_filtered)

plot_dfbetas_lmer(res_bossou_PAU_morph, 'rFq_morph')

tab_model(res_bossou_PAU_morph$model_full_original) 
tab_model(res_bossou_PAU_morph$model_full_filtered) 


#Perform same on non-filterd dataset
Bossou_pau_morph_og<-lmer(log ~ rFq_morph + (1|Signaller)+(1 |morphs_anon ) + (1 | Goal), data =Bossou_pau_morph)
Bossou_pau_og_morph_null<-lmer( log ~ 1  + (1|Signaller)+(1 | morphs_anon) + (1 | Goal), data = Bossou_pau_morph)
anova(Bossou_pau_morph_og, Bossou_pau_og_morph_null)

#Perform simple correlation
Bossou_pau_filtered_morph <-Bossou_pau_morph %>% filter(!morphs_anon %in% res_bossou_PAU_morph$flagged_levels)
Bossou_pau_filtered_morphs_median <- Bossou_pau_filtered_morph %>% group_by(morphs_anon) %>% mutate(median_duration = median(Pau_duration)) %>% 
  ungroup() %>% select(morphs_anon, rFq_morph, median_duration) %>% distinct()
b_pau_morph<-cor.test( method = 'kendall',Bossou_pau_filtered_morphs_median$median_duration, Bossou_pau_filtered_morphs_median$rFq_morph) #t = -1.0367, df = 85, p-value = 0.3028
b_pau_morph <- data.frame(p.value = b_pau_morph$p.value)
b_pau_morph$unit <- "b_pau_morph"

#Perform log on filterd dataset
bossou_pau_morph_log<-lmer(log ~ log(rFq_morph)+ (1 | morphs_anon) + (1 | Goal), data = Bossou_pau_filtered_morph)
Bossou_pau_morph_log_null<-lmer( log ~ 1  + (1 | morphs_anon) + (1 | Goal), data = Bossou_pau_filtered_morph)
anova(bossou_pau_morph_log, Bossou_pau_morph_log_null) # no difference 0.8915  1     0.3451

#p value adjustment
library(dplyr)
join_p_values<-bind_rows(s_mau_act,b_mau_act,w_mau_act, s_mau_morph,
                         w_mau_morph,b_mau_morph, s_pau_morph, w_pau_morph,
                         b_pau_morph, s_pau_act,b_pau_act, w_pau_act )

raw_p_vector <- na.omit(as.vector(join_p_values$p.value))
# Now you can adjust them safely
adjusted_p <- p.adjust(raw_p_vector, method = "holm")
join_p_values$adjusted_p<-adjusted_p
