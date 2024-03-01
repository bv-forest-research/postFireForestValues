
mem_fit_and_extract <- function(var_names, data) {
  form_p <- paste(var_names, "~ Planted + TimeSinceFire + (1|FIRE_NAME)")
  gl_p <- lmer(formula = paste(form_p, collapse = " "),
               data = data)
  
  form_np <- paste(var_names, "~ TimeSinceFire + (1|FIRE_NAME)")
  gl_np <- lmer(formula = paste(form_np, collapse = " "),
                data = data)
  
  # Create a data.table with extracted information
  
  summary_dt <- data.table(
    Variable = var_names,
    Planting = round(summary(gl_p)$coefficients["PlantedP",1],3),
    TimeSincFire_pl = round(summary(gl_p)$coefficients["TimeSinceFire",1],3),
    AIC_pl = AIC(gl_p),
    Rsq_pl_m = rsq.lmm(gl_p)$model,
    Rsq_pl_f = rsq.lmm(gl_p)$fixed,
    Rsq_pl_r = rsq.lmm(gl_p)$random,
    TimeSinceFire_NPeff = round(summary(gl_np)$coefficients["TimeSinceFire",1],3),
    AIC_NPeff = AIC(gl_np),
    Rsq_NPeff_m = rsq.lmm(gl_np)$model,
    Rsq_NPeff_f = rsq.lmm(gl_np)$fixed,
    Rsq_NPeffl_r = rsq.lmm(gl_np)$random
  )
  #suppressWarnings()
  return(summary_dt)
}

lm_fit_and_extract <- function(var_names, data) {
  form <- paste(var_names, "~ Planted + TimeSinceFire")
  gl <- lm(formula = paste(form, collapse = " "),
           data = data)
  
  
  # Create a data.table with extracted information
  summary_dt <- data.table(
    Variable = var_names,
    Treatment = round(summary(gl)$coefficients["PlantedP",1],3),
    Treatment_p_val = round(summary(gl)$coefficients["PlantedP",4],3),
    TimeSinceFire = round(summary(gl)$coefficients["TimeSinceFire",1],3),
    TimeSinceFire_p_val = round(summary(gl)$coefficients["TimeSinceFire",4],3),
    R_Squared = summary(gl)$r.sq
  )
  #suppressWarnings()
  return(summary_dt)
}

gam_fit_and_extract <- function(variable_name, data) {
  form <- paste(variable_name, "~ Planted + s(TimeSinceFire, bs = 'cr')")
  gm <- gam(formula = as.formula(form),
            data = data,
            family = gaussian(link = "identity"),
            method = "REML")
  
  
  summary_dt <- data.table(
    Variable = variable_name,
    Treatment = round(summary(gm)$p.table["PlantedP",1],3),
    Treatment_p_val = round(summary(gm)$p.table["PlantedP",4],3),
    TimeSinceFire = round(summary(gm)$s.table[,1],3),
    TimeSinceFire_p_val = round(summary(gm)$s.table[,4],3),
    R_Squared = round(summary(gm)$r.sq,2)
  )
  #suppressWarnings()
  return(summary_dt)
}

gam_beta_fit_and_extract <- function(variable_name, data) {
  form <- paste(variable_name, "~ Planted + s(TimeSinceFire, bs = 'cr')")
  gm <- gam(formula = as.formula(form),
            data = data,
            family = betar(link = "logit"),
            method = "REML")
  
  
  summary_dt <- data.table(
    Variable = variable_name,
    Treatment = round(summary(gm)$p.table["PlantedP",1],3),
    Treatment_p_val = round(summary(gm)$p.table["PlantedP",4],3),
    TimeSinceFire = round(summary(gm)$s.table[,1],3),
    TimeSinceFire_p_val = round(summary(gm)$s.table[,4],3),
    R_Squared = round(summary(gm)$r.sq,2)
  )
  #suppressWarnings()
  return(summary_dt)
}

#using betar - for data that ranges from 0 to 1
beta_fit_and_extract <- function(var_names, data) {
  form <- paste(var_names, "~ Planted + TimeSinceFire")
  gl <- betareg::betareg(formula = paste(form, collapse = " "),
                         data = data)
  
  
  # Create a data.table with extracted information
  summary_dt <- data.table(
    Variable = var_names,
    Treatment = round(summary(gl)$coefficients$mean["PlantedP",1],3),
    Treatment_p_val = round(summary(gl)$coefficients$mean["PlantedP",4],3),
    TimeSinceFire = round(summary(gl)$coefficients$mean["TimeSinceFire",1],3),
    TimeSinceFire_p_val = round(summary(gl)$coefficients$mean["TimeSinceFire",4],3),
    R_Squared = summary(gl)$r.sq
  )
  #suppressWarnings()
  return(summary_dt)
}