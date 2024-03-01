#calculate fuel and fire risk 

# Load libraries
library(data.table)
library(ggplot2)
library(lubridate)

in_dir <- "01_data_inputs"
out_dir <- "02_prepped_values"

# Treatments -------------------------------------------------------------------------
FR_treatments <- fread(file.path(in_dir,"FR_Treatments.csv"))

#Plot treatment cleaning
FR_treatments[,`:=`(PlotID = as.factor(ID), Planted = as.factor(Planted))]
FR_treatments[, TimeSinceFire := 2020 - FIRE_YEAR]
#for this paper, we don't need all the columns:
plot_treatments <- FR_treatments[,.(PlotID, Planted, TimeSinceFire,FIRE_NAME)]


# Fuels 
#fuels_dt <- fread(file.path(in_dir,"ccpDF-CCP_AllWx_Results_CFIS_O1btoS1_RegenToSFC.csv"))
fuels_dt <- fread(file.path(in_dir,"ccpDF_modified-CCP_AllWx_Results_SFI-Consume_02202024.csv"))
fuels_dt <- merge(fuels_dt, plot_treatments, 
                  by.x = "Plot", by.y = "PlotID", all.x = TRUE, allow.cartesian = TRUE)
fuels_dt[, ymd_wx := ymd(wxDate)]
fuels_dt[, m_wx := month(ymd_wx)]
fuels_dt[, d_wx := day(ymd_wx)]
fuels_dt[, md_wx := m_wx*100 + d_wx]

fuels_dt[,TSF := ifelse(TimeSinceFire <= 10, "<10",
                        ifelse(TimeSinceFire <= 20, "10-20",
                               ifelse(TimeSinceFire <= 40, "20-40",
                                      "40-60+")))]

#surface fire intensity:
fuels_dt[, sfi := 18000 * hfros * sfc / 60]

#all carbon dioxide emmissions (not sure if I should include monoxide)
fuels_dt[, co2 := CO2F + CO2S]

#means crown fire probability based on percentile fire weather -----------------------------
fire_vals <- fuels_dt[, .(
  mn_50_ccp = mean(probability_ccp[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_ccp = mean(probability_ccp[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_ccp = mean(probability_ccp[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_ccp = mean(probability_ccp[percentile > 94 & percentile < 96], na.rm = TRUE),
  mn_50_hfi = mean(hfi[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_hfi = mean(hfi[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_hfi = mean(hfi[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_hfi = mean(hfi[percentile > 94 & percentile < 96], na.rm = TRUE),
  mn_50_sfi = mean(sfi[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_sfi = mean(sfi[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_sfi = mean(sfi[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_sfi = mean(sfi[percentile > 94 & percentile < 96], na.rm = TRUE),
  mn_50_mort = mean(fofem_propBAKilled[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_mort = mean(fofem_propBAKilled[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_mort = mean(fofem_propBAKilled[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_mort = mean(fofem_propBAKilled[percentile > 94 & percentile < 96], na.rm = TRUE),
  mn_50_CO2 = mean(co2[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_CO2 = mean(co2[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_CO2 = mean(co2[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_CO2 = mean(co2[percentile > 94 & percentile < 96], na.rm = TRUE),
  mn_50_preload = mean(LitPre[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_preload = mean(LitPre[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_preload = mean(LitPre[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_preload = mean(LitPre[percentile > 94 & percentile < 96], na.rm = TRUE)
  ), by = .(Plot)]

for (i in names(fire_vals))fire_vals[is.na(get(i)), (i):=0]
setnames(fire_vals, "Plot","PlotID")
#rescale even if between 0 and 1??? 
#scale_fn <- function(var){(var - min(var)) / (max(var) - min(var))}
#cnames <- colnames(fire_vals)[ colnames(fire_vals)!="PlotID"]
#fire_vals[, (cnames) := lapply(.SD, scale_fn), .SDcols = cnames]

fire_vals <- merge(plot_treatments, fire_vals, by = "PlotID")

fwrite(fire_vals, file.path(out_dir,"fire_values.csv"))

ind_names <- colnames(fire_vals)[!colnames(fire_vals) %in% c("PlotID",
                                                             "Planted",
                                                             "TimeSinceFire",
                                                             "FIRE_NAME")]

ind_table <- melt(fire_vals, id.vars = c("PlotID","Planted","TimeSinceFire", "FIRE_NAME"),
                  measure.vars = ind_names)

var_incl <- c("mn_90_ccp","mn_90_hfi", "mn_90_sfi", "mn_90_mort",
              "mn_90_CO2")
ind_table[, c("var1", "var_type") := tstrsplit(variable, "(?<=[0-9])_", perl = TRUE)]

graph_ind <- ind_table[grepl(paste(c("ccp","hfi","sfi","mort","CO2"), collapse = "|"),
                             variable)]

#break it down:
contrasting_colors <- c("darkblue", "#E7B800","#4DAF4A","darkred")
ggplot(graph_ind)+
  geom_point(aes(x = TimeSinceFire, y = value, colour = var1), alpha = 0.5)+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = var1), 
              alpha = 0, method = "lm")+
  labs(color = "Values")+
  scale_color_manual(labels = c("50th", "75th", "90th", "95th"), 
                     values = contrasting_colors)+
  xlab("Time since fire")+
  ylab("Fuel and fire effects")+
  facet_grid(c("var_type","Planted"),scales = "free_y")+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))


#proportions -------------------------------------------------------------------------------------
# Fire types --------------------------
percentiles <- c(50, 75, 90, 95)
full_grid <- CJ(Planted = unique(fuels_dt$Planted),
                TSF = unique(fuels_dt$TSF),
                fire_type = unique(fuels_dt$fire_type))

ft_list <- list()
for (p in percentiles) {
  ft <- merge(
    fuels_dt[percentile > (p - 1) & percentile < (p + 1),
             .(ft = .N), by = .(Planted, TSF, fire_type)],
    full_grid,
    by = c("Planted", "TSF","fire_type"),
    all.y = TRUE
  )
  ft[is.na(ft), ft:=0]
  ft <- merge(
    ft,
    fuels_dt[percentile > (p - 1) & percentile < (p + 1),
             .N, by = .(Planted, TSF)],
    by = c("Planted", "TSF"),
    all.x = TRUE
  )
  col_name <- paste0("ft_", p)
  ft[, prop := ft/N][, c("ft", "N") := NULL]
  setnames(ft, "prop", col_name)
  ft_list[[col_name]] <- ft
}

ft_result <- Reduce(function(x, y) merge(x, y, by = c("Planted", "TSF", "fire_type")), ft_list)
ft <- melt(ft_result, id.vars = c("Planted", "TSF", "fire_type"),
                    variable.name = "fwi_percentile",
                    value.name = "proportion")
ft[, fwi_percentile := as.factor(gsub("ft_", "fwi_", fwi_percentile))]

ggplot(ft)+
  geom_bar(aes(x = Planted,y = proportion, fill = fire_type), stat = "identity")+
  facet_grid(c("fwi_percentile","TSF"))

ggplot(ft[fwi_percentile == "fwi_90"])+
  geom_bar(aes(x = Planted,y = proportion, fill = fire_type), stat = "identity")+
  facet_grid(.~TSF)+
  theme_minimal() +
  # ylab(expression("Seedlings (< 1.3 m)" ~ ha^-1 ~ "(log scale)"))+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))


# Fire severity -----------------------
full_grid <- CJ(Planted = unique(fuels_dt$Planted),
                TSF = unique(fuels_dt$TSF),
                fofem_severity = unique(fuels_dt$fofem_severity))

fs_list <- list()
for (p in percentiles) {
  fs <- merge(
    fuels_dt[percentile > (p - 1) & percentile < (p + 1),
             .(fs = .N), by = .(Planted, TSF, fofem_severity)],
    full_grid,
    by = c("Planted", "TSF","fofem_severity"),
    all.y = TRUE
  )
  fs[is.na(fs), fs:=0]
  fs <- merge(
    fs,
    fuels_dt[percentile > (p - 1) & percentile < (p + 1),
             .N, by = .(Planted, TSF)],
    by = c("Planted", "TSF"),
    all.x = TRUE
  )
  
  col_name <- paste0("fs_", p)
  fs[, prop := fs/N][, c("fs", "N") := NULL]
  setnames(fs, "prop", col_name)
  fs_list[[col_name]] <- fs
}

fs_result <- Reduce(function(x, y) merge(x, y, by = c("Planted", "TSF", "fofem_severity")),
                    fs_list)
fs <- melt(fs_result, id.vars = c("Planted", "TSF", "fofem_severity"),
           variable.name = "fwi_percentile",
           value.name = "proportion")
fs[, fwi_percentile := as.factor(gsub("fs_", "fwi_", fwi_percentile))]
fs[, fofem_severity:=factor(fofem_severity, levels = c("no trees","low","mod",
                                                      "high"))]
ggplot(fs)+
  geom_bar(aes(x = Planted,y = proportion, fill = fofem_severity), stat = "identity")+
  scale_fill_manual(
    values  = c("no trees" = "darkgreen","low" = "lightyellow","mod"= "orange","high" = "darkred"))+
  facet_grid(c("fwi_percentile","TSF"))

ggplot(fs[fwi_percentile == "fwi_90"])+
  geom_bar(aes(x = Planted,y = proportion, fill = fofem_severity), stat = "identity")+
  facet_grid(.~TSF)+
  theme_minimal() +
  scale_fill_manual(
    values  = c("no trees" = "darkgreen","low" = "lightyellow","mod"= "orange","high" = "darkred"))+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))



#Analysis ----------------------------------------------------------------------
library(betareg)
library(lme4)
library(rsq)
library(mgcv)

source("./R/00-utils/models.R")

#beta regression - constant to avoid actual 0 and 1
vars_scale <- c("mn_50_mort","mn_75_mort", "mn_90_mort", "mn_95_mort",
                "mn_50_ccp","mn_75_ccp", "mn_90_ccp", "mn_95_ccp")

fire_vals[, paste0((vars_scale),"_s") :=
            lapply(.SD, function(x) {
              ifelse(x == 0, x + 0.00001,  
                     ifelse(x == 1, x - 0.00001, x))  
            }), .SDcols = vars_scale]

var_incl_s <- c("mn_50_mort_s","mn_75_mort_s", "mn_90_mort_s", "mn_95_mort_s")
beta_res <- lapply(var_incl_s, beta_fit_and_extract, data = fire_vals)
beta_results <- dplyr::bind_rows(beta_res)
var_incl <- c("mn_50_mort","mn_75_mort", "mn_90_mort", "mn_95_mort")
gam_res <- lapply(var_incl_s, gam_beta_fit_and_extract, data = fire_vals)
gam_results <- dplyr::bind_rows(gam_res)


var_incl_s <- c("mn_50_ccp_s","mn_75_ccp_s", "mn_90_ccp_s", "mn_95_ccp_s")
beta_res <- lapply(var_incl_s, beta_fit_and_extract, data = fire_vals)
beta_results <- dplyr::bind_rows(beta_res)
var_incl <- c("mn_50_ccp","mn_75_ccp", "mn_90_ccp", "mn_95_ccp")
gam_res <- lapply(var_incl_s, gam_beta_fit_and_extract, data = fire_vals)
gam_results <- dplyr::bind_rows(gam_res)


var_incl <- c("mn_50_CO2","mn_75_CO2", "mn_90_CO2", "mn_95_CO2")
lm_res <- lapply(var_incl, lm_fit_and_extract, data = fire_vals)
lm_results <- dplyr::bind_rows(lm_res)
gam_res <- lapply(var_incl, gam_fit_and_extract, data = fire_vals)
gam_results <- dplyr::bind_rows(gam_res)

var_incl <- c("mn_50_sfi","mn_75_sfi", "mn_90_sfi", "mn_95_sfi")
lm_res <- lapply(var_incl, lm_fit_and_extract, data = fire_vals)
lm_results <- dplyr::bind_rows(lm_res)
gam_res <- lapply(var_incl, gam_fit_and_extract, data = fire_vals)
gam_results <- dplyr::bind_rows(gam_res)

var_incl <- c("mn_50_hfi","mn_75_hfi", "mn_90_hfi", "mn_95_hfi")
lm_res <- lapply(var_incl, lm_fit_and_extract, data = fire_vals)
lm_results <- dplyr::bind_rows(lm_res)
gam_res <- lapply(var_incl, gam_fit_and_extract, data = fire_vals)
gam_results <- dplyr::bind_rows(gam_res)


var_incl <- c("mn_50_preload","mn_75_preload", "mn_90_preload", "mn_95_preload")

# emissions deep dive ----------------------------------------------------------------------------
#trying to figure out the emissions stuff:
pre_vals <- fuels_dt[, .(
  mn_50_DW1Pre = mean(DW1Pre[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_DW1Pre = mean(DW1Pre[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_DW1Pre = mean(DW1Pre[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_DW1Pre = mean(DW1Pre[percentile > 94 & percentile < 96], na.rm = TRUE),
  mn_50_DW10Pre = mean(DW10Pre[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_DW10Pre = mean(DW10Pre[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_DW10Pre = mean(DW10Pre[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_DW10Pre = mean(DW10Pre[percentile > 94 & percentile < 96], na.rm = TRUE),
  mn_50_DW100Pre = mean(DW100Pre[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_DW100Pre = mean(DW100Pre[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_DW100Pre = mean(DW100Pre[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_DW100Pre = mean(DW100Pre[percentile > 94 & percentile < 96], na.rm = TRUE),
  mn_50_DW1kSndPre = mean(DW1kSndPre[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_DW1kSndPre = mean(DW1kSndPre[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_DW1kSndPre = mean(DW1kSndPre[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_DW1kSndPre = mean(DW1kSndPre[percentile > 94 & percentile < 96], na.rm = TRUE),
  mn_50_DW1kRotPre = mean(DW1kRotPre[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_DW1kRotPre = mean(DW1kRotPre[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_DW1kRotPre = mean(DW1kRotPre[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_DW1kRotPre = mean(DW1kRotPre[percentile > 94 & percentile < 96], na.rm = TRUE)
), by = .(Plot)]

for (i in names(pre_vals))pre_vals[is.na(get(i)), (i):=0]
setnames(pre_vals, "Plot","PlotID")
pre_vals <- merge(plot_treatments, pre_vals, by = "PlotID")

pre_names <- colnames(pre_vals)[!colnames(pre_vals) %in% c("PlotID",
                                                             "Planted",
                                                             "TimeSinceFire",
                                                             "FIRE_NAME")]

pre_table <- melt(pre_vals, id.vars = c("PlotID","Planted","TimeSinceFire", "FIRE_NAME"),
                  measure.vars = pre_names)

pre_table[, c("var1", "var_type") := tstrsplit(variable, "(?<=[0-9])_", perl = TRUE)]


#break it down:
contrasting_colors <- c("darkblue", "#E7B800","#4DAF4A","darkred")
ggplot(pre_table)+
  geom_point(aes(x = TimeSinceFire, y = value, colour = variable), alpha = 0.5)+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = variable), 
              alpha = 0, method = "loess")+
  labs(color = "Values")+
  scale_color_manual(labels = c("50th", "75th", "90th", "95th"), 
                     values = contrasting_colors)+
  xlab("Time since fire")+
  ylab("Ecosystem Service Value")+
  facet_grid(c("Planted","var_type"))+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18),legend.position = "none")





#consumed:
#trying to figure out the emissions stuff:
Con_vals <- fuels_dt[, .(
  mn_50_DW1Con = mean(DW1Con[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_DW1Con = mean(DW1Con[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_DW1Con = mean(DW1Con[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_DW1Con = mean(DW1Con[percentile > 94 & percentile < 96], na.rm = TRUE),
  mn_50_DW10Con = mean(DW10Con[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_DW10Con = mean(DW10Con[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_DW10Con = mean(DW10Con[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_DW10Con = mean(DW10Con[percentile > 94 & percentile < 96], na.rm = TRUE),
  mn_50_DW100Con = mean(DW100Con[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_DW100Con = mean(DW100Con[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_DW100Con = mean(DW100Con[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_DW100Con = mean(DW100Con[percentile > 94 & percentile < 96], na.rm = TRUE),
  mn_50_DW1kSndCon = mean(DW1kSndCon[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_DW1kSndCon = mean(DW1kSndCon[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_DW1kSndCon = mean(DW1kSndCon[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_DW1kSndCon = mean(DW1kSndCon[percentile > 94 & percentile < 96], na.rm = TRUE),
  mn_50_DW1kRotCon = mean(DW1kRotCon[percentile > 49 & percentile < 51], na.rm = TRUE),
  mn_75_DW1kRotCon = mean(DW1kRotCon[percentile > 74 & percentile < 76], na.rm = TRUE),
  mn_90_DW1kRotCon = mean(DW1kRotCon[percentile > 89 & percentile < 91], na.rm = TRUE),
  mn_95_DW1kRotCon = mean(DW1kRotCon[percentile > 94 & percentile < 96], na.rm = TRUE)
), by = .(Plot)]

for (i in names(Con_vals))Con_vals[is.na(get(i)), (i):=0]
setnames(Con_vals, "Plot","PlotID")
Con_vals <- merge(plot_treatments, Con_vals, by = "PlotID")

Con_names <- colnames(Con_vals)[!colnames(Con_vals) %in% c("PlotID",
                                                           "Planted",
                                                           "TimeSinceFire",
                                                           "FIRE_NAME")]

Con_table <- melt(Con_vals, id.vars = c("PlotID","Planted","TimeSinceFire", "FIRE_NAME"),
                  measure.vars = Con_names)

Con_table[, c("var1", "var_type") := tstrsplit(variable, "(?<=[0-9])_", perl = TRUE)]


#break it down:
contrasting_colors <- c("darkblue", "#E7B800","#4DAF4A","darkred")
ggplot(Con_table)+
  geom_point(aes(x = TimeSinceFire, y = value, colour = var1), alpha = 0.5)+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = var1), 
              alpha = 0, method = "loess")+
  labs(color = "Values")+
  scale_color_manual(labels = c("50th", "75th", "90th", "95th"), 
                     values = contrasting_colors)+
  xlab("Time since fire")+
  ylab("Tons/acre")+
  facet_grid(c("var_type","Planted"),scales = "free_y")+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))

var_incl <- c("mn_50_CO2","mn_75_CO2", "mn_90_CO2", "mn_95_CO2")
lm_res <- lapply(var_incl, lm_fit_and_extract, data = pre_vals)
lm_results <- bind_rows(lm_res)
gam_res <- lapply(var_incl, gam_fit_and_extract, data = pre_vals)
gam_results <- bind_rows(gam_res)

var_incl <- c("mn_50_sfi","mn_75_sfi", "mn_90_sfi", "mn_95_sfi")
lm_res <- lapply(var_incl, lm_fit_and_extract, data = Con_vals)
lm_results <- bind_rows(lm_res)
gam_res <- lapply(var_incl, gam_fit_and_extract, data = Con_vals)
gam_results <- bind_rows(gam_res)

ggplot(Con_table[var_type == "DW1Con"])+
  geom_point(aes(x = TimeSinceFire, y = value, colour = var1), alpha = 0.5)+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = var1), 
              alpha = 0, method = "loess")+
  labs(color = "Values")+
  xlab("Time since fire")+
  ylab("Ecosystem Service Value")+
  facet_grid(c("var_type","Planted"))+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))

ggplot(Con_table[var_type == "DW10Con"])+
  geom_point(aes(x = TimeSinceFire, y = value, colour = var1), alpha = 0.5)+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = var1), 
              alpha = 0, method = "loess")+
  labs(color = "Values")+
  xlab("Time since fire")+
  ylab("Ecosystem Service Value")+
  facet_grid(c("var_type","Planted"))+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))

ggplot(Con_table[var_type == "DW100Con"])+
  geom_point(aes(x = TimeSinceFire, y = value, colour = var1), alpha = 0.5)+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = var1), 
              alpha = 0, method = "loess")+
  labs(color = "Values")+
  xlab("Time since fire")+
  ylab("Ecosystem Service Value")+
  facet_grid(c("var_type","Planted"))+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))

ggplot(Con_table[var_type == "DW1kSndCon"])+
  geom_point(aes(x = TimeSinceFire, y = value, colour = var1), alpha = 0.5)+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = var1), 
              alpha = 0, method = "loess")+
  labs(color = "Values")+
  xlab("Time since fire")+
  ylab("Ecosystem Service Value")+
  facet_grid(c("var_type","Planted"))+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))

ggplot(Con_table[var_type == "DW1kRotCon"])+
  geom_point(aes(x = TimeSinceFire, y = value, colour = var1), alpha = 0.5)+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = var1), 
              alpha = 0, method = "loess")+
  labs(color = "Values")+
  xlab("Time since fire")+
  ylab("Ecosystem Service Value")+
  facet_grid(c("var_type","Planted"))+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))
