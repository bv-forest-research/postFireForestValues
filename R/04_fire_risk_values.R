#calculate fuel and fire risk 

# Load libraries
library(data.table)
library(tidyverse)

library(treeCalcs)

in_dir <- "01_data_inputs"
out_dir <- "02_prepped_values"

# Treatments -------------------------------------------------------------------------
FR_treatments <- fread(file.path(in_dir,"FR_Treatments.csv"))

#Plot treatment cleaning
FR_treatments[,`:=`(PlotID = as.factor(ID), Planted = as.factor(Planted))]
FR_treatments[, TimeSinceFire := 2020 - FIRE_YEAR]
#for this paper, we don't need all the columns:
plot_treatments <- FR_treatments[,.(PlotID, Planted, TimeSinceFire)]


# Fuels 
fuels_dt <- fread(file.path(in_dir,"ccpDF-CCP_AllWx_Results_CFIS_O1btoS1_RegenToSFC.csv"))
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

#means crown fire probability etc based on percentile fire weather:
ccp_50 <- fuels_dt[percentile > 49 & percentile < 51,
                   .(mn_50_ccp = mean(probability_ccp)),
                   by = "Plot"]

ccp_75 <- fuels_dt[percentile > 74 & percentile < 76,
                   .(mn_75_ccp = mean(probability_ccp)),
                   by = "Plot"]

ccp_90 <- fuels_dt[percentile > 89 & percentile < 91,
                   .(mn_90_ccp = mean(probability_ccp)),
                   by = "Plot"]

ccp_95 <- fuels_dt[percentile > 94 & percentile < 96,
                   .(mn_95_ccp = mean(probability_ccp)),
                   by = "Plot"]

ccp_50_75 <- merge(ccp_50, ccp_75, by = c("Plot"))
ccp_50_75_90 <- merge(ccp_50_75, ccp_90, by = c("Plot"))
ccp_50_75_90_95 <- merge(ccp_50_75_90, ccp_95, by = c("Plot"))

#means head fire intensity based on percentile fire weather:
hfi_50 <- fuels_dt[percentile > 49 & percentile < 51,
                   .(mn_50_hfi = mean(hfi)),
                   by = "Plot"]

hfi_75 <- fuels_dt[percentile > 74 & percentile < 76,
                   .(mn_75_hfi = mean(hfi)),
                   by = "Plot"]

hfi_90 <- fuels_dt[percentile > 89 & percentile < 91,
                   .(mn_90_hfi = mean(hfi)),
                   by = "Plot"]

hfi_95 <- fuels_dt[percentile > 94 & percentile < 96,
                   .(mn_95_hfi = mean(hfi)),
                   by = "Plot"]


hfi_50_75 <- merge(hfi_50, hfi_75, by = c("Plot"))
hfi_50_75_90 <- merge(hfi_50_75, hfi_90, by = c("Plot"))
hfi_50_75_90_95 <- merge(hfi_50_75_90, hfi_95, by = c("Plot"))

hfi_ccp <- merge(ccp_50_75_90_95, hfi_50_75_90_95, by = "Plot")

setnames(hfi_ccp, "Plot","PlotID")
#rescale even if between 0 and 1??? 
scale_fn <- function(var){(var - min(var)) / (max(var) - min(var))}
cnames <- c("mn_50_hfi","mn_75_hfi","mn_90_hfi","mn_95_hfi")
hfi_ccp[, (cnames) := lapply(.SD, scale_fn), .SDcols = cnames]

write.csv(hfi_ccp, "./02-prepped_values/hfi_ccp.csv",row.names = F)



#proportion of planted and not-planted fire types under different FWI percentiles
ft_50 <- merge(fuels_dt[percentile > 49 & percentile < 51,
                        .(ft_50 = .N), by = .(Planted, TSF, fire_type_ccp)],
               fuels_dt[percentile > 49 & percentile < 51,
                        .N, by = .(Planted, TSF)], 
               by = c("Planted","TSF"))
ft_50[, prop_ft_50 := ft_50/N]
ft_50[,`:=`(ft_50 = NULL, N = NULL)]


ft_75 <- merge(fuels_dt[percentile > 74 & percentile < 76,
                        .(ft_75 = .N),
                        by = .(Planted, TSF, fire_type_ccp)],
               fuels_dt[percentile > 74 & percentile < 76,
                        .N, by = .(Planted, TSF)],
               by = c("Planted", "TSF"))
ft_75[, prop_ft_75 := ft_75/N]
ft_75[,`:=`(ft_75 = NULL, N = NULL)]


ft_90 <- merge(fuels_dt[percentile > 89 & percentile < 91,
                        .(ft_90 = .N),
                        by = .(Planted, TSF, fire_type_ccp)],
               fuels_dt[percentile > 89 & percentile < 91,
                        .N, by = .(Planted, TSF)],
               by = c("Planted", "TSF"))
ft_90[, prop_ft_90 := ft_90/N]
ft_90[,`:=`(ft_90 = NULL, N = NULL)]


ft_95 <- merge(fuels_dt[percentile > 94 & percentile < 96,
                        .(ft_95 = .N),
                        by = .(Planted, TSF, fire_type_ccp)],
               fuels_dt[percentile > 94 & percentile < 96,
                        .N, by = .(Planted, TSF)],
               by = c("Planted", "TSF"))
ft_95[, prop_ft_95 := ft_95/N]
ft_95[,`:=`(ft_95 = NULL, N = NULL)]


ft_50_75 <- merge(ft_50, ft_75, by = c("Planted", "TSF","fire_type_ccp"))
ft_50_75_90 <- merge(ft_50_75, ft_90, by = c("Planted", "TSF","fire_type_ccp"))
ft_50_75_90_95 <- merge(ft_50_75_90, ft_95, by = c("Planted", "TSF","fire_type_ccp"))
ft <- melt(ft_50_75_90_95, id.vars = c("Planted", "TSF", "fire_type_ccp"),
           variable.name = "fwi_percentile",
           value.name = "proportion")
ft[, fwi_percentile := as.factor(gsub("prop_ft_", "fwi_", fwi_percentile))]

ggplot(ft)+
  geom_bar(aes(x = Planted,y = proportion, fill = fire_type_ccp), stat = "identity")+
  facet_grid(c("fwi_percentile","TSF"))

ggplot(ft[fwi_percentile == "fwi_90"])+
  geom_bar(aes(x = Planted,y = proportion, fill = fire_type_ccp), stat = "identity")+
  facet_grid(.~TSF)+
  theme_minimal() +
  # ylab(expression("Seedlings (< 1.3 m)" ~ ha^-1 ~ "(log scale)"))+
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))

