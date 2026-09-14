# integrate across values
library(data.table)
library(ggplot2)
library(scico)
library(ggrepel)

in_dir <- "02_prepped_values"
out_dir <- "03_outputs"
#Schwenk et al 2012
#1. overall goal: manage forests for multiple values
#2. set objectives: objectives: 1. store carbon, 2. produce timber/wood products, 3. sustain biodiversity
#3. assign weights
#4. choose management prescriptions
#5. calculate partial utilities
#6. calculate total utilities



#1a. partial utilities carbon and timber

#The partial utility for C storage, UC,i,j, was calculated as the mean annual C stored during the
# 100-year simulation (at site i for prescription j ), divided by the mean annual C for the 
# site-prescription combination with the maximum C storage (considering all four prescriptions).

# mean annual C store not planted/ (max c (planted or not planted)) 

#1b. partial utlities biodiversity

#average occupancy for each bird species
#rescaled occupancy estimates so species with greater occupancy did not have a disproportionate influence
#then summed across all 51 species

# sum( mean occupancy not planted species 1/ max occupancy for not planted species 1) / max species occupancy
# across all prescriptions (planted or not planted)

#forest_ind_list <- list.files(in_dir, "indi", full.names = TRUE)
#fi <- purrr::map(forest_ind_list, fread)
#fi_dt <- do.call(function(x)merge(x, by = "PlotID"), fi)
#fi_dt <- data.table::rbindlist(fi, use.names = TRUE, fill = TRUE, idcol = "source")
# January 31 - there's still treatment errors to resolve - how can NM == PFO?
FR_treatments <- fread(file.path("01_data_inputs","FR_Treatments.csv"))

#Plot treatment cleaning
FR_treatments[,`:=`(PlotID = as.factor(ID), Planted = as.factor(Planted))]
FR_treatments[, TimeSinceFire := 2020 - FIRE_YEAR]
#for this paper, we don't need all the columns:
plot_treatments <- FR_treatments[,.(PlotID, Planted, TimeSinceFire)]
plot_treatments[,TSF := ifelse(TimeSinceFire <= 10, "<10",
                               ifelse(TimeSinceFire <= 20, "10-20",
                                      ifelse(TimeSinceFire <= 40, "20-40",
                                             "40-60+")))]
plot_treatments[, Planted := factor(Planted, levels = c("P", "NP"))]
pre_post_treat <- fread(file.path(in_dir, "jb_treatments.csv"))
#management class "SM" = some mix, "PFO" = post-fire only, "NM" = no management
plot_treatments[, management_class := pre_post_treat$management_class]

#some of these are raw and some are already centred and scaled

hb <- fread(list.files(in_dir, "hab", full.names = TRUE))
setnafill(hb, fill = 0, cols = colnames(hb)[grep("Habitat",colnames(hb))])
cr <- fread(list.files(in_dir, "carbon", full.names = TRUE))
fl <- fread(list.files(in_dir, "fire_values", full.names = TRUE))
tb <- fread(list.files(in_dir, "vol", full.names = TRUE))
la <- fread(list.files(in_dir, "layers", full.names = TRUE))
sa <- fread(file.path(in_dir, "plot_structure.csv"))
sa_min <- sa[
  ,lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
  .SDcols = c("snags_ha","qmd_dbh", "live_ba","can.open","top_height","sd_hgt",
              "stems_ha","Total_Herbs","ShrubsB2", "ShrubsB1","CQI", "sd_lcr","regen_mnSPH")
]
#sa_min <- data.table(scale(sa_min))
sa_min[, PlotID := sa$PlotID]

plot_attributes <- Reduce(
  function(x, y) merge(x, y, by = "PlotID", all = TRUE),
  list(
    #plot_treatments,
    hb,
    cr,
    fl,
    tb,
    la,
    sa_min
  )
)
plot_attributes <- plot_attributes[
  ,lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
  .SDcols = is.numeric
]
plot_attributes[, `:=`(PlotID = plot_treatments$PlotID, 
                       TSF = plot_treatments$TSF,
                       Planted = plot_treatments$Planted,
                       TimeSinceFire = plot_treatments$TimeSinceFire)]

# ---------------------------------------------------------------------------
# Treatment x time significance testing (MANOVA), run ahead of the MCDM
# utility calculations. Tests whether response values differ by management
# treatment, time since fire, and their interaction - both across all
# response values together and within ecological categories (fire risk,
# wildlife habitat, carbon, timber, stand structure) - so we don't have to
# assume a single simplified response (e.g. mean habitat utility) is valid.
# ---------------------------------------------------------------------------

manova_dt <- copy(plot_attributes)
manova_dt[, Planted := factor(plot_treatments$Planted)]
manova_dt[, management_class := factor(plot_treatments$management_class)]
manova_dt[, Planted_ord := factor(Planted, ordered = TRUE)]

habitat_species_cols <- c(
  "MartenHabitat", "FisherHabitat", "GoshawkHabitat",
  "HareHabitat", "SquirrelHabitat", "SmMammalHabitat",
  "GrouseHabitat", "GrizzlyHabitat"
)
fire_metrics <- c("ccp", "hfi", "sfi", "mort", "CO2", "preload")
weather_levels <- c("50", "75", "90", "95")

# One fire response set per weather percentile bin (50th/75th/90th/95th)
fire_response_categories <- setNames(
  lapply(weather_levels, function(w) intersect(paste0("mn_", w, "_", fire_metrics), names(manova_dt))),
  paste0("Fire_", weather_levels)
)

response_categories <- c(
  fire_response_categories,
  list(
    Wildlife = intersect(habitat_species_cols, names(manova_dt)),
    Carbon = intersect(c("TotalCarbon", "DeadCarbon", "LiveCarbon"), names(manova_dt)),
    Timber = intersect("MerchVol", names(manova_dt)),
    StandStructure = intersect(
      c("snags_ha","qmd_dbh", "live_ba","can.open","top_height","sd_hgt",
              "stems_ha","Total_Herbs","ShrubsB2", "ShrubsB1","CQI", "sd_lcr","regen_mnSPH"),
      names(manova_dt)
    )
  )
)
# "All" uses the 90th percentile fire set as representative, avoiding
# near-duplicate/collinear fire columns from the other weather percentiles
response_categories$All <- unique(unlist(
  response_categories[setdiff(names(response_categories), setdiff(names(fire_response_categories), "Fire_90"))]
))

# MANOVA fails on constant columns, so drop any zero-variance response per category
drop_constant <- function(vars, data) {
  vars[vapply(data[, ..vars], function(x) isTRUE(stats::var(x, na.rm = TRUE) > 0), logical(1))]
}

run_manova <- function(vars, data) {
  vars <- drop_constant(vars, data)
  if (length(vars) < 2) return(NULL)
  y <- as.matrix(data[, ..vars])
  fit <- tryCatch(
    stats::manova(y ~ Planted * TimeSinceFire, data = data),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  stats_mat <- summary(fit, test = "Pillai")$stats
  dt <- as.data.table(stats_mat, keep.rownames = "Term")
  dt[Term != ""]
}

manova_results <- rbindlist(
  lapply(names(response_categories), function(cat_name) {
    vars <- drop_constant(response_categories[[cat_name]], manova_dt)
    res <- run_manova(vars, manova_dt)
    if (is.null(res)) return(NULL)
    res[, `:=`(Category = cat_name, NResponses = length(vars))]
    res
  }),
  fill = TRUE
)
setnames(manova_results, "Pr(>F)", "p_value", skip_absent = TRUE)
setcolorder(manova_results, c("Category", "NResponses", "Term"))

# Univariate follow-up (GAM) - which individual responses drive any multivariate
# signal. Planted is fitted as an ordered factor with a shared smooth plus a
# "difference smooth" (Simpson 2018): the shared s(TimeSinceFire) tests the
# overall time trend, the Planted_ord parametric term tests the main effect,
# and the by-factor difference smooth tests the Planted x Time interaction -
# all three read directly from one model's summary table (no unreliable
# nested-model comparisons needed).
run_univariate <- function(var, data) {
  form <- stats::as.formula(paste0(
    "`", var, "` ~ Planted_ord + s(TimeSinceFire, k = 4) + s(TimeSinceFire, by = Planted_ord, k = 4)"
  ))
  fit <- tryCatch(mgcv::gam(form, data = data, method = "REML"), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  smry <- summary(fit)
  p_row <- smry$p.table["Planted_ord.L", ]
  s_tab <- smry$s.table

  # Direction of the Planted effect comes straight from its coefficient sign
  # (positive = Not planted higher). Direction of the time trend comes from
  # the shared smooth's fitted contribution at the min vs max observed time.
  time_range <- range(data$TimeSinceFire, na.rm = TRUE)
  newdata <- data.frame(
    TimeSinceFire = time_range,
    Planted_ord = factor(levels(data$Planted_ord)[1], levels = levels(data$Planted_ord))
  )
  term_pred <- tryCatch(predict(fit, newdata = newdata, type = "terms"), error = function(e) NULL)
  time_direction <- NA_real_
  if (!is.null(term_pred)) {
    time_col <- grep("^s\\(TimeSinceFire\\)$", colnames(term_pred))
    if (length(time_col) == 1) time_direction <- sign(term_pred[2, time_col] - term_pred[1, time_col])
  }

  data.table(
    Variable = var,
    Term = c("Planted", "TimeSinceFire", "Planted:TimeSinceFire"),
    F_value = c(unname(p_row["t value"])^2, s_tab[1, "F"], s_tab[2, "F"]),
    p_value = c(unname(p_row["Pr(>|t|)"]), s_tab[1, "p-value"], s_tab[2, "p-value"]),
    Direction = c(sign(unname(p_row["Estimate"])), time_direction, NA_real_)
  )
}

univariate_results <- rbindlist(
  lapply(setdiff(names(response_categories), "All"), function(cat_name) {
    vars <- drop_constant(response_categories[[cat_name]], manova_dt)
    res <- rbindlist(lapply(vars, run_univariate, data = manova_dt), fill = TRUE)
    if (nrow(res) == 0) return(NULL)
    res[, Category := cat_name]
    res
  }),
  fill = TRUE
)
setcolorder(univariate_results, c("Category", "Variable", "Term"))

fwrite(manova_results, file.path(out_dir, "manova_planted_time_summary.csv"))
fwrite(univariate_results, file.path(out_dir, "univariate_planted_time_summary.csv"))

# Figure summarizing, per individual response variable, the direction of each
# significant design effect: which way TimeSinceFire trends, and whether
# Planted or Not planted is higher. The interaction has no single direction,
# so it is just flagged as significant/not.
univariate_plot_dt <- copy(univariate_results)[!is.na(p_value)]
univariate_plot_dt[, Term := factor(
  Term,
  levels = c("Planted", "TimeSinceFire", "Planted:TimeSinceFire"),
  labels = c("Planted", "Time since fire", "Planted x Time")
)]
univariate_plot_dt[, Significant := p_value < 0.05]
univariate_plot_dt[, DirectionLabel := fifelse(
  !Significant, "n.s.",
  fifelse(
    Term == "Planted", fifelse(Direction > 0, "Higher: Not planted", "Higher: Planted"),
    fifelse(
      Term == "Time since fire", fifelse(Direction > 0, "Increasing over time", "Decreasing over time"),
      "Significant interaction"
    )
  )
)]
univariate_plot_dt[, DirectionLabel := factor(DirectionLabel, levels = c(
  "Higher: Not planted", "Higher: Planted",
  "Increasing over time", "Decreasing over time",
  "Significant interaction", "n.s."
))]
var_order <- univariate_plot_dt[, .(min_p = min(p_value, na.rm = TRUE)), by = Variable][order(-min_p), Variable]
univariate_plot_dt[, Variable := factor(Variable, levels = var_order)]

p_univariate_effects <- ggplot(univariate_plot_dt, aes(x = Term, y = Variable)) +
  geom_point(aes(colour = DirectionLabel, size = Significant, alpha = Significant)) +
  facet_grid(Category ~ ., scales = "free_y", space = "free_y") +
  scale_colour_manual(
    values = c(
      "Higher: Not planted" = "#4477AA",
      "Higher: Planted" = "#CC6677",
      "Increasing over time" = "#228833",
      "Decreasing over time" = "#AA3377",
      "Significant interaction" = "#EE7733",
      "n.s." = "grey80"
    ),
    name = "Direction / effect", drop = FALSE
  ) +
  scale_size_manual(values = c(`TRUE` = 4.5, `FALSE` = 2), guide = "none") +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.5), guide = "none") +
  theme_minimal(base_size = 12) +
  theme(
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 20, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Direction and significance of each design effect (GAM)",
    subtitle = "Point colour = direction of significant effects (p < 0.05); small faded points = not significant"
  )

ggsave(
  filename = "univariate_gam_effects_summary.jpg", plot = p_univariate_effects, path = out_dir,
  device = "jpeg", dpi = 300, bg = "white",
  width = 9.5, height = 0.28 * uniqueN(univariate_plot_dt$Variable) + 2
)


# MANOVA summary figure - Pillai's trace (tile shading) and significance (label)
# for each response category x model term
manova_plot_dt <- manova_results[Term != "Residuals"]
manova_plot_dt[, Term := factor(
  Term,
  levels = rev(c("Planted", "TimeSinceFire", "Planted:TimeSinceFire")),
  labels = rev(c("Treatment", "Time since fire", "Treatment x Time"))
)]
manova_plot_dt[, Significance := cut(
  p_value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("p < 0.001", "p < 0.01", "p < 0.05", "n.s.")
)]
# Order categories by strength of the treatment (Planted) effect
category_order <- manova_plot_dt[Term == "Treatment"][order(-Pillai), Category]
manova_plot_dt[, Category := factor(Category, levels = rev(category_order))]
manova_plot_dt <- manova_plot_dt[Category != "All"]

p_manova <- ggplot(manova_plot_dt, aes(x = Category, y = Pillai, fill = Term)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(
    aes(label = Significance),
    position = position_dodge(width = 0.75),
    hjust = -0.1,
    size = 3.2
  ) +
  coord_flip(ylim = c(0, 1.05)) +
  scale_fill_scico_d(palette = "berlin", name = "Model term") +
  theme_minimal(base_size = 14) +
  labs(
    x = NULL, y = "Pillai's trace",
    title = "MANOVA: strength of treatment x time effects by response category",
    subtitle = "Bar height = Pillai's trace (multivariate effect size); label = significance; categories ranked by treatment effect"
  )

ggsave(filename = "manova_treatment_time_summary.jpg", plot = p_manova, path = out_dir, device = "jpeg", dpi = 300, bg = "white", width = 11, height = 7)

# ---------------------------------------------------------------------------
# Multivariate ordination (RDA) of all response values together, constrained
# by treatment and time since fire - the biplot vectors show which individual
# responses drive the treatment/time separation detected by the MANOVA above.
# ---------------------------------------------------------------------------
ord_vars <- drop_constant(response_categories$All, manova_dt)
ord_y <- scale(as.matrix(manova_dt[, ..ord_vars]))

ord_fit <- vegan::rda(ord_y ~ Planted + TimeSinceFire, data = manova_dt)
ord_axis_pct <- round(100 * summary(ord_fit)$cont$importance[2, 1:2], 1)

ord_site_scores <- as.data.table(vegan::scores(ord_fit, display = "sites", scaling = 2, choices = 1:2))
ord_site_scores[, `:=`(
  PlotID = manova_dt$PlotID,
  Planted = manova_dt$Planted,
  TSF = manova_dt$TSF
)]

ord_loadings <- as.data.table(
  vegan::scores(ord_fit, display = "species", scaling = 2, choices = 1:2),
  keep.rownames = "Variable"
)

p_ordination <- ggplot(ord_site_scores, aes(x = RDA1, y = RDA2)) +
  stat_ellipse(
    aes(fill = Planted, group = Planted),
    geom = "polygon", alpha = 0.15, colour = NA, level = 0.68
  ) +
  geom_point(aes(colour = Planted, shape = TSF), size = 3, alpha = 0.85) +
  geom_segment(
    data = ord_loadings,
    aes(x = 0, y = 0, xend = RDA1 * 2, yend = RDA2 * 2),
    arrow = arrow(length = unit(0.15, "cm")), colour = "grey30", linewidth = 0.7
  ) +
  ggrepel::geom_text_repel(
    data = ord_loadings,
    aes(x = RDA1 * 2.2, y = RDA2 * 2.2, label = Variable),
    size = 3.3, colour = "grey20", max.overlaps = Inf
  ) +
  scale_colour_scico_d(palette = "berlin") +
  scale_fill_scico_d(palette = "berlin") +
  theme_minimal(base_size = 14) +
  labs(
    x = paste0("RDA1 (", ord_axis_pct[1], "%)"),
    y = paste0("RDA2 (", ord_axis_pct[2], "%)"),
    colour = "Planted", fill = "Planted", shape = "Time since fire",
    title = "Ordination of all response values (RDA)",
    subtitle = "Constrained by planted treatment and time since fire; arrows = response loadings"
  )

ggsave(filename = "manova_ordination_all_values.jpg", plot = p_ordination, path = out_dir, device = "jpeg", dpi = 300, bg = "white", width = 10, height = 7.5)

# ---------------------------------------------------------------------------
# Constrained ordination (RDA) of ecosystem values (fire, wildlife, carbon,
# timber - excluding stand structure) using stand structure attributes as the
# predictors, with management treatment and time-since-fire category overlaid
# to see how they align with the structure-driven ordination space.
# ---------------------------------------------------------------------------
value_response_vars <- drop_constant(
  unique(unlist(response_categories[c("Fire_90", "Wildlife", "Carbon", "Timber")])),
  manova_dt
)
structure_predictor_vars <- drop_constant(response_categories$StandStructure, manova_dt)

value_y <- scale(as.matrix(manova_dt[, ..value_response_vars]))
structure_x <- as.data.frame(scale(as.matrix(manova_dt[, ..structure_predictor_vars])))

structure_rda_fit <- vegan::rda(value_y ~ ., data = structure_x)
structure_rda_axis_pct <- round(100 * summary(structure_rda_fit)$cont$importance[2, 1:2], 1)

# Permutation tests for overall fit and each structure predictor's contribution
structure_rda_anova <- vegan::anova.cca(structure_rda_fit, permutations = 999)
structure_rda_anova_terms <- vegan::anova.cca(structure_rda_fit, by = "terms", permutations = 999)
vegan::vif.cca(structure_rda_fit)
fwrite(as.data.table(structure_rda_anova, keep.rownames = "Term"), file.path(out_dir, "rda_structure_predictors_overall.csv"))
fwrite(as.data.table(structure_rda_anova_terms, keep.rownames = "Term"), file.path(out_dir, "rda_structure_predictors_by_term.csv"))

structure_rda_site_scores <- as.data.table(vegan::scores(structure_rda_fit, display = "sites", scaling = 2, choices = 1:2))
structure_rda_site_scores[, `:=`(
  PlotID = manova_dt$PlotID,
  Planted = manova_dt$Planted,
  TSF = manova_dt$TSF
)]

structure_rda_response_loadings <- as.data.table(
  vegan::scores(structure_rda_fit, display = "species", scaling = 2, choices = 1:2),
  keep.rownames = "Variable"
)
structure_rda_predictor_loadings <- as.data.table(
  vegan::scores(structure_rda_fit, display = "bp", scaling = 2, choices = 1:2),
  keep.rownames = "Variable"
)

p_structure_rda <- ggplot(structure_rda_site_scores, aes(x = RDA1, y = RDA2)) +
  stat_ellipse(
    aes(colour = Planted, linetype = TSF, group = interaction(Planted, TSF)),
    linewidth = 0.6, level = 0.68
  ) +
  geom_point(aes(colour = Planted, shape = TSF), size = 3, alpha = 0.85) +
  geom_segment(
    data = structure_rda_response_loadings,
    aes(x = 0, y = 0, xend = RDA1 * 2, yend = RDA2 * 2),
    arrow = arrow(length = unit(0.12, "cm")), colour = "grey50", linewidth = 0.5, alpha = 0.7
  ) +
  ggrepel::geom_text_repel(
    data = structure_rda_response_loadings,
    aes(x = RDA1 * 2.2, y = RDA2 * 2.2, label = Variable),
    size = 3, colour = "grey40", max.overlaps = Inf
  ) +
  geom_segment(
    data = structure_rda_predictor_loadings,
    aes(x = 0, y = 0, xend = RDA1 * 2, yend = RDA2 * 2),
    arrow = arrow(length = unit(0.15, "cm")), colour = "#b2182b", linewidth = 0.9
  ) +
  ggrepel::geom_text_repel(
    data = structure_rda_predictor_loadings,
    aes(x = RDA1 * 2.3, y = RDA2 * 2.3, label = Variable),
    size = 3.4, colour = "#b2182b", fontface = "bold", max.overlaps = Inf
  ) +
  scale_colour_scico_d(palette = "berlin") +
  theme_minimal(base_size = 14) +
  labs(
    x = paste0("RDA1 (", structure_rda_axis_pct[1], "%)"),
    y = paste0("RDA2 (", structure_rda_axis_pct[2], "%)"),
    colour = "Planted", shape = "Time since fire", linetype = "Time since fire",
    title = "Ecosystem values ordinated by stand structure predictors",
    subtitle = "Grey arrows = value responses; red arrows = structure predictors; points/ellipses = planted x time"
  )

ggsave(filename = "rda_structure_predictors_ordination.jpg", plot = p_structure_rda, path = out_dir, device = "jpeg", dpi = 300, bg = "white", width = 10.5, height = 7.5)

# ---------------------------------------------------------------------------
# Does stand structure (sa_min variables) differ significantly by Planted,
# by management_class, or over time? Planted and management_class are tested
# in separate MANOVA models (they are confounded/overlapping groupings), each
# crossed with TimeSinceFire.
# ---------------------------------------------------------------------------
structure_vars_ss <- drop_constant(response_categories$StandStructure, manova_dt)
structure_y <- as.matrix(manova_dt[, ..structure_vars_ss])

run_structure_manova <- function(grouping_var) {
  form <- stats::as.formula(paste0("structure_y ~ ", grouping_var, " * TimeSinceFire"))
  fit <- stats::manova(form, data = manova_dt)
  stats_mat <- summary(fit, test = "Pillai")$stats
  dt <- as.data.table(stats_mat, keep.rownames = "Term")
  dt <- dt[Term != ""]
  dt[, Grouping := grouping_var]
  dt
}

structure_manova_results <- rbindlist(lapply(c("Planted", "management_class"), run_structure_manova))
setnames(structure_manova_results, "Pr(>F)", "p_value", skip_absent = TRUE)
fwrite(structure_manova_results, file.path(out_dir, "manova_stand_structure_planted_vs_management.csv"))


#-----------------------------------------------------------------------------------------------------------

# MCDM / Schwenk-style multi-criteria utility analysis
# This version keeps habitat values separate by species and evaluates fire
# risk across the 50/75/90/95th percentile weather conditions from the fire
# indicators dataset.

mcdm_dt <- copy(plot_attributes)
mcdm_dt[, management_class := plot_treatments$management_class]

scale01 <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- 0
  rng <- diff(range(x, na.rm = TRUE))
  if (isTRUE(all.equal(rng, 0))) {
    rep(0.5, length(x))
  } else {
    (x - min(x, na.rm = TRUE)) / rng
  }
}

# Species-specific habitat utilities
for (sp in habitat_species_cols) {
  scaled_col <- paste0(sp, "_scaled")
  util_col <- paste0(sp, "_Util")
  mcdm_dt[[scaled_col]] <- scale01(mcdm_dt[[sp]])
  mcdm_dt[[util_col]] <- mcdm_dt[[scaled_col]] / max(mcdm_dt[[scaled_col]], na.rm = TRUE)
}

# Carbon and timber utilities
mcdm_dt[, CarbonUtil := scale01(TotalCarbon)]
mcdm_dt[, TimberUtil := scale01(MerchVol)]
mcdm_dt[, `:=`(
  CarbonUtil = CarbonUtil / max(CarbonUtil, na.rm = TRUE),
  TimberUtil = TimberUtil / max(TimberUtil, na.rm = TRUE)
)]

# Fire-risk utilities across 50/75/90/95th weather conditions using multiple
# fire metrics from fl (ccp, hfi, sfi, mort, CO2, preload)
for (weather in weather_levels) {
  fire_cols <- paste0("mn_", weather, "_", fire_metrics)
  fire_cols <- intersect(fire_cols, names(mcdm_dt))

  if (length(fire_cols) > 0) {
    fire_risk_mat <- as.data.frame(lapply(fire_cols, function(col) scale01(mcdm_dt[[col]])))
    fire_risk_score <- rowMeans(fire_risk_mat, na.rm = TRUE)
    fire_util_score <- 1 - fire_risk_score

    mcdm_dt[[paste0("FireRisk_", weather)]] <- fire_risk_score
    mcdm_dt[[paste0("FireUtil_", weather)]] <- fire_util_score
  }
}

# Long-form fire metric table for more detailed sensitivity checks
fire_metric_long <- rbindlist(lapply(weather_levels, function(weather) {
  fire_cols <- paste0("mn_", weather, "_", fire_metrics)
  fire_cols <- intersect(fire_cols, names(mcdm_dt))

  rbindlist(lapply(fire_cols, function(col) {
    data.table(
      PlotID = mcdm_dt$PlotID,
      WeatherScenario = paste0("Weather_", weather),
      Metric = sub(paste0("^mn_", weather, "_"), "", col),
      RawValue = mcdm_dt[[col]],
      Utility = 1 - scale01(mcdm_dt[[col]])
    )
  }))
}))

# Weighting scenarios for the full MCDM calculation
weight_scenarios <- list(
  equal = c(Carbon = 0.25, Timber = 0.25, Biodiversity = 0.25, Fire = 0.25),
  carbon_focus = c(Carbon = 0.5, Timber = 0.15, Biodiversity = 0.2, Fire = 0.15),
  timber_focus = c(Carbon = 0.15, Timber = 0.5, Biodiversity = 0.2, Fire = 0.15),
  biodiversity_focus = c(Carbon = 0.15, Timber = 0.15, Biodiversity = 0.5, Fire = 0.2),
  biodiversity_high = c(Carbon = 0.1, Timber = 0.1, Biodiversity = 0.7, Fire = 0.1),
  fire_focus = c(Carbon = 0.15, Timber = 0.15, Biodiversity = 0.2, Fire = 0.5)
)

for (w_name in names(weight_scenarios)) {
  w <- weight_scenarios[[w_name]]
  species_share <- w["Biodiversity"] / length(habitat_species_cols)

  for (weather in weather_levels) {
    fire_col <- paste0("FireUtil_", weather)
    species_term <- rowSums(sapply(habitat_species_cols, function(sp) {
      mcdm_dt[[paste0(sp, "_Util")]] * species_share
    }))

    total_utility <- (
      w["Carbon"] * mcdm_dt$CarbonUtil +
        w["Timber"] * mcdm_dt$TimberUtil +
        species_term +
        w["Fire"] * mcdm_dt[[fire_col]]
    )

    mcdm_dt[[paste0("TotalUtility_", w_name, "_", weather)]] <- total_utility
  }
}

mcdm_dt[, ManagementGroup := paste(Planted, TSF, sep = "_")]
mcdm_dt[, MeanSpeciesUtil := rowMeans(.SD, na.rm = TRUE), .SDcols = paste0(habitat_species_cols, "_Util")]

mcdm_summary <- mcdm_dt[
  ,
  lapply(.SD, mean, na.rm = TRUE),
  by = .(management_class, Planted, TSF),
  .SDcols = c(
    paste0(habitat_species_cols, "_Util"),
    "CarbonUtil", "TimberUtil",
    paste0("FireUtil_", weather_levels),
    paste0("TotalUtility_", rep(names(weight_scenarios), each = length(weather_levels)), "_", rep(weather_levels, times = length(weight_scenarios)))
  )
]

# Species sensitivity to weight shifts
species_sensitivity <- rbindlist(lapply(habitat_species_cols, function(sp) {
  util_col <- paste0(sp, "_Util")
  base_weight <- weight_scenarios[["equal"]]["Biodiversity"] / length(habitat_species_cols)

  rbindlist(lapply(names(weight_scenarios), function(w_name) {
    species_weight <- weight_scenarios[[w_name]]["Biodiversity"] / length(habitat_species_cols)
    data.table(
      Species = sp,
      WeightScenario = w_name,
      MeanSpeciesUtility = mean(mcdm_dt[[util_col]], na.rm = TRUE),
      MeanWeightedContribution = mean(mcdm_dt[[util_col]] * species_weight, na.rm = TRUE),
      DeltaFromEqual = mean(mcdm_dt[[util_col]] * species_weight, na.rm = TRUE) -
        mean(mcdm_dt[[util_col]] * base_weight, na.rm = TRUE)
    )
  }))
}))

# Pairwise trade-off / synergy check across the species and fire criteria
criterion_cols <- c(
  "CarbonUtil", "TimberUtil",
  paste0(habitat_species_cols, "_Util"),
  paste0("FireUtil_", weather_levels)
)
cor_mat <- cor(mcdm_dt[, ..criterion_cols], use = "pairwise.complete.obs")
cor_dt <- as.data.table(cor_mat, keep.rownames = "Criterion")
cor_dt <- melt(cor_dt, id.vars = "Criterion", variable.name = "Criterion2", value.name = "Correlation")

# Time-aware summaries for TSF categories and continuous time trends
mcdm_time_summary <- mcdm_dt[
  , .(
    MeanCarbon = mean(CarbonUtil, na.rm = TRUE),
    MeanFire = mean(FireUtil_90, na.rm = TRUE),
    MeanTimber = mean(TimberUtil, na.rm = TRUE),
    MeanSpecies = mean(MeanSpeciesUtil, na.rm = TRUE),
    MeanTimeSinceFire = mean(TimeSinceFire, na.rm = TRUE),
    nPlots = .N
  ),
  by = .(TSF, Planted, management_class)
]

mcdm_time_long <- melt(
  mcdm_dt,
  id.vars = c("PlotID", "Planted", "management_class", "TimeSinceFire"),
  measure.vars = c("CarbonUtil", "TimberUtil", "FireUtil_90", "MeanSpeciesUtil"),
  variable.name = "Criterion",
  value.name = "Utility"
)
mcdm_time_long[, Criterion := factor(
  Criterion,
  levels = c("CarbonUtil", "TimberUtil", "FireUtil_90", "MeanSpeciesUtil"),
  labels = c("Carbon", "Timber", "Fire (90th)", "Mean species")
)]

# Biodiversity weight sweep to show how utility changes when biodiversity is up-weighted
biodiversity_weights <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)

biodiversity_sweep <- rbindlist(lapply(biodiversity_weights, function(bw) {
  remaining <- 1 - bw
  w <- c(Carbon = remaining / 3, Timber = remaining / 3, Biodiversity = bw, Fire = remaining / 3)
  species_term <- rowMeans(sapply(habitat_species_cols, function(sp) {
    mcdm_dt[[paste0(sp, "_Util")]]
  }), na.rm = TRUE) * w["Biodiversity"]
  total_utility <- (
    w["Carbon"] * mcdm_dt$CarbonUtil +
      w["Timber"] * mcdm_dt$TimberUtil +
      species_term +
      w["Fire"] * mcdm_dt$FireUtil_90
  )

  data.table(
    BiodiversityWeight = bw,
    MeanUtility = mean(total_utility, na.rm = TRUE),
    MeanCarbon = mean(mcdm_dt$CarbonUtil, na.rm = TRUE),
    MeanFire = mean(mcdm_dt$FireUtil_90, na.rm = TRUE),
    MeanSpecies = mean(mcdm_dt$MeanSpeciesUtil, na.rm = TRUE)
  )
}))

# Save the main outputs
fwrite(mcdm_summary, file.path(out_dir, "mcdm_summary.csv"))
fwrite(cor_dt, file.path(out_dir, "mcdm_tradeoff_correlations.csv"))
fwrite(fire_metric_long, file.path(out_dir, "mcdm_fire_metric_utilities.csv"))
fwrite(species_sensitivity, file.path(out_dir, "mcdm_species_sensitivity.csv"))
fwrite(mcdm_time_summary, file.path(out_dir, "mcdm_time_summary.csv"))
fwrite(biodiversity_sweep, file.path(out_dir, "mcdm_biodiversity_weight_sweep.csv"))
fwrite(
  mcdm_dt[, .(
    PlotID, Planted, TSF, management_class,
    CarbonUtil, TimberUtil,
    paste0(habitat_species_cols, "_Util"),
    paste0("FireUtil_", weather_levels),
    paste0("TotalUtility_", rep(names(weight_scenarios), each = length(weather_levels)), "_", rep(weather_levels, times = length(weight_scenarios)))
  )],
  file.path(out_dir, "mcdm_plot_scores.csv")
)

species_sensitivity[, WeightScenario := factor(
  WeightScenario,
  levels = names(weight_scenarios)
)]

# Species sensitivity as a line plot
p_species_sens <- ggplot(
  species_sensitivity,
  aes(x = WeightScenario, y = DeltaFromEqual, group = Species, colour = Species)
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_colour_scico_d(palette = "berlin") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Weighting scenario",
    y = "Change in weighted utility vs equal weights",
    colour = "Species",
    title = "Species sensitivity to weighting scenario"
  ) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

ggsave(filename = "mcdm_species_sensitivity_line.jpg", path = out_dir, device = "jpeg", dpi = 300, bg = "white")

# Species sensitivity as a heat map
p_species_heat <- ggplot(
  species_sensitivity,
  aes(x = WeightScenario, y = Species, fill = DeltaFromEqual)
) +
  geom_tile(colour = "white") +
  scale_fill_scico(palette = "berlin") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Weighting scenario",
    y = "Species",
    fill = "Delta vs equal",
    title = "Sensitivity of each species to weighting scenario"
  ) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

ggsave(filename = "mcdm_species_sensitivity_heatmap.jpg", path = out_dir, device = "jpeg", dpi = 300, bg = "white")

tradeoff_dt <- melt(
  mcdm_dt,
  id.vars = c("PlotID", "Planted", "TSF", "management_class", "CarbonUtil", "TimberUtil"),
  measure.vars = paste0("FireUtil_", weather_levels),
  variable.name = "WeatherScenario",
  value.name = "FireUtility"
)
tradeoff_dt[, WeatherScenario := factor(
  WeatherScenario,
  levels = paste0("FireUtil_", weather_levels),
  labels = paste0(weather_levels, "th percentile")
)]

p_tradeoff_planted <- ggplot(
  tradeoff_dt,
  aes(x = CarbonUtil, y = FireUtility, colour = TimberUtil, shape = Planted)
) +
  geom_point(size = 3, alpha = 0.85) +
  geom_smooth(
    aes(group = Planted, linetype = Planted),
    method = "lm",
    se = FALSE,
    linewidth = 0.9,
    colour = "grey30"
  ) +
  facet_wrap(~WeatherScenario) +
  scale_colour_gradientn(
    colours = scico(100, palette = "berlin"),
    name = "Timber utility"
  ) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Carbon utility",
    y = "Fire utility",
    colour = "Timber utility",
    shape = "Treatment",
    linetype = "Treatment",
    title = "Carbon vs fire trade-offs by planted status"
  )

ggsave(filename = "mcdm_tradeoffs_by_weather_planted.jpg", path = out_dir, device = "jpeg", dpi = 300, bg = "white")

p_tradeoff_management <- ggplot(
  tradeoff_dt,
  aes(x = CarbonUtil, y = FireUtility, colour = TimberUtil, shape = management_class)
) +
  geom_point(size = 3, alpha = 0.85) +
  geom_smooth(
    aes(group = management_class, linetype = management_class),
    method = "lm",
    se = FALSE,
    linewidth = 0.9,
    colour = "grey30"
  ) +
  facet_wrap(~WeatherScenario) +
  scale_colour_gradientn(
    colours = scico(100, palette = "berlin"),
    name = "Timber utility"
  ) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Carbon utility",
    y = "Fire utility",
    colour = "Timber utility",
    shape = "Management class",
    linetype = "Management class",
    title = "Carbon vs fire trade-offs by management class"
  )

ggsave(filename = "mcdm_tradeoffs_by_weather_management.jpg", path = out_dir, device = "jpeg", dpi = 300, bg = "white")

# Also save the planted and management figures as separate files with the same
# content so they can be reviewed independently without overloading a single panel.
# The main figures above are already split; this keeps the workflow explicit.

# Time-based gap between fire and carbon utility
mcdm_time_gap <- copy(mcdm_dt)
mcdm_time_gap[, FireCarbonGap := FireUtil_90 - CarbonUtil]

mcdm_time_gap_long <- melt(
  mcdm_time_gap,
  id.vars = c("PlotID", "TimeSinceFire", "Planted", "CarbonUtil"),
  measure.vars = paste0("FireUtil_", weather_levels),
  variable.name = "FireWeatherCategory",
  value.name = "FireUtility"
)
mcdm_time_gap_long[, FireWeatherCategory := factor(
  FireWeatherCategory,
  levels = paste0("FireUtil_", weather_levels),
  labels = paste0(weather_levels, "th percentile")
)]
mcdm_time_gap_long[, FireCarbonGap := FireUtility - CarbonUtil]

p_time_gap <- ggplot(
  mcdm_time_gap_long,
  aes(x = TimeSinceFire, y = FireCarbonGap, colour = Planted, linetype = Planted)
) +
  geom_point(alpha = 0.35, size = 2.0) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~FireWeatherCategory) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Time since fire",
    y = "Fire utility - carbon utility",
    colour = "Treatment",
    linetype = "Treatment",
    title = "Change in fire vs carbon utility through time"
  )

ggsave(filename = "mcdm_time_fire_carbon_gap.jpg", path = out_dir, device = "jpeg", dpi = 300, bg = "white")

# Time trend of key criteria by group
p_time_criteria <- ggplot(
  mcdm_time_long[Criterion %in% c("Carbon", "Fire (90th)"), ],
  aes(x = TimeSinceFire, y = Utility, colour = Criterion, linetype = Planted)
) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  #facet_wrap(~management_class) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Time since fire",
    y = "Utility",
    colour = "Criterion",
    linetype = "Treatment",
    title = "Carbon and fire utilities through time"
  )

ggsave(filename = "mcdm_time_criteria.jpg", path = out_dir, device = "jpeg", dpi = 300, bg = "white")

# Biodiversity-weight sweep figure
p_bio_sweep <- ggplot(
  biodiversity_sweep,
  aes(x = BiodiversityWeight, y = MeanUtility)
) +
  geom_line(linewidth = 1, colour = "#2166ac") +
  geom_point(size = 2.5, colour = "#2166ac") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Biodiversity weight",
    y = "Mean overall utility",
    title = "Sensitivity of overall utility to biodiversity weight"
  )

ggsave(filename = "mcdm_biodiversity_weight_sweep.jpg", path = out_dir, device = "jpeg", dpi = 300, bg = "white")


#hb_cr <- merge(hb, cr, by ="PlotID")
#hb_cr_fl <- merge(hb_cr, fl, by ="PlotID")
#hb_cr_fl_tb <- merge(hb_cr_fl, tb, by ="PlotID")
#hb_cr_fl_tb_sa <- merge(hb_cr_fl_tb, sa_min, by = "PlotID")


#FR_treatments <- fread(file.path("01_data_inputs","FR_Treatments.csv"))
#sa[ID == "FR41"|ID == "FR48"|ID == "FR50"|ID == "FR50"|ID == "FR60", 
 #             under_plant := "Y"][is.na(under_plant), under_plant := "N"]

#Plot treatment cleaning
#sa[,`:=`(PlotID = as.factor(ID), Planted = as.factor(Planted))]
#sa[, TimeSinceFire := 2020 - FIRE_YEAR]
#for this paper, we don't need all the columns:
#plot_treatments <- sa[,.(PlotID, Planted, TimeSinceFire, under_plant, management_class)]

#hb_cr_fl_tb_sa <- merge(plot_treatments, hb_cr_fl_tb_sa, by = "PlotID")
#hb_cr_fl_tb_sa[, management_class := ifelse(management_class == "Pre + Post fire","PPF",
 #                                           ifelse(management_class == "Post-fire only","PF",
  #                                                 "NM"))]

ind_names <- colnames(plot_attributes)[!colnames(plot_attributes) %in% c("PlotID",
                                                                 "Planted",
                                                                 "TimeSinceFire",
                                                                 "TSF")]
tr_names <- c("PlotID","Planted","TimeSinceFire", "TSF")

scale_dt <- plot_attributes[
  ,
  lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
  .SDcols = ind_names
]
scale_m <- scale(scale_dt, center = TRUE, scale = TRUE)
scale_dt <- as.data.table(scale_m)
scale_dt[, PlotID := FR_treatments$PlotID]
plot_sc <- Reduce(
  function(x, y) merge(x, y, by = "PlotID", all = TRUE),
  list(
    scale_dt,
    plot_treatments
  )
)
plot_long <- melt(plot_sc, 
                  id.vars = c("PlotID", "TSF", "Planted", "TimeSinceFire"), 
                  measure.vars = ind_names,
                  variable.name = "StructureVar",
                  value.name = "Value")



pca <- prcomp(scale_m, center = FALSE, scale. = FALSE)

scores <- as.data.table(pca$x[, 1:2])
scores[, PlotID := FR_treatments$PlotID]
scores_dt <- scores[
  plot_sc,
  on = "PlotID"
]

loadings <- as.data.table(pca$rotation[, 1:2], keep.rownames = "variable")

ggplot(scores_dt, aes(PC1, PC2)) +
  stat_ellipse(
    aes(fill = TSF, group = TSF),
    geom = "polygon",
    alpha = 0.18,
    colour = NA,
    level = 0.68
  ) +
  
  stat_ellipse(
    aes(linetype = Planted, group = Planted),
    colour = "black",
    linewidth = 0.9,
    level = 0.68
  ) +

  geom_point(
  aes(colour = TSF, shape = Planted),
  size = 3,
  alpha = 0.85
) +
  
  
  ## 4) Faded PCA loading vectors
  geom_segment(
    data = loadings,
    aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3),
    arrow = arrow(length = unit(0.15, "cm")),
    colour = "grey30",
    alpha = 0.55,
    linewidth = 0.8
  ) +
  
  #geom_text_repel(
  #  data = loadings,
   # aes(x = PC1 * 3.3, y = PC2 * 3.3, label = variable),
  #  size = 4,
  #  colour = "grey20",
  #  box.padding = 0.15,
  #  point.padding = 0.2,
  #  segment.color = "grey50",
  #  segment.size = 0.4,
  #  max.overlaps = Inf
  #) +
  ## Linetype control for planting
  scale_linetype_manual(
    values = c("P" = "dashed", "NP" = "solid")
  ) +
  
  scale_fill_scico_d(palette = "berlin")+
  
  scale_colour_scico_d(palette = "berlin")+
  theme_minimal(base_size = 15) +
  labs(
    x = paste0("PC1 (", round(100 * summary(pca)$importance[2,1], 1), "% variance)"),
    y = paste0("PC2 (", round(100 * summary(pca)$importance[2,2], 1), "% variance)")
  )+
  theme(
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12)
  )

sph_cols <- grep("^SPH_", names(plot_attributes), value = TRUE)
ba_cols  <- grep("^BA_",  names(plot_attributes), value = TRUE)

# Layer summaries
plot_attributes[, `:=`(
  SPH_Regen = rowSums(.SD, na.rm = TRUE),
  BA_Regen  = rowSums(.SD, na.rm = TRUE)
), .SDcols = grep("Layer4_Regen", c(sph_cols, ba_cols), value = TRUE)]

plot_attributes[, `:=`(
  SPH_Sapling = rowSums(.SD, na.rm = TRUE),
  BA_Sapling  = rowSums(.SD, na.rm = TRUE)
), .SDcols = grep("Layer3_Sapl", c(sph_cols, ba_cols), value = TRUE)]

habitat_vars <- c(
  "MartenHabitat", "FisherHabitat", "GoshawkHabitat",
  "GrouseHabitat", "SquirrelHabitat"
)
structure_vars <- grep("^(SPH|Layer)_", names(plot_attributes), value = TRUE)

#structure_vars <- c(
#  "SPH_Regen", "SPH_Sapling", "SPH_Pole", "SPH_Mature",
##  "BA_Regen", "BA_Sapling", "BA_Pole", "BA_Mature"
#)

library(glmnet)

X <- as.matrix(plot_attributes[, ..structure_vars])
Y <- as.matrix(plot_attributes[, ..habitat_vars])

X <- as.matrix(plot_attributes[, ..structure_vars])

lasso_hab <- lapply(habitat_vars, function(hab) {
  Y <- plot_attributes[[hab]]
  
  cv.glmnet(
    x = X,
    y = Y,
    alpha = 1,
    standardize = TRUE
  )
})

names(lasso_hab) <- habitat_vars

coef_dt <- rbindlist(lapply(names(lasso_hab), function(hab) {
  co <- coef(lasso_hab[[hab]], s = "lambda.min")
  dt <- as.data.table(as.matrix(co), keep.rownames = "StructureVar")
  setnames(dt, "lambda.min", "coef")
  dt[, Habitat := hab]
  dt[coef != 0 & StructureVar != "(Intercept)"]
}))

heat_dt <- coef_dt[
  ,
  .(selected = TRUE),
  by = .(StructureVar, Habitat)
]

ggplot(
  heat_dt,
  aes(x = Habitat, y = StructureVar, fill = selected)
) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("TRUE" = "#2166ac"),
    guide = "none"
  ) +
  labs(
    x = NULL,
    y = "Structural predictor",
    title = "Structural predictors selected by LASSO",
    subtitle = "Presence/absence across habitat models"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )














var_incl <- c("MartenHabitat","GoshawkHabitat", "HareHabitat", "SquirrelHabitat",
              "GrouseHabitat","FisherHabitat", "SmMammalHabitat","GrizzlyHabitat",
              "TotalCarbon", "DeadCarbon", "LiveCarbon",
              "MerchVol", "mn_50_hfi","mn_75_hfi","mn_90_hfi","mn_95_hfi", 
              "mn_50_ccp","mn_75_ccp","mn_90_ccp","mn_95_ccp")
scale_fn <- function(var){(var - min(var)) / (max(var) - min(var))}

plot_attributes[, (var_incl) := lapply(.SD, scale_fn), .SDcols = var_incl]

ind_table <- melt(plot_attributes, id.vars = c("PlotID","Planted","TimeSinceFire"),
                  measure.vars = ind_names)

ggplot(ind_table)+
  geom_point(aes(x = TimeSinceFire, y = value, colour = variable))+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = variable), alpha = 0.2, method = "gam")+
  xlab("Time since fire")+
  ylab("Ecosystem Service Value")+
  facet_wrap(~Planted)

#which fuels:
cnames <- c("mn_50_hfi","mn_75_hfi","mn_90_hfi","mn_95_hfi", 
            "mn_50_ccp","mn_75_ccp","mn_90_ccp","mn_95_ccp")
fire_ind <- ind_table[variable %in% cnames]

#head fire intensity
ggplot(fire_ind[variable %in% c("mn_50_hfi","mn_75_hfi","mn_90_hfi","mn_95_hfi")])+
  geom_point(aes(x = TimeSinceFire, y = value, colour = variable))+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = variable), alpha = 0, method = "loess")+
  xlab("Time since fire")+
  ylab("Head fire intensity")+
  facet_wrap(~Planted)
ggsave(filename = "head_fire_int.jpg",path = out_dir, device='jpeg', dpi=300, bg="white")

#head fire intensity
ggplot(fire_ind[variable %in% c("mn_50_ccp","mn_75_ccp","mn_90_ccp","mn_95_ccp")])+
  geom_point(aes(x = TimeSinceFire, y = value, colour = variable))+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = variable),linewidth = 1.5,
              alpha = 0, method = "lm")+
  xlab("Time since fire")+
  ylab("probability of crown fire")+
  facet_wrap(~Planted)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))
  
ggsave(filename = "prob_crown.jpg",path = out_dir, device='jpeg', dpi=300, bg="white")




### graphs of indices of interest:
graph_ind <- ind_table[variable %in% var_incl]


contrasting_colors <- c("#00AFBB", "#E7B800","#0072B2", "#FC4E07","#4DAF4A","#A65628",
                        "#CC79A7","#FF33CC","#8E7BAA","#999999","#E41A1C","#000000")





#break it down:
wildlife_ind <- c("MartenHabitat","GoshawkHabitat", "HareHabitat", "SquirrelHabitat",
                 "GrouseHabitat")
contrasting_colors <- c("#00AFBB", "#E7B800","#0072B2","#4DAF4A","#A65628")
ggplot(graph_ind[variable %in% wildlife_ind])+
  geom_point(aes(x = TimeSinceFire, y = value, colour = variable), alpha = 0.5)+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = variable), 
              alpha = 0, method = "lm")+
  labs(color = "Values")+
  scale_color_manual(labels = c("Marten", "Goshawk", "Hare", "Squirrel",
                                "Grouse"), 
                     values = contrasting_colors)+
  xlab("Time since fire")+
  ylab("Ecosystem Service Value")+
  facet_wrap(~Planted)+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))
ggsave(filename = "wildlife_values.jpg",path = out_dir, device='jpeg', dpi=300, bg="white")

carb_ind <- c("TotalCarbon", "DeadCarbon", "LiveCarbon")
contrasting_colors <- c("#FF33CC","#8E7BAA","#999999")
ggplot(graph_ind[variable %in% carb_ind])+
  geom_point(aes(x = TimeSinceFire, y = value, colour = variable), alpha = 0.5)+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = variable), 
              alpha = 0, method = "lm")+
  labs(color = "Values")+
  scale_color_manual(labels = c("Total carbon", "Dead carbon", "Live carbon"), 
                     values = contrasting_colors)+
  xlab("Time since fire")+
  ylab("Ecosystem Service Value")+
  facet_wrap(~Planted)+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))
ggsave(filename = "carb_values.jpg",path = out_dir, device='jpeg', dpi=300, bg="white")


merch_fuel_ind <- c("MerchVol", "mn_90_ccp")
contrasting_colors <- c("#E41A1C","#000000")
ggplot(graph_ind[variable %in% merch_fuel_ind])+
  geom_point(aes(x = TimeSinceFire, y = value, colour = variable), alpha = 0.5)+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = variable), 
              alpha = 0, method = "lm")+
  labs(color = "Values")+
  scale_color_manual(labels = c("mean prob crown fire 90th","merch vol"), 
                     values = contrasting_colors)+
  xlab("Time since fire")+
  ylab("Ecosystem Service Value")+
  facet_wrap(~Planted)+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))
ggsave(filename = "fuel_vol_values.jpg",path = out_dir, device='jpeg', dpi=300, bg="white")

#all together
ggplot(graph_ind)+
  geom_point(aes(x = TimeSinceFire, y = value, colour = variable), alpha = 0.2)+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = variable), 
              alpha = 0, method = "lm")+
  labs(color = "Values")+
  scale_color_hue() +
  xlab("Time since fire")+
  ylab("Ecosystem Service Value")+
  facet_wrap(~Planted)+
  theme_minimal()+
  theme(strip.text.x = element_text(face="bold"),text=element_text(size=18))
ggsave(filename = "merge_values.jpg",path = out_dir, device='jpeg', dpi=300, bg="white")













ind_sel <- ind_table[variable == "GoshawkHabitat"|
                       variable == "FisherHabitat"|
                       variable == "HareHabitat"|
                       variable == "GrouseHabitat"|
                       variable == "TotalCarbon"|
                       variable == "DeadCarbon"|
                       variable == "MerchVol"|
                       variable == "mn_ccp"]

ggplot(ind_sel)+
  geom_point(aes(x = TimeSinceFire, y = value, colour = variable))+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = variable), alpha = 0.2, method = "gam")+
  xlab("Time since fire")+
  ylab("Ecosystem Service Value")+
  facet_wrap(~Planted)

ind_sel <- ind_table[variable == "snags_ha"|
                       variable == "qmd_dbh"|
                       variable == "live_ba"|
                       variable == "can.open"]

ggplot(ind_sel)+
  geom_point(aes(x = TimeSinceFire, y = value, colour = variable))+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = variable), alpha = 0.2, method = "gam")+
  xlab("Time since fire")+
  ylab("Stand structure")+
  facet_wrap(~Planted)

sa_all <- sa[
  ,lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
  .SDcols = c("snags_ha","qmd_dbh", "live_ba","can.open",
              "large_trees_ha", "stems_ha",  "large_snags_ha",
              "regen_mnSPH", "regen_sdSPH", "sd_ba", "sd_dbh",
              "tree_sp_richness", "regen_sp_richness", "top_height",  "CQI")
]
sa_all <- data.table(scale(sa_all))
sa_all[, PlotID := sa$PlotID]
hb_cr_fl_tb_sa_all <- merge(hb_cr_fl, sa_all, by ="PlotID")
hb_cr_fl_tb_sa_all <- merge(plot_treatments, hb_cr_fl_tb_sa_all, by = "PlotID")
hb_cr_fl_tb_sa_all[, management_class := ifelse(management_class == "Pre + Post fire","PPF",
                                            ifelse(management_class == "Post-fire only","PF",
                                                   "NM"))]
struct_vars <- c(
  "large_trees_ha",
  "stems_ha",
  "snags_ha",
  "large_snags_ha",
  "regen_mnSPH",
  "regen_sdSPH",
  "sd_ba",
  "sd_dbh",
  "qmd_dbh",
  "tree_sp_richness",
  "regen_sp_richness",
  "live_ba",
  "top_height",
  "CQI",
  "can.open"
)
ind_names <- colnames(hb_cr_fl_tb_sa_all)[!colnames(hb_cr_fl_tb_sa_all) %in% c("PlotID",
                                                                       "Planted",
                                                                       "TimeSinceFire",
                                                                       "under_plant",
                                                                       "management_class")]
ind_table_all <- melt(hb_cr_fl_tb_sa_all, id.vars = c("PlotID","Planted","TimeSinceFire",
                                              "under_plant","management_class"),
                  measure.vars = ind_names)

ind_sel <- ind_table_all[variable == "snags_ha"|
                       variable == "qmd_dbh"|
                       variable == "live_ba"|
                       variable == "can.open"|
                       variable == "large_trees_ha"|
                       variable == "large_snags_ha"|
                       variable == "stems_ha"|
                       variable == "regen_mnSPH"|
                       variable == "regen_sdSPH"|
                       variable == "sd_ba"|
                       variable == "sd_dbh"|
                       variable == "tree_sp_richness"|
                       variable == "regen_sp_richness"|
                       variable == "top_height"|
                       variable == "CQI"]

ggplot(ind_sel)+
  geom_point(aes(x = TimeSinceFire, y = value, colour = variable))+
  geom_smooth(aes(x = TimeSinceFire, y = value, colour = variable), alpha = 0.2, method = "gam")+
  xlab("Time since fire")+
  ylab("Stand structure")+
  facet_wrap(~Planted)+
  ylim(c(-3,5))



ggplot(hb_cr_fl_tb_sa_all)+
  geom_point(aes(y = FisherHabitat, x = CQI))+
  geom_smooth(aes(y = FisherHabitat, x = CQI), 
              alpha = 0.2, method = "gam")+
  xlab("CQI")+
  ylab("goshawk")+
  facet_wrap(~Planted)

t.test(hb_cr_fl_tb_sa_all[Planted == "P"]$CQI,
       hb_cr_fl_tb_sa_all[Planted == "NP"]$CQI)

sa_nona <- sa[, 
              lapply(.SD, function(x) fifelse(is.na(x), 0, x)), 
              .SDcols = struct_vars 
][,`:=`(PlotID = sa$PlotID, Planted = sa$Planted)]
sim_res <- simper(sa_nona[,..struct_vars], 
                  group = sa_nona$Planted)
summary(sim_res)
sim_res <- simper(hb_cr_fl_tb_sa_all[,..struct_vars], 
                  group = hb_cr_fl_tb_sa_all$Planted)
summary(sim_res)





#models 


model <- lm(MartenHabitat ~ Planted + management_class + TimeSinceFire +
              snags_ha  + qmd_dbh   +  live_ba + can.open, 
            data = hb_cr_fl_tb_sa)
summary(model)

library(mgcv)
library(dplyr)
library(betareg)
# Define the list of variable names
variable_names <- c("GoshawkHabitat", "MartenHabitat","HareHabitat",
                    "TotalCarbon", "mn_90_ccp", "MerchVol")

# Function to fit GAM and extract summary information
#using betar - for data that ranges from 0 to 1
gam_fit_and_extract <- function(variable_name, data) {
  form <- paste(variable_name, "~ Planted + management_class + s(TimeSinceFire, bs = 'cr')")
  gm <- gam(formula = as.formula(form),
            data = data,
            family = betar(link = "logit"),
            method = "REML")
  
 
   summary_dt <- data.table(
    Variable = variable_name,
    Treatment = round(summary(gm)$p.table["PlantedP",1],3),
    Treatment_p_val = round(summary(gm)$p.table["PlantedP",4],3),
    Manage = round(summary(gm)$p.table["management_classPF",1],3),
    Manage_p_val = round(summary(gm)$p.table["PlantedP",4],3),
    TimeSinceFire = round(summary(gm)$s.table[,1],3),
    TimeSinceFire_p_val = round(summary(gm)$s.table[,4],3),
    R_Squared = round(summary(gm)$r.sq,2)
  )
  #suppressWarnings()
  return(summary_dt)
}

# Apply the function to each variable name
results <- lapply(variable_names, gam_fit_and_extract, data = hb_cr_fl_tb_sa)

# Combine results into a single data frame
gam_results <- bind_rows(results)



#linear model:
lm_fit_and_extract <- function(variable_name, data) {
  # Create the formula for the linear model
  form <- paste(variable_name, "~ Planted + management_class + TimeSinceFire +
                snags_ha  + qmd_dbh   +  live_ba + can.open")
  
  # Fit the linear model
  lm_model <- lm(as.formula(form), data = data)
  
  # Extract summary information
  summary_dt <- data.table(
    Variable = variable_name,
    Treatment = round(summary(lm_model)$coefficients["PlantedP", "Estimate"], 3),
    Treatment_p_val = round(summary(lm_model)$coefficients["PlantedP", "Pr(>|t|)"], 3),
    Manage_post = round(summary(lm_model)$coefficients["management_classPF", "Estimate"], 3),
    Manage_post_p_val = round(summary(lm_model)$coefficients["management_classPF", "Pr(>|t|)"], 3),
    Manage_pp = round(summary(lm_model)$coefficients["management_classPPF", "Estimate"], 3),
    Manage_pp_p_val = round(summary(lm_model)$coefficients["management_classPPF", "Pr(>|t|)"], 3),
    TimeSinceFire = round(summary(lm_model)$coefficients["TimeSinceFire", "Estimate"], 3),
    TimeSinceFire_p_val = round(summary(lm_model)$coefficients["TimeSinceFire", "Pr(>|t|)"], 3),
    snags = round(summary(lm_model)$coefficients["snags_ha", "Estimate"], 3),
    snags_p_val = round(summary(lm_model)$coefficients["snags_ha", "Pr(>|t|)"], 3),
    qmd_dbh = round(summary(lm_model)$coefficients["qmd_dbh", "Estimate"], 3),
    qmd_dbh_p_val = round(summary(lm_model)$coefficients["qmd_dbh", "Pr(>|t|)"], 3),
    live_ba = round(summary(lm_model)$coefficients["live_ba", "Estimate"], 3),
    live_ba_p_val = round(summary(lm_model)$coefficients["live_ba", "Pr(>|t|)"], 3),
    can.open = round(summary(lm_model)$coefficients["can.open", "Estimate"], 3),
    can.open_p_val = round(summary(lm_model)$coefficients["can.open", "Pr(>|t|)"], 3),
    R_Squared = round(summary(lm_model)$r.squared, 2)
  )
  
  return(summary_dt)
}

# Apply the function to each variable name
results <- lapply(variable_names, lm_fit_and_extract, data = hb_cr_fl_tb_sa)

# Combine results into a single data frame
lm_results <- bind_rows(results)






lm_fit_and_extract <- function(variable_name, data) {
  #form <- paste(variable_name, "~ TimeSinceFire + snags_ha + qmd_dbh + live_ba + can.open")
  #lm_model <- lm(as.formula(form), data = data)
  
  form <- paste(variable_name, "~ Planted + 
                s(snags_ha, bs = 'cr') + 
                s(qmd_dbh, bs = 'cr') + 
                s(live_ba, bs = 'cr') + 
                s(can.open, bs = 'cr') + 
                s(TimeSinceFire, bs = 'cr')")
  gm <- gam(formula = as.formula(form),
            data = data,
            family = betar(link = "logit"),
            method = "REML")
  
  
  # Extract summary information
  summary_dt <- data.table(
    Variable = variable_name,
    TimeSinceFire = round(summary(lm_model)$coefficients["TimeSinceFire", "Estimate"], 3),
    TimeSinceFire_p_val = round(summary(lm_model)$coefficients["TimeSinceFire", "Pr(>|t|)"], 3),
    snags = round(summary(lm_model)$coefficients["snags_ha", "Estimate"], 3),
    snags_p_val = round(summary(lm_model)$coefficients["snags_ha", "Pr(>|t|)"], 3),
    qmd_dbh = round(summary(lm_model)$coefficients["qmd_dbh", "Estimate"], 3),
    qmd_dbh_p_val = round(summary(lm_model)$coefficients["qmd_dbh", "Pr(>|t|)"], 3),
    live_ba = round(summary(lm_model)$coefficients["live_ba", "Estimate"], 3),
    live_ba_p_val = round(summary(lm_model)$coefficients["live_ba", "Pr(>|t|)"], 3),
    can.open = round(summary(lm_model)$coefficients["can.open", "Estimate"], 3),
    can.open_p_val = round(summary(lm_model)$coefficients["can.open", "Pr(>|t|)"], 3),
    R_Squared = round(summary(lm_model)$r.squared, 2)
  )
  
  return(summary_dt)
}

# Apply the function to each variable name
results <- lapply(variable_names, lm_fit_and_extract, data = hb_cr_fl_tb_sa)

# Combine results into a single data frame
lm_results <- bind_rows(results)





variable_names <- c("GoshawkHabitat", "FisherHabitat","HareHabitat",
                    "TotalCarbon", "mn_90_ccp", "MerchVol")

# Add a small decimal to values of exactly 0 and subtract from values of exactly 1
hb_cr_fl_tb[, paste0((variable_names),"_s") :=
                  lapply(.SD, function(x) {
                    ifelse(x == 0, x + 0.00001,  
                           ifelse(x == 1, x - 0.00001, x))  
                  }), .SDcols = variable_names]

hb_cr_fl_tb[, paste0((variable_names),"_s") := lapply(.SD, scale_fn), 
            .SDcols = variable_names]


variable_names <- c("GoshawkHabitat_s", "FisherHabitat_s","HareHabitat_s",
                    "TotalCarbon_s", "mn_90_ccp_s", "MerchVol_s")

#using betar - for data that ranges from 0 to 1
fit_and_extract <- function(variable_name, data) {
  form <- paste(variable_name, "~ Planted + TimeSinceFire")
  gl <- betareg::betareg(formula = as.formula(form),
                        data = data)
  
  
  # Create a data.table with extracted information
  summary_dt <- data.table(
    Variable = variable_name,
    Treatment = round(summary(gl)$coefficients$mean["PlantedP",1],3),
    Treatment_p_val = round(summary(gl)$coefficients$mean["PlantedP",4],3),
    TimeSinceFire = round(summary(gl)$coefficients$mean["TimeSinceFire",1],3),
    TimeSinceFire_p_val = round(summary(gl)$coefficients$mean["TimeSinceFire",1],3),
    R_Squared = summary(gl)$r.sq
  )
  #suppressWarnings()
  return(summary_dt)
}

# Apply the function to each variable name
results <- lapply(variable_names, fit_and_extract, data = hb_cr_fl_tb)

# Combine results into a single data frame
linear_results <- bind_rows(results)





set.seed(123)  # For reproducibility
nmds_result <- vegan::metaMDS(hb_cr_fl_tb[,..ind_names],
                              k = 2, distance = "bray")

nmds_data <- as.data.table(vegan::scores(nmds_result, display = "sites"))
species_scores <- vegan::scores(nmds_result, display = "species")
species_df <- as.data.frame(species_scores)
species_df$Species <- rownames(species_df)

# Combine NMDS scores with the original data
data_with_nmds <- cbind(hb_cr_fl_tb, nmds_data)

# Plot the NMDS ordination with color and shape by groups
ggplot() +
  geom_point(data = data_with_nmds,
             aes(x = NMDS1, y = NMDS2, size = TimeSinceFire, 
                 colour = Planted, shape = factor(under_plant)))+
  #geom_point(data = species_df, aes(color = Species,x = NMDS1, y = NMDS2))+
  geom_text(data = species_df, aes(x = NMDS1, y = NMDS2,
                                   label = Species, hjust = 1.2), size = 3)


form <- GrouseHabitat ~ Planted + TimeSinceFire
gl <- betareg::betareg(formula = as.formula(form),
                data = hb_cr_fl_tb)




gamMod <- mgcv::gam(dNBRReSamp~s(PlantAge, k=25), data = dat_csv_dt[FireID=="Chutanli"], 
                    method = "REML", family = "scat")
summary(gamMod) 
gam.check(gamMod)
gamMod <- mgcv::gam(dNBRReSamp~s(PlantAge, k=25), data = dat_csv_dt[FireID=="Island"], 
                    method = "REML", family = "scat")
summary(gamMod)
gam.check(gamMod)
gamMod <- mgcv::gam(dNBRReSamp~s(PlantAge, k=25), data = dat_csv_dt[FireID=="Nadina"], 
                    method = "REML", family = "scat")
gamMod <- mgcv::gam(dNBRCAT~s(PlantAge), data = dat_csv_dt[FireID=="Nadina"], 
                    method = "REML", family = ocat(R=4))
predict(gamMod,dat_csv_dt[FireID=="Nadina",.(dNBRCAT,PlantAge)],type="response",se=TRUE)
summary(gamMod)
gam.check(gamMod)

gamMod <- mgcv::gam(dNBRReSamp~s(PlantAge), data = dat_csv_dt[FireID=="Shovel"], method = "REML")
summary(gamMod)
gamMod <- mgcv::gam(dNBRReSamp~s(PlantAge), data = dat_csv_dt[FireID=="Tezzeron"], method = "REML")
summary(gamMod)
gamMod <- mgcv::gam(dNBRReSamp~s(PlantAge), data = dat_csv_dt[FireID=="Verdun"], method = "REML")
summary(gamMod)









hab_ind <- melt(HabitatIndicies, id.vars = c("PlotID","Planted","TimeSinceFire"),
                measure.vars = c("MartenHabitat", "FisherHabitat", "GoshawkHabitat", 
                                 "HareHabitat", "SquirrelHabitat",
                                 "SmMammalHabitat", "GrouseHabitat"))
setnames(hab_ind, c("variable","value"), c("species","habitat_index"))

#Scale the indices between 0 and 1
scale_fn <- function(var){(var - min(var)) / (max(var) - min(var))}

hab_ind[,hab_ind_sc := scale_fn(habitat_index),by = .(species)]
hab_ind_d <- dcast(hab_ind, PlotID + Planted + TimeSinceFire ~ species,
                   value.var = "hab_ind_sc")
hab_ind_d <- hab_ind_d[,.(PlotID, MartenHabitat, FisherHabitat, GoshawkHabitat, HareHabitat,
                          SquirrelHabitat,SmMammalHabitat, GrouseHabitat)]
write.csv(hab_ind_d, "./02-prepped_values/hab_indi.csv", row.names = F)


custom_color_scale <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b15928")


ggplot(data= hab_ind)+
  geom_point(aes(x = TimeSinceFire, y = hab_ind_sc, colour = species))+
  geom_smooth(aes(x = TimeSinceFire, y = hab_ind_sc, colour = species), alpha = 0,
              method = "gam")+
  labs(color = "Wildlife species")+
  scale_color_manual(labels = c("Marten", "Fisher", "Goshawk", "Hare", "Squirrel",
                                "Small mammal", "Grouse"), 
                     values = custom_color_scale)+
  xlab("Time since fire")+
  ylab("Habitat index")+
  facet_wrap(~Planted)


ggplot(data= hab_ind)+
  geom_boxplot(aes(x = Planted, y = hab_ind_sc, fill = species))






hab_ind <- hab_ind[,.(PlotID, Planted, TimeSinceFire,MartenHabitat, FisherHabitat, GoshawkHabitat,
                      HareHabitat, SquirrelHabitat, SmMammalHabitat)]

hab_vars <- names(hab_ind)[!names(hab_ind) %in% c("PlotID","Planted","TimeSinceFire")]

scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

hab_ind[, (hab_vars) := lapply(.SD, scale_values), .SDcols = hab_vars]

#x = habitat score
part_utils <- function(x){sum(mean(x-np)/ max(x-p))/max(x)}

#fisher:
fisher_np <- (hab_ind[Planted == "NP",
                      mean(FisherHabitat)]/hab_ind[Planted == "P",
                              max(FisherHabitat)])/hab_ind[,max(FisherHabitat)]
fisher_p <- (hab_ind[Planted == "P",
                      mean(FisherHabitat)]/hab_ind[Planted == "NP", 
                            max(FisherHabitat)])/hab_ind[,max(FisherHabitat)]

#marten:
marten_np <- (hab_ind[Planted == "NP",
                      mean(MartenHabitat)]/hab_ind[Planted == "P",
                              max(MartenHabitat)])/hab_ind[,max(MartenHabitat)]
merten_p <- (hab_ind[Planted == "P",
                     mean(MartenHabitat)]/hab_ind[Planted == "NP",
                            max(MartenHabitat)])/hab_ind[,max(MartenHabitat)]

#marten:
marten_np <- (hab_ind[Planted == "NP",
                      mean(MartenHabitat)]/hab_ind[Planted == "P",
                             max(MartenHabitat)])/hab_ind[,max(MartenHabitat)]
merten_p <- (hab_ind[Planted == "P",
                     mean(MartenHabitat)]/hab_ind[Planted == "NP",
                             max(MartenHabitat)])/hab_ind[,max(MartenHabitat)]

#2. Total utility
#weights equal, and then weights unbalanced

# for each site (71) sum(weightC * CpartUtil + weightT * TpartUtil + weightB * BpartUtil)/ number of sites

#we don't need part 3 because we didn't ahve different site types

# maximum utility under each weighting scenario.





#another framework is multifunctionality, but this seems to be an optimization approach - 
# define the ideal and anti-deal for an idicator and than optimize management decision




