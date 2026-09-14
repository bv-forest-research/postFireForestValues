# Pareto-frontier workflow for post-fire forest values
# This is a reduced copy of the full MCDM workflow, keeping only the
# data preparation and replacing the partial-utility weighting step with
# a Pareto-frontier calculation.

library(data.table)
library(ggplot2)
library(scico)
library(mgcv)

script_arg <- commandArgs(trailingOnly = FALSE)
script_file <- script_arg[grep("^--file=", script_arg)]
script_file <- if (length(script_file) > 0) sub("^--file=", "", script_file[1]) else ""

if (nzchar(script_file)) {
  project_dir <- normalizePath(file.path(dirname(script_file), ".."), winslash = "/", mustWork = FALSE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

in_dir <- file.path(project_dir, "02_prepped_values")
out_dir <- file.path(project_dir, "03_outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

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

is_dominated <- function(i, j, dt, crit_cols) {
  i_vals <- as.numeric(dt[i, ..crit_cols])
  j_vals <- as.numeric(dt[j, ..crit_cols])

  all(j_vals >= i_vals, na.rm = TRUE) && any(j_vals > i_vals, na.rm = TRUE)
}

# -----------------------------
# 1. Import and clean treatments
# -----------------------------
FR_treatments <- fread(file.path(project_dir, "01_data_inputs", "FR_Treatments.csv"))
FR_treatments[, `:=`(PlotID = as.factor(ID), Planted = as.factor(Planted))]
FR_treatments[, TimeSinceFire := 2020 - FIRE_YEAR]

plot_treatments <- FR_treatments[, .(PlotID, Planted, TimeSinceFire)]
plot_treatments[, TSF := ifelse(
  TimeSinceFire <= 10, "<10",
  ifelse(TimeSinceFire <= 20, "10-20",
         ifelse(TimeSinceFire <= 40, "20-40", "40-60+"))
)]
plot_treatments[, Planted := factor(Planted, levels = c("P", "NP"))]

pre_post_treat <- fread(file.path(project_dir, "01_data_inputs", "jb_treatments.csv"))
plot_treatments[, management_class := pre_post_treat$management_class]

# -----------------------------
# 2. Import indicator data
# -----------------------------
hb <- fread(list.files(in_dir, pattern = "hab", full.names = TRUE)[1])
setnafill(hb, fill = 0, cols = colnames(hb)[grep("Habitat", colnames(hb))])
cr <- fread(list.files(in_dir, pattern = "carbon", full.names = TRUE)[1])
fl <- fread(list.files(in_dir, pattern = "fire_values", full.names = TRUE)[1])
tb <- fread(list.files(in_dir, pattern = "vol", full.names = TRUE)[1])
la <- fread(list.files(in_dir, pattern = "layers", full.names = TRUE)[1])
sa <- fread(file.path(in_dir, "plot_structure.csv"))

sa_min <- sa[
  , lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
  .SDcols = c("snags_ha", "qmd_dbh", "live_ba", "can.open", "top_height",
              "stems_ha", "Total_Herbs", "ShrubsB2", "ShrubsB1")
]
sa_min[, PlotID := sa$PlotID]

plot_attributes <- Reduce(
  function(x, y) merge(x, y, by = "PlotID", all = TRUE),
  list(hb, cr, fl, tb, la, sa_min)
)

plot_key <- plot_attributes$PlotID
numeric_cols <- names(which(vapply(plot_attributes, is.numeric, logical(1))))

plot_attributes <- plot_attributes[
  , lapply(.SD, function(x) fifelse(is.na(x), 0, x)),
  .SDcols = numeric_cols
]
plot_attributes[, PlotID := plot_key]

plot_metadata <- plot_treatments[, .(
  PlotID, Planted, TSF, TimeSinceFire, management_class
)]

pareto_dt <- merge(
  plot_attributes,
  plot_metadata,
  by = "PlotID",
  all.x = TRUE
)
pareto_dt[,`:=`(TimeSinceFire.x = NULL, TimeSinceFire.y = NULL)]
pareto_dt <- pareto_dt[order(PlotID)]
pareto_dt <- pareto_dt[management_class %in% c("NM", "PFO")]

habitat_species_cols <- c(
  "MartenHabitat", "FisherHabitat", "GoshawkHabitat",
  "HareHabitat", "SquirrelHabitat", "SmMammalHabitat",
  "GrouseHabitat", "GrizzlyHabitat"
)

for (sp in habitat_species_cols) {
  scaled_col <- paste0(sp, "_scaled")
  util_col <- paste0(sp, "_Util")
  pareto_dt[[scaled_col]] <- scale01(pareto_dt[[sp]])
  pareto_dt[[util_col]] <- pareto_dt[[scaled_col]] / max(pareto_dt[[scaled_col]], na.rm = TRUE)
}

pareto_dt[, CarbonUtil := scale01(TotalCarbon)]
pareto_dt[, TimberUtil := scale01(MerchVol)]

weather_levels <- c("50", "75", "90", "95")
fire_metrics <- c("ccp", "hfi", "sfi", "mort", "CO2", "preload")

for (weather in weather_levels) {
  fire_cols <- paste0("mn_", weather, "_", fire_metrics)
  fire_cols <- intersect(fire_cols, names(pareto_dt))

  fire_risk_mat <- vapply(
    fire_cols,
    function(col) scale01(pareto_dt[[col]]),
    numeric(nrow(pareto_dt))
  )
  fire_risk_score <- rowMeans(fire_risk_mat, na.rm = TRUE)

  # Fire metrics are coded such that larger values indicate greater fire hazard;
  # invert them so the frontier is built on higher-is-better fire utility.
  pareto_dt[, paste0("FireRisk_", weather) := fire_risk_score]
  pareto_dt[, paste0("FireUtil_", weather) := 1 - fire_risk_score]
}

pareto_dt[, MeanSpeciesUtil := rowMeans(.SD, na.rm = TRUE), .SDcols = paste0(habitat_species_cols, "_Util")]

pareto_time_summary <- pareto_dt[
  , .(
    CarbonUtil = mean(CarbonUtil, na.rm = TRUE),
    TimberUtil = mean(TimberUtil, na.rm = TRUE),
    MeanSpeciesUtil = mean(MeanSpeciesUtil, na.rm = TRUE),
    TimeSinceFire = mean(TimeSinceFire, na.rm = TRUE),
    FireUtil_50 = mean(FireUtil_50, na.rm = TRUE),
    FireUtil_75 = mean(FireUtil_75, na.rm = TRUE),
    FireUtil_90 = mean(FireUtil_90, na.rm = TRUE),
    FireUtil_95 = mean(FireUtil_95, na.rm = TRUE)
  ),
  by = .(Planted, TSF)
]

pareto_time_long <- melt(
  pareto_time_summary,
  id.vars = c("Planted", "TSF", "TimeSinceFire"),
  measure.vars = c("CarbonUtil", "TimberUtil", "MeanSpeciesUtil", paste0("FireUtil_", weather_levels)),
  variable.name = "Criterion",
  value.name = "UtilityValue"
)
pareto_time_long[, Criterion := factor(
  Criterion,
  levels = c("CarbonUtil", "TimberUtil", "MeanSpeciesUtil", paste0("FireUtil_", weather_levels)),
  labels = c("Carbon", "Timber", "Mean species", paste0(weather_levels, "th percentile"))
)]

# -----------------------------
# 4. Pareto-frontier calculation
# -----------------------------
criterion_cols <- c("CarbonUtil", "TimberUtil", "MeanSpeciesUtil", "FireUtil_90")

pareto_dt[, ParetoFrontier := FALSE]
pareto_dt[, ParetoRank := NA_integer_]

for (tsf_group in unique(pareto_dt$TSF)) {
  group_idx <- which(pareto_dt$TSF == tsf_group)
  remaining <- group_idx
  current_rank <- 1L

  while (length(remaining) > 0) {
    frontier_idx <- integer(0)

    for (i in remaining) {
      dominated <- FALSE

      for (j in remaining) {
        if (i == j) next
        if (is_dominated(i, j, pareto_dt, criterion_cols)) {
          dominated <- TRUE
          break
        }
      }

      if (!dominated) {
        frontier_idx <- c(frontier_idx, i)
      }
    }

    if (length(frontier_idx) == 0) {
      break
    }

    pareto_dt[frontier_idx, ParetoRank := current_rank]
    pareto_dt[frontier_idx, ParetoFrontier := TRUE]
    remaining <- setdiff(remaining, frontier_idx)
    current_rank <- current_rank + 1L
  }
}

pareto_time_frontier <- rbindlist(lapply(unique(pareto_time_summary$TSF), function(tsf_group) {
  time_pts <- copy(pareto_time_summary[TSF == tsf_group])
  time_pts[, ParetoFrontier := FALSE]
  time_pts[, ParetoRank := NA_integer_]

  remaining <- seq_len(nrow(time_pts))
  current_rank <- 1L

  while (length(remaining) > 0) {
    frontier_idx <- integer(0)

    for (i in remaining) {
      dominated <- FALSE

      for (j in remaining) {
        if (i == j) next
        if (is_dominated(i, j, time_pts, criterion_cols)) {
          dominated <- TRUE
          break
        }
      }

      if (!dominated) {
        frontier_idx <- c(frontier_idx, i)
      }
    }

    if (length(frontier_idx) == 0) {
      break
    }

    time_pts[frontier_idx, `:=`(ParetoFrontier = TRUE, ParetoRank = current_rank)]
    remaining <- setdiff(remaining, frontier_idx)
    current_rank <- current_rank + 1L
  }

  time_pts[, TSF := tsf_group]
  time_pts
}))

fwrite(pareto_time_frontier, file.path(out_dir, "pareto_time_frontier_by_tsf.csv"))

traj_grid <- data.table(expand.grid(
  TimeSinceFire = seq(min(pareto_dt$TimeSinceFire, na.rm = TRUE), max(pareto_dt$TimeSinceFire, na.rm = TRUE), length.out = 60),
  Planted = c("P", "NP")
))

traj_predictions <- rbindlist(lapply(criterion_cols, function(criterion_name) {
  rbindlist(lapply(c("P", "NP"), function(trt) {
    fit_dt <- pareto_dt[Planted == trt]
    fit <- mgcv::gam(
      as.formula(paste0(criterion_name, " ~ s(TimeSinceFire, k = 4, bs = 'cr')")),
      data = fit_dt,
      method = "REML"
    )

    pred_dt <- traj_grid[Planted == trt]
    pred_dt[, UtilityValue := as.numeric(predict(fit, newdata = pred_dt, type = "response"))]
    pred_dt[, Criterion := criterion_name]
    pred_dt
  }))
}))

traj_predictions[, Criterion := factor(
  Criterion,
  levels = criterion_cols,
  labels = c("Carbon", "Timber", "Mean species", "Fire")
)]

# Pooled GAM interaction models for each indicator utility column.
# These are fit directly on the underlying utility indicators rather than only
# on the Pareto-frontier summary criteria, so the treatment-time effect is
# evaluated on the same data that drive the frontier.
gam_response_cols <- c(
  "CarbonUtil",
  "TimberUtil",
  paste0(habitat_species_cols, "_Util"),
  paste0("FireUtil_", weather_levels)
)

make_gam_label <- function(criterion_name) {
  if (criterion_name == "CarbonUtil") {
    "Carbon"
  } else if (criterion_name == "TimberUtil") {
    "Timber"
  } else if (startsWith(criterion_name, "FireUtil_")) {
    paste0("Fire ", sub("^FireUtil_", "", criterion_name))
  } else {
    "Habitat"
  }
}

gam_fit_list <- lapply(gam_response_cols, function(criterion_name) {
  fit_dt <- pareto_dt[, .(
    TimeSinceFire,
    Planted = factor(Planted, levels = c("P", "NP")),
    UtilityValue = get(criterion_name)
  )]

  fit <- mgcv::gam(
    UtilityValue ~ Planted + s(TimeSinceFire) + s(TimeSinceFire, by = Planted),
    data = fit_dt,
    method = "REML"
  )

  fit
})
names(gam_fit_list) <- gam_response_cols

# Difference curves with confidence bands for planted minus not-planted
# predictions. These summarize where the treatment trajectories diverge.
diff_grid <- data.table(
  TimeSinceFire = seq(
    min(pareto_dt$TimeSinceFire, na.rm = TRUE),
    max(pareto_dt$TimeSinceFire, na.rm = TRUE),
    length.out = 60
  )
)

pred_grid <- data.table(expand.grid(
  TimeSinceFire = diff_grid$TimeSinceFire,
  Planted = factor(c("P", "NP"), levels = c("P", "NP"))
))

pred_curve_dt <- rbindlist(lapply(gam_response_cols, function(criterion_name) {
  fit <- gam_fit_list[[criterion_name]]

  pred <- predict(
    fit,
    newdata = pred_grid,
    se.fit = TRUE,
    type = "response"
  )

  pred_dt <- data.table(
    Criterion = criterion_name,
    CriterionLabel = make_gam_label(criterion_name),
    TimeSinceFire = pred_grid$TimeSinceFire,
    Planted = pred_grid$Planted,
    UtilityValue = as.numeric(pred$fit),
    UtilitySE = as.numeric(pred$se.fit)
  )

  pred_dt[, `:=`(
    TreatmentLabel = ifelse(Planted == "P", "Planted", "Not planted")
  )]

  dcast(pred_dt, Criterion + CriterionLabel + TimeSinceFire ~ Planted, value.var = c("UtilityValue", "UtilitySE"))
}))

pred_curve_dt[, Diff := UtilityValue_P - UtilityValue_NP]
pred_curve_dt[, SE_diff := sqrt(UtilitySE_P^2 + UtilitySE_NP^2)]
pred_curve_dt[, `:=`(
  CI_low = Diff - 1.96 * SE_diff,
  CI_high = Diff + 1.96 * SE_diff
)]

fwrite(pred_curve_dt, file.path(out_dir, "pareto_gam_difference_curve.csv"))

# Model term summaries to support inference on the GAM interaction.
gam_p_table <- rbindlist(lapply(gam_response_cols, function(criterion_name) {
  fit <- gam_fit_list[[criterion_name]]
  sm <- summary(fit)

  p_tbl <- as.data.table(sm$p.table, keep.rownames = "Parameter")
  p_tbl[, `:=`(
    Criterion = criterion_name,
    CriterionLabel = make_gam_label(criterion_name)
  )]
  p_tbl
}))

gam_s_table <- rbindlist(lapply(gam_response_cols, function(criterion_name) {
  fit <- gam_fit_list[[criterion_name]]
  sm <- summary(fit)

  s_tbl <- as.data.table(sm$s.table, keep.rownames = "SmoothTerm")
  s_tbl[, `:=`(
    Criterion = criterion_name,
    CriterionLabel = make_gam_label(criterion_name)
  )]
  s_tbl
}))

fwrite(gam_p_table, file.path(out_dir, "pareto_gam_parametric_summary.csv"))
fwrite(gam_s_table, file.path(out_dir, "pareto_gam_smooth_summary.csv"))

# Frontier membership summary by TSF and planted status with bootstrap CIs.
frontier_membership <- pareto_dt[
  , .(
    nPlots = .N,
    FrontierCount = sum(ParetoFrontier, na.rm = TRUE),
    FrontierProportion = mean(ParetoFrontier, na.rm = TRUE)
  ),
  by = .(TSF, Planted)
]
frontier_membership[, FrontierProportion := round(FrontierProportion, 4)]

set.seed(123)
boot_n <- 400L
bootstrap_frontier <- rbindlist(lapply(unique(pareto_dt$TSF), function(tsf_group) {
  group_dt <- pareto_dt[TSF == tsf_group]
  group_counts <- group_dt[, .N, by = .(Planted)]
  p_n <- group_counts[Planted == "P", N]
  np_n <- group_counts[Planted == "NP", N]

  if (p_n == 0 || np_n == 0) {
    return(NULL)
  }

  boot_draws <- rbindlist(lapply(seq_len(boot_n), function(b) {
    p_sample <- group_dt[Planted == "P"][sample.int(p_n, p_n, replace = TRUE)]
    np_sample <- group_dt[Planted == "NP"][sample.int(np_n, np_n, replace = TRUE)]

    data.table(
      TSF = tsf_group,
      Bootstrap = b,
      FrontierPropDiff = mean(p_sample$ParetoFrontier, na.rm = TRUE) - mean(np_sample$ParetoFrontier, na.rm = TRUE)
    )
  }))

  boot_draws[, `:=`(
    CI_low = quantile(FrontierPropDiff, 0.025, na.rm = TRUE),
    CI_high = quantile(FrontierPropDiff, 0.975, na.rm = TRUE),
    Estimate = mean(FrontierPropDiff, na.rm = TRUE)
  )]

  boot_draws[, .(TSF, Estimate = unique(Estimate), CI_low = unique(CI_low), CI_high = unique(CI_high))][1]
}))

frontier_membership <- merge(
  frontier_membership,
  bootstrap_frontier,
  by = "TSF",
  all.x = TRUE
)

fwrite(frontier_membership, file.path(out_dir, "pareto_frontier_membership_summary.csv"))

# -----------------------------
# 5. Save the reduced analysis outputs
# -----------------------------
pareto_summary <- pareto_dt[
  , .(
    nPlots = .N,
    MeanCarbon = mean(CarbonUtil, na.rm = TRUE),
    MeanTimber = mean(TimberUtil, na.rm = TRUE),
    MeanSpecies = mean(MeanSpeciesUtil, na.rm = TRUE),
    MeanFire = mean(FireUtil_90, na.rm = TRUE)
  ),
  by = .(Planted, TSF, management_class, ParetoFrontier)
]

fwrite(pareto_summary, file.path(out_dir, "pareto_frontier_summary.csv"))
fwrite(pareto_dt[, .(PlotID, Planted, TSF, management_class, CarbonUtil, TimberUtil, MeanSpeciesUtil, FireUtil_90, ParetoFrontier, ParetoRank)], file.path(out_dir, "pareto_frontier_plot_scores.csv"))

# -----------------------------
# 6. Figures
# -----------------------------
traj_predictions_weather_species <- rbindlist(lapply(weather_levels, function(weather) {
  fire_col <- paste0("FireUtil_", weather)

  rbindlist(lapply(habitat_species_cols, function(species_name) {
    habitat_col <- paste0(species_name, "_Util")
    habitat_label <- sub("Habitat$", "", species_name)

    criterion_cols_local <- c("CarbonUtil", "TimberUtil", habitat_col, fire_col)

    rbindlist(lapply(criterion_cols_local, function(criterion_name) {
      rbindlist(lapply(c("P", "NP"), function(trt) {
        fit_dt <- pareto_dt[Planted == trt]
        fit <- mgcv::gam(
          as.formula(paste0(criterion_name, " ~ s(TimeSinceFire, k = 4, bs = 'cr')")),
          data = fit_dt,
          method = "REML"
        )

        pred_dt <- traj_grid[Planted == trt]
        pred_dt[, UtilityValue := as.numeric(predict(fit, newdata = pred_dt, type = "response"))]
        pred_dt[, Criterion := criterion_name]
        pred_dt[, WeatherScenario := weather]
        pred_dt[, HabitatSpecies := habitat_label]

        if (criterion_name == "CarbonUtil") {
          pred_dt[, CriterionLabel := "Carbon"]
        } else if (criterion_name == "TimberUtil") {
          pred_dt[, CriterionLabel := "Timber"]
        } else if (criterion_name == fire_col) {
          pred_dt[, CriterionLabel := "Fire"]
        } else {
          pred_dt[, CriterionLabel := "Habitat"]
        }

        pred_dt
      }))
    }))
  }))
}))

p_weather_species_traj <- ggplot(
  traj_predictions_weather_species,
  aes(x = TimeSinceFire, y = UtilityValue, colour = CriterionLabel, linetype = Planted)
) +
  geom_line(linewidth = 1.0) +
  #facet_grid(WeatherScenario ~ HabitatSpecies, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Time since fire",
    y = "Predicted utility",
    colour = "Criterion",
    linetype = "Treatment",
    title = "GAM trajectories by fire-weather scenario and habitat species"
  ) +
  facet_grid(
    WeatherScenario ~ HabitatSpecies,
    labeller = labeller(
      WeatherScenario = function(x) paste0("Fire-weather: ", x),
      HabitatSpecies = function(x) paste0("Species: ", x)
    ),
    scales = "free_y"
  )

traj_plot_path <- file.path(out_dir, "pareto_trajectory_weather_species.jpg")
ggsave(filename = traj_plot_path, plot = p_weather_species_traj, device = "jpeg", dpi = 300, bg = "white")
message("Saved: ", traj_plot_path)


p_frontier_scatter <- ggplot(
  pareto_dt,
  aes(x = CarbonUtil, y = FireUtil_90, colour = ParetoFrontier)
) +
  geom_point(size = 3, alpha = 0.9, aes(group = TSF, colour = TSF, shape = Planted)) +
  #geom_smooth(method = "lm", aes(group = Planted, linetype = Planted)) +
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
  #scale_colour_manual(values = c("FALSE" = "grey65", "TRUE" = "#b2182b")) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Carbon utility",
    y = "Fire utility",
    #colour = "Pareto-frontier point",
    #shape = "Treatment",
    title = "Carbon vs fire Pareto frontier"
  )

ggsave(filename = "pareto_frontier_carbon_fire_all.jpg", path = out_dir, device = "jpeg", dpi = 300, bg = "white")

p_frontier_by_group <- ggplot(
  pareto_dt,
  aes(x = CarbonUtil, y = TimberUtil, colour = ParetoFrontier, shape = management_class)
) +
  geom_point(size = 3, alpha = 0.9) +
  geom_smooth(method = "lm", aes(group = management_class, linetype = management_class)) +
  facet_wrap(~TSF) +
  scale_colour_manual(values = c("FALSE" = "grey65", "TRUE" = "#2166ac")) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Carbon utility",
    y = "Timber utility",
    colour = "Pareto-frontier point",
    shape = "Management class",
    title = "Carbon vs timber Pareto frontier by management class and TSF bin"
  )

ggsave(filename = "pareto_frontier_carbon_timber.jpg", path = out_dir, device = "jpeg", dpi = 300, bg = "white")

pareto_box_long <- melt(
  pareto_dt,
  id.vars = c("PlotID", "Planted", "TSF", "management_class", "ParetoRank"),
  measure.vars = c("CarbonUtil", "TimberUtil", "MeanSpeciesUtil", "FireUtil_90"),
  variable.name = "UtilityCriterion",
  value.name = "UtilityValue"
)
pareto_box_long[, UtilityCriterion := factor(
  UtilityCriterion,
  levels = c("CarbonUtil", "TimberUtil", "MeanSpeciesUtil", "FireUtil_90"),
  labels = c("Carbon", "Timber", "Mean species", "Fire")
)]

p_frontier_box <- ggplot(
  pareto_box_long,
  aes(x = TSF, y = UtilityValue, fill = Planted)
) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.35) +
  facet_wrap(~UtilityCriterion, ncol = 2) +
  scale_fill_manual(values = c("P" = "#b2182b", "NP" = "#2166ac")) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Time since fire bin",
    y = "Utility value",
    fill = "Treatment",
    title = "Utility distributions by TSF and planted status"
  ) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(filename = "pareto_frontier_boxplot_tsf_planted.jpg", path = out_dir, device = "jpeg", dpi = 300, bg = "white")

traj_plot <- ggplot(
  traj_predictions,
  aes(x = TimeSinceFire, y = UtilityValue, colour = Planted, linetype = Planted)
) +
  geom_line(linewidth = 1.0) +
  facet_wrap(~Criterion, ncol = 2) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Time since fire",
    y = "Predicted utility",
    colour = "Treatment",
    linetype = "Treatment",
    title = "GAM trajectories for each value through time"
  )

ggsave(filename = "pareto_trajectory_gam.jpg", path = out_dir, device = "jpeg", dpi = 300, bg = "white")

traj_frontier_plot <- ggplot(
  pareto_time_frontier,
  aes(x = TimeSinceFire, y = ParetoRank, colour = Planted)
) +
  geom_point(size = 3, alpha = 0.9) +
  facet_wrap(~TSF) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Time since fire bin",
    y = "Pareto rank",
    colour = "Treatment",
    title = "Pareto frontier rank by treatment within each TSF bin"
  )

ggsave(filename = "pareto_frontier_time_rank.jpg", path = out_dir, device = "jpeg", dpi = 300, bg = "white")

message("Pareto-frontier workflow complete. Outputs saved to 03_outputs/")
