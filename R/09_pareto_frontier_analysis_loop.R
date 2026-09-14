# Pareto-frontier workflow for post-fire forest values
# This is a reduced copy of the full MCDM workflow, keeping only the
# data preparation and replacing the partial-utility weighting step with
# a Pareto-frontier calculation.

library(data.table)
library(ggplot2)
library(scico)
library(mgcv)

in_dir <- "02_prepped_values"
out_dir <- "03_outputs"

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

compute_pareto_frontier <- function(dt, criterion_cols) {
  dt <- copy(dt)
  dt[, ParetoFrontier := FALSE]
  dt[, ParetoRank := NA_integer_]

  for (tsf_group in unique(dt$TSF)) {
    group_idx <- which(dt$TSF == tsf_group)
    remaining <- group_idx
    current_rank <- 1L

    while (length(remaining) > 0) {
      frontier_idx <- integer(0)

      for (i in remaining) {
        dominated <- FALSE

        for (j in remaining) {
          if (i == j) next
          if (is_dominated(i, j, dt, criterion_cols)) {
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

      dt[frontier_idx, `:=`(ParetoRank = current_rank, ParetoFrontier = TRUE)]
      remaining <- setdiff(remaining, frontier_idx)
      current_rank <- current_rank + 1L
    }
  }

  dt
}

compute_time_frontier <- function(time_summary, criterion_cols) {
  time_pts <- copy(time_summary)
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

  time_pts
}

# -----------------------------
# 1. Import and clean treatments
# -----------------------------
FR_treatments <- fread(file.path("01_data_inputs", "FR_Treatments.csv"))
FR_treatments[, `:=`(PlotID = as.factor(ID), Planted = as.factor(Planted))]
FR_treatments[, TimeSinceFire := 2020 - FIRE_YEAR]

plot_treatments <- FR_treatments[, .(PlotID, Planted, TimeSinceFire)]
plot_treatments[, TSF := ifelse(
  TimeSinceFire <= 10, "<10",
  ifelse(TimeSinceFire <= 20, "10-20",
         ifelse(TimeSinceFire <= 40, "20-40", "40-60+"))
)]
plot_treatments[, Planted := factor(Planted, levels = c("P", "NP"))]

pre_post_treat <- fread(file.path(in_dir, "jb_treatments.csv"))
plot_treatments[, management_class := pre_post_treat$management_class]

# -----------------------------
# 2. Import indicator data
# -----------------------------
hb <- fread(list.files(in_dir, "hab", full.names = TRUE))
setnafill(hb, fill = 0, cols = colnames(hb)[grep("Habitat", colnames(hb))])
cr <- fread(list.files(in_dir, "carbon", full.names = TRUE))
fl <- fread(list.files(in_dir, "fire_values", full.names = TRUE))
tb <- fread(list.files(in_dir, "vol", full.names = TRUE))
la <- fread(list.files(in_dir, "layers", full.names = TRUE))
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
pareto_dt[, `:=`(TimeSinceFire.x = NULL, TimeSinceFire.y = NULL)]
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

traj_grid <- data.table(expand.grid(
  TimeSinceFire = seq(
    min(pareto_dt$TimeSinceFire, na.rm = TRUE),
    max(pareto_dt$TimeSinceFire, na.rm = TRUE),
    length.out = 60
  ),
  Planted = c("P", "NP")
))

summary_list <- list()
score_list <- list()
trajectory_list <- list()
diff_curve_list <- list()
gam_param_list <- list()
gam_smooth_list <- list()
frontier_membership_list <- list()
time_frontier_list <- list()

for (weather in weather_levels) {
  fire_col <- paste0("FireUtil_", weather)

  for (habitat_col in paste0(habitat_species_cols, "_Util")) {
    habitat_label <- sub("_Util$", "", habitat_col)
    criterion_cols <- c("CarbonUtil", "TimberUtil", habitat_col, fire_col)

    analysis_dt <- compute_pareto_frontier(copy(pareto_dt), criterion_cols)
    analysis_dt[, WeatherScenario := weather]
    analysis_dt[, HabitatSpecies := habitat_label]

    time_summary <- analysis_dt[
      , .(
        CarbonUtil = mean(CarbonUtil, na.rm = TRUE),
        TimberUtil = mean(TimberUtil, na.rm = TRUE),
        HabitatUtil = mean(get(habitat_col), na.rm = TRUE),
        FireUtil = mean(get(fire_col), na.rm = TRUE),
        TimeSinceFire = mean(TimeSinceFire, na.rm = TRUE)
      ),
      by = .(Planted, TSF)
    ]
    time_summary[, WeatherScenario := weather]
    time_summary[, HabitatSpecies := habitat_label]

    time_criterion_cols <- c("CarbonUtil", "TimberUtil", "HabitatUtil", "FireUtil")
    time_frontier <- compute_time_frontier(time_summary, time_criterion_cols)
    time_frontier[, WeatherScenario := weather]
    time_frontier[, HabitatSpecies := habitat_label]
    time_frontier_list[[length(time_frontier_list) + 1L]] <- time_frontier

    traj_predictions <- rbindlist(lapply(criterion_cols, function(criterion_name) {
      rbindlist(lapply(c("P", "NP"), function(trt) {
        fit_dt <- analysis_dt[Planted == trt]
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
        pred_dt
      }))
    }))
    trajectory_list[[length(trajectory_list) + 1L]] <- traj_predictions

    gam_fit_list <- lapply(criterion_cols, function(criterion_name) {
      fit_dt <- analysis_dt[, .(
        TimeSinceFire,
        Planted = factor(Planted, levels = c("P", "NP")),
        UtilityValue = get(criterion_name)
      )]

      mgcv::gam(
        UtilityValue ~ Planted + s(TimeSinceFire) + s(TimeSinceFire, by = Planted),
        data = fit_dt,
        method = "REML"
      )
    })
    names(gam_fit_list) <- criterion_cols

    diff_grid <- data.table(
      TimeSinceFire = seq(
        min(analysis_dt$TimeSinceFire, na.rm = TRUE),
        max(analysis_dt$TimeSinceFire, na.rm = TRUE),
        length.out = 60
      )
    )

    pred_grid <- data.table(expand.grid(
      TimeSinceFire = diff_grid$TimeSinceFire,
      Planted = factor(c("P", "NP"), levels = c("P", "NP"))
    ))

    pred_curve_dt <- rbindlist(lapply(criterion_cols, function(criterion_name) {
      fit <- gam_fit_list[[criterion_name]]

      pred <- predict(
        fit,
        newdata = pred_grid,
        se.fit = TRUE,
        type = "response"
      )

      pred_dt <- data.table(
        Criterion = criterion_name,
        TimeSinceFire = pred_grid$TimeSinceFire,
        Planted = pred_grid$Planted,
        UtilityValue = as.numeric(pred$fit),
        UtilitySE = as.numeric(pred$se.fit),
        WeatherScenario = weather,
        HabitatSpecies = habitat_label
      )

      dcast(pred_dt, Criterion + TimeSinceFire + WeatherScenario + HabitatSpecies ~ Planted, value.var = c("UtilityValue", "UtilitySE"))
    }))

    pred_curve_dt[, `:=`(
      Diff = UtilityValue_P - UtilityValue_NP,
      SE_diff = sqrt(UtilitySE_P^2 + UtilitySE_NP^2),
      CI_low = Diff - 1.96 * SE_diff,
      CI_high = Diff + 1.96 * SE_diff
    )]
    pred_curve_dt[, Criterion := factor(
      Criterion,
      levels = criterion_cols,
      labels = c("Carbon", "Timber", habitat_label, "Fire")
    )]
    diff_curve_list[[length(diff_curve_list) + 1L]] <- pred_curve_dt

    gam_summary_dt <- rbindlist(lapply(criterion_cols, function(criterion_name) {
      fit <- gam_fit_list[[criterion_name]]
      sm <- summary(fit)

      p_tbl <- as.data.table(sm$p.table, keep.rownames = "Parameter")
      p_tbl[, `:=`(Criterion = criterion_name, WeatherScenario = weather, HabitatSpecies = habitat_label)]

      s_tbl <- as.data.table(sm$s.table, keep.rownames = "SmoothTerm")
      s_tbl[, `:=`(Criterion = criterion_name, WeatherScenario = weather, HabitatSpecies = habitat_label)]

      list(p_tbl = p_tbl, s_tbl = s_tbl)
    }))

    gam_param_list[[length(gam_param_list) + 1L]] <- rbindlist(lapply(gam_summary_dt, function(x) x$p_tbl))
    gam_smooth_list[[length(gam_smooth_list) + 1L]] <- rbindlist(lapply(gam_summary_dt, function(x) x$s_tbl))

    frontier_membership <- analysis_dt[
      , .(
        nPlots = .N,
        FrontierCount = sum(ParetoFrontier, na.rm = TRUE),
        FrontierProportion = mean(ParetoFrontier, na.rm = TRUE)
      ),
      by = .(TSF, Planted)
    ]
    frontier_membership[, FrontierProportion := round(FrontierProportion, 4)]
    frontier_membership[, WeatherScenario := weather]
    frontier_membership[, HabitatSpecies := habitat_label]

    set.seed(123)
    boot_n <- 400L
    bootstrap_frontier <- rbindlist(lapply(unique(analysis_dt$TSF), function(tsf_group) {
      group_dt <- analysis_dt[TSF == tsf_group]
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
    frontier_membership_list[[length(frontier_membership_list) + 1L]] <- frontier_membership

    summary_dt <- analysis_dt[
      , .(
        nPlots = .N,
        MeanCarbon = mean(CarbonUtil, na.rm = TRUE),
        MeanTimber = mean(TimberUtil, na.rm = TRUE),
        MeanHabitat = mean(get(habitat_col), na.rm = TRUE),
        MeanFire = mean(get(fire_col), na.rm = TRUE)
      ),
      by = .(Planted, TSF, management_class, ParetoFrontier)
    ]
    summary_dt[, WeatherScenario := weather]
    summary_dt[, HabitatSpecies := habitat_label]
    summary_list[[length(summary_list) + 1L]] <- summary_dt

    score_dt <- analysis_dt[, .(
      PlotID,
      Planted,
      TSF,
      management_class,
      CarbonUtil,
      TimberUtil,
      HabitatUtil = get(habitat_col),
      FireUtil = get(fire_col),
      ParetoFrontier,
      ParetoRank,
      WeatherScenario = weather,
      HabitatSpecies = habitat_label
    )]
    score_list[[length(score_list) + 1L]] <- score_dt
  }
}

pareto_summary_all <- rbindlist(summary_list)
pareto_score_all <- rbindlist(score_list)
pareto_time_frontier_all <- rbindlist(time_frontier_list)
pareto_trajectory_all <- rbindlist(trajectory_list)
pareto_diff_curve_all <- rbindlist(diff_curve_list)
gam_p_table <- rbindlist(gam_param_list)
gam_s_table <- rbindlist(gam_smooth_list)
frontier_membership_all <- rbindlist(frontier_membership_list)

fwrite(pareto_summary_all, file.path(out_dir, "pareto_frontier_summary.csv"))
fwrite(pareto_score_all, file.path(out_dir, "pareto_frontier_plot_scores.csv"))
fwrite(pareto_time_frontier_all, file.path(out_dir, "pareto_time_frontier_by_tsf.csv"))
fwrite(frontier_membership_all, file.path(out_dir, "pareto_frontier_membership_summary.csv"))
fwrite(pareto_diff_curve_all, file.path(out_dir, "pareto_gam_difference_curve.csv"))
fwrite(gam_p_table, file.path(out_dir, "pareto_gam_parametric_summary.csv"))
fwrite(gam_s_table, file.path(out_dir, "pareto_gam_smooth_summary.csv"))

traj_plot <- ggplot(
  pareto_trajectory_all,
  aes(x = TimeSinceFire, y = UtilityValue, colour = Planted, linetype = Planted)
) +
  geom_line(linewidth = 1.0) +
  facet_grid(WeatherScenario ~ HabitatSpecies, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Time since fire",
    y = "Predicted utility",
    colour = "Treatment",
    linetype = "Treatment",
    title = "GAM trajectories by weather scenario and habitat species"
  )

ggsave(filename = "pareto_trajectory_gam.jpg", path = out_dir, device = "jpeg", dpi = 300, bg = "white")

traj_frontier_plot <- ggplot(
  pareto_time_frontier_all,
  aes(x = TimeSinceFire, y = ParetoRank, colour = Planted)
) +
  geom_point(size = 3, alpha = 0.9) +
  facet_grid(WeatherScenario ~ HabitatSpecies) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Time since fire bin",
    y = "Pareto rank",
    colour = "Treatment",
    title = "Pareto frontier rank by treatment within each TSF bin"
  )

ggsave(filename = "pareto_frontier_time_rank.jpg", path = out_dir, device = "jpeg", dpi = 300, bg = "white")

message("Pareto-frontier workflow complete. Outputs saved to 03_outputs/")
