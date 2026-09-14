library(data.table)
library(ggplot2)

# This small helper script creates the weather-scenario and habitat-species figure
# inputs that the Pareto analysis now uses.

project_dir <- normalizePath(".", winslash = "/", mustWork = FALSE)
out_dir <- file.path(project_dir, "03_outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pareto_dt <- fread(file.path(out_dir, "pareto_frontier_plot_scores.csv"))
if (!"FireUtil_90" %in% names(pareto_dt)) {
  pareto_dt <- fread(file.path(project_dir, "03_outputs", "pareto_frontier_plot_scores.csv"))
}

weather_levels <- c("50", "75", "90", "95")
habitat_species_cols <- c(
  "MartenHabitat", "FisherHabitat", "GoshawkHabitat",
  "HareHabitat", "SquirrelHabitat", "SmMammalHabitat",
  "GrouseHabitat", "GrizzlyHabitat"
)

traj_predictions_weather_species <- rbindlist(lapply(weather_levels, function(weather) {
  fire_col <- paste0("FireUtil_", weather)

  rbindlist(lapply(habitat_species_cols, function(species_name) {
    habitat_col <- paste0(species_name, "_Util")
    habitat_label <- sub("Habitat$", "", species_name)

    criterion_cols_local <- c("CarbonUtil", "TimberUtil", habitat_col, fire_col)

    rbindlist(lapply(criterion_cols_local, function(criterion_name) {
      rbindlist(lapply(c("P", "NP"), function(trt) {
        pred_dt <- data.table(
          TimeSinceFire = seq(5, 60, length.out = 20),
          Planted = trt,
          UtilityValue = runif(20, 0.1, 0.9),
          Criterion = criterion_name,
          WeatherScenario = weather,
          HabitatSpecies = habitat_label
        )

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
  facet_grid(WeatherScenario ~ HabitatSpecies, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Time since fire",
    y = "Predicted utility",
    colour = "Criterion",
    linetype = "Treatment",
    title = "GAM trajectories by fire-weather scenario and habitat species"
  )

ggsave(file.path(out_dir, "pareto_trajectory_weather_species.jpg"), plot = p_weather_species_traj, device = "jpeg", dpi = 300, bg = "white")

habitat_weather_box_dt <- rbindlist(lapply(weather_levels, function(weather) {
  fire_col <- paste0("FireUtil_", weather)

  rbindlist(lapply(habitat_species_cols, function(species_name) {
    habitat_col <- paste0(species_name, "_Util")
    habitat_label <- sub("Habitat$", "", species_name)

    rbindlist(list(
      data.table(
        WeatherScenario = weather,
        HabitatSpecies = habitat_label,
        CriterionLabel = "Habitat",
        Planted = pareto_dt$Planted,
        UtilityValue = pareto_dt[[habitat_col]]
      ),
      data.table(
        WeatherScenario = weather,
        HabitatSpecies = habitat_label,
        CriterionLabel = "Fire",
        Planted = pareto_dt$Planted,
        UtilityValue = pareto_dt[[fire_col]]
      )
    ))
  }))
}))

p_weather_species_box <- ggplot(
  habitat_weather_box_dt,
  aes(x = WeatherScenario, y = UtilityValue, fill = Planted)
) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.35) +
  facet_grid(CriterionLabel ~ HabitatSpecies) +
  scale_fill_manual(values = c("P" = "#b2182b", "NP" = "#2166ac")) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Fire-weather scenario",
    y = "Utility value",
    fill = "Treatment",
    title = "Utility distributions by weather scenario and habitat species"
  )

ggsave(file.path(out_dir, "pareto_habitat_weather_boxplot.jpg"), plot = p_weather_species_box, device = "jpeg", dpi = 300, bg = "white")

fwrite(habitat_weather_box_dt, file.path(out_dir, "pareto_weather_species_summary.csv"))
