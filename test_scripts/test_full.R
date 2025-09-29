# --- 1. Simulation definieren ---

mein_design <- define_design(
  id = "subject",
  between = list(group = c("Control", "Treatment")),
  within = list(time = c(-1, 0, 1)),
  nesting_vars = list(school = 1:10)
)

# Definiere die Modellformel
sim_formula <- y ~ group * time + (1 | subject)

# --- 2. Erwartete Koeffizienten-Namen abrufen ---

# Nutze die Helferfunktion, um die exakten Namen zu bekommen.
# Das vermeidet Tippfehler und falsche Annahmen.
get_fixed_effects_structure(
  formula = formula(sim_formula),
  design = mein_design
)

means.df <- tidyr::expand_grid(group = c("Control", "Treatment"), time = c(-1,0,1)) %>%
  dplyr::mutate(outcome = c(#Control, -1, 0, 1
                          5, 5, 5,
                          #Treatment, -1, -, 1
                          5, 7, 9))

fixed_effects_from_means(formula = sim_formula, means = means.df)

sim_fixed_effects <- list(
  `(Intercept)` = 0,
  groupTreatment = 7.1,
  time = 5,
  `groupTreatment:time` = 5.4
)

get_random_effects_structure(sim_formula, mein_design)

sim_random_effects <- list(
  subject = list(
    `(Intercept)` = 9
  ),
  sd_resid = 4
)

plot_sim_model(sim_formula, mein_design, fixed_effects = sim_fixed_effects, random_effects = sim_random_effects, n = 50)


power_results <- power_sim(
  formula = sim_formula,
  design = mein_design,
  test_parameter = c("time", "groupTreatment:time"), # Welcher p-Wert interessiert uns?
  fixed_effects = sim_fixed_effects,
  random_effects = sim_random_effects,
  n_sims = 100,
  n_start = 5,
  n_increment = 5,
  parallel_plan = "sequential"
)

plot_sim_model(power_results, type = "power_curve")
plot_sim_model(power_results, type = "spaghetti")

summary(power_results)

# --- 3. Effektstärken festlegen ---

# Definiere die "Bauplan-Teile" für die Simulation.
# Die Namen müssen exakt mit der Ausgabe von get_fixed_effects_structure() übereinstimmen.
# Annahme: Control (10 -> 11), Treatment (10 -> 13)
# -> Intercept=10, groupTreatment=0, timepost=1, groupTreatment:timepost=2
sim_fixed_effects <- list(
  `(Intercept)`              = 10,
  `groupTreatment`           = 0,
  `timepost`                 = 1,
  `groupTreatment:timepost`  = 0.5  # Der Effekt von Interesse
)

# Definiere die Varianzen der Zufallseffekte
sim_random_effects <- list(
  subject = list(
    `(Intercept)` = 2.5,
    timepost = 1,
    cor_Intercept__timepost = 0.5
  ),
  sd_resid = 2.5
)


# --- 4. Power-Simulation durchführen ---

# Führe die Hauptfunktion aus.
# Für einen schnellen Test nutzen wir wenige Wiederholungen (n_sims)
# und kleine Stichprobenschritte (sim_steps).
# parallel_plan = "sequential" ist gut für schnelles Testen und Debugging.
power_results <- power_sim(
  formula = sim_formula,
  design = mein_design,
  test_parameter = "groupTreatment:timefollowup", # Welcher p-Wert interessiert uns?
  fixed_effects = sim_fixed_effects,
  random_effects = sim_random_effects,
  n_sims = 100,
  n_start = 20,
  n_increment = 5,
  parallel_plan = "sequential"
)

plot_sim_model(sim_formula, mein_design, fixed_effects = sim_fixed_effects, random_effects = sim_random_effects, n = 30)
plot_sim_model(power_results, type = "power_curve")

# --- 5. Ergebnisse anzeigen ---

cat("\nErgebnis der Power-Analyse:\n")
print(power_results)

sim.formula.test2 <- y~time + (time|subject)
des.test2 <- define_design(id = "subject", within = list(time = c("pre", "post")))
get_fixed_effects_structure(formula = sim.formula.test2, design = des.test2)
fe.test2 <- list(
  `(Intercept)`              = 10,
  `timepost`                 = 3
)
get_random_effects_structure(sim.formula.test2, design = des.test2)
re.test2 <- list(
  subject = list(
    `(Intercept)` = 5,
    timepost = 4,
    cor_Intercept__timepost = 0.9
  ),
  sd_resid = 4
)

plot_sim_model(sim.formula.test2, des.test2, fixed_effects = fe.test2, random_effects = re.test2, n = 50)

