library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(patchwork)
library(sandwich)
library(lmtest)
library(haven)
library(fwildclusterboot)
library(fixest)

OUTPUT_DIR <- "figs"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("MUNSHI & ROSENZWEIG (2015) REPLICATION WITH WILD CLUSTER BOOTSTRAP\n")
cat(strrep("=", 70), "\n")
cat("Original paper: 'Networks and Misallocation: Insurance, Migration,\n")
cat("                 and the Rural-Urban Wage Gap'\n")
cat("American Economic Review, 2015\n\n")

set.seed(999)

d8 <- read_dta("data/replications/munshi_rosenzweig/data/table8a.dta")
d6 <- read_dta("data/replications/munshi_rosenzweig/data/table6.dta")

d6_filtered <- d6 %>%
  filter(total >= 30, cvsq < 100, icrisat == 1)

cat("DATA SUMMARY\n")
cat(strrep("-", 50), "\n")
cat("Table 8a:\n")
cat(sprintf("  Observations: %d\n", nrow(d8)))
cat(sprintf("  States (clusters): %d\n", n_distinct(d8$state)))
cat("\nTable 6:\n")
cat(sprintf("  Observations (after filter): %d\n", nrow(d6_filtered)))
cat(sprintf("  Villages: %d\n", n_distinct(d6_filtered$village)))
cat(sprintf("  Castes: %d\n", n_distinct(d6_filtered$castecode)))

run_table8_regression <- function(outcome, predictors, data, label) {
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  data_clean <- data %>% drop_na(all_of(c(outcome, predictors, "state")))

  if (nrow(data_clean) < 50) return(NULL)

  model <- lm(formula, data = data_clean)

  key_pred <- predictors[1]
  beta <- coef(model)[key_pred]

  se_hc1 <- sqrt(vcovHC(model, type = "HC1")[key_pred, key_pred])
  t_hc1 <- beta / se_hc1

  se_state <- tryCatch(
    sqrt(vcovCL(model, cluster = data_clean$state, type = "HC1")[key_pred, key_pred]),
    error = function(e) NA_real_
  )
  t_state <- if (!is.na(se_state)) beta / se_state else NA_real_

  p_boot <- tryCatch({
    boot_result <- boottest(
      model,
      clustid = "state",
      param = key_pred,
      B = 999,
      type = "webb"
    )
    boot_result$p_val
  }, error = function(e) NA_real_)

  tibble(
    table = "Table 8a",
    label = label,
    outcome = outcome,
    key_predictor = key_pred,
    n_obs = nrow(data_clean),
    n_clusters = n_distinct(data_clean$state),
    coef = beta,
    t_hc1 = t_hc1,
    t_state = t_state,
    p_boot = p_boot,
    sig_hc1 = abs(t_hc1) > 1.96,
    sig_state = !is.na(t_state) && abs(t_state) > 1.96,
    sig_boot = !is.na(p_boot) && p_boot < 0.05
  )
}

cat("\n\nTABLE 8a REGRESSIONS (15 state clusters)\n")
cat(strrep("-", 70), "\n")

table8_specs <- list(
  list(
    outcome = "dpout10",
    predictors = c("pdinc10", "pjdincx10", "mark3d", "jmark3d", "share71", "jshare71"),
    label = "Outmig10 ~ own inc"
  ),
  list(
    outcome = "dpout10",
    predictors = c("pjdincx10", "pdinc10", "mark3d", "jmark3d", "share71", "jshare71"),
    label = "Outmig10 ~ jati inc"
  ),
  list(
    outcome = "dpoutvb5",
    predictors = c("pdinc5", "pjdincx5", "mark3d", "jmark3d", "share71", "jshare71"),
    label = "Outmig5 ~ own inc"
  ),
  list(
    outcome = "dpoutvb5",
    predictors = c("pjdincx5", "pdinc5", "mark3d", "jmark3d", "share71", "jshare71"),
    label = "Outmig5 ~ jati inc"
  )
)

results_8 <- table8_specs %>%
  map(~ run_table8_regression(.x$outcome, .x$predictors, d8, .x$label)) %>%
  compact() %>%
  bind_rows()

cat(sprintf("%-25s %8s %8s %8s %10s %10s\n",
            "Specification", "Coef", "t(HC1)", "t(State)", "p(Boot)", "Verdict"))
cat(strrep("-", 70), "\n")

results_8 <- results_8 %>%
  mutate(
    verdict = case_when(
      sig_hc1 & sig_state & sig_boot ~ "ROBUST",
      sig_hc1 & !sig_boot ~ "FRAGILE",
      !sig_hc1 ~ "Not sig",
      TRUE ~ "Mixed"
    )
  )

results_8 %>%
  rowwise() %>%
  mutate(
    output = sprintf("%-25s %8.4f %8.2f %8.2f %10.3f %10s\n",
                     label, coef, t_hc1,
                     if_else(is.na(t_state), NA_real_, t_state),
                     if_else(is.na(p_boot), NA_real_, p_boot),
                     verdict)
  ) %>%
  pull(output) %>%
  cat()

run_table6_regression <- function(outcome, predictors, data, label, cluster_var, include_village_fe = FALSE) {
  if (include_village_fe) {
    formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + "), "| village"))
    data_clean <- data %>% drop_na(all_of(c(outcome, predictors, cluster_var, "village")))
    model <- feols(formula, data = data_clean)
    coefs <- coef(model)
    vcov_hc1 <- vcov(model, vcov = "hetero")
    vcov_clust <- vcov(model, vcov = ~castecode)
  } else {
    formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
    data_clean <- data %>% drop_na(all_of(c(outcome, predictors, cluster_var)))
    model <- lm(formula, data = data_clean)
    coefs <- coef(model)
    vcov_hc1 <- vcovHC(model, type = "HC1")
    vcov_clust <- tryCatch(
      vcovCL(model, cluster = data_clean[[cluster_var]], type = "HC1"),
      error = function(e) NULL
    )
  }

  if (nrow(data_clean) < 100) return(NULL)

  key_pred <- predictors[1]
  beta <- coefs[key_pred]

  se_hc1 <- sqrt(vcov_hc1[key_pred, key_pred])
  t_hc1 <- beta / se_hc1

  if (!is.null(vcov_clust)) {
    se_clust <- sqrt(vcov_clust[key_pred, key_pred])
    t_clust <- beta / se_clust
  } else {
    t_clust <- NA_real_
  }

  if (!include_village_fe) {
    p_boot <- tryCatch({
      boot_result <- boottest(
        model,
        clustid = cluster_var,
        param = key_pred,
        B = 999,
        type = "webb"
      )
      boot_result$p_val
    }, error = function(e) NA_real_)
  } else {
    p_boot <- tryCatch({
      boot_result <- boottest(
        model,
        clustid = "castecode",
        param = key_pred,
        B = 999,
        type = "webb"
      )
      boot_result$p_val
    }, error = function(e) NA_real_)
  }

  tibble(
    table = "Table 6",
    label = label,
    outcome = outcome,
    key_predictor = key_pred,
    n_obs = nrow(data_clean),
    n_clusters = n_distinct(data_clean[[cluster_var]]),
    coef = beta,
    t_hc1 = t_hc1,
    t_clust = t_clust,
    p_boot = p_boot,
    sig_hc1 = abs(t_hc1) > 1.96,
    sig_clust = !is.na(t_clust) && abs(t_clust) > 1.96,
    sig_boot = !is.na(p_boot) && p_boot < 0.05
  )
}

cat("\n\nTABLE 6 REGRESSIONS (148 caste clusters)\n")
cat(strrep("-", 70), "\n")

d6_filtered <- d6_filtered %>%
  mutate(mig = mig1)

table6_specs <- list(
  list(
    outcome = "mig",
    predictors = c("pminc", "jpminc"),
    label = "Mig ~ own inc (1)",
    cluster_var = "castecode",
    include_village_fe = FALSE
  ),
  list(
    outcome = "mig",
    predictors = c("jpminc", "pminc"),
    label = "Mig ~ jati inc (1)",
    cluster_var = "castecode",
    include_village_fe = FALSE
  ),
  list(
    outcome = "mig",
    predictors = c("pminc", "jpminc", "cvsq"),
    label = "Mig ~ own inc (2)",
    cluster_var = "castecode",
    include_village_fe = FALSE
  ),
  list(
    outcome = "mig",
    predictors = c("jpminc", "pminc", "cvsq"),
    label = "Mig ~ jati inc (2)",
    cluster_var = "castecode",
    include_village_fe = FALSE
  ),
  list(
    outcome = "mig",
    predictors = c("pminc", "jpminc", "cvsq", "vjpminc"),
    label = "Mig ~ own inc + vill FE",
    cluster_var = "castecode",
    include_village_fe = TRUE
  ),
  list(
    outcome = "mig",
    predictors = c("jpminc", "pminc", "cvsq", "vjpminc"),
    label = "Mig ~ jati inc + vill FE",
    cluster_var = "castecode",
    include_village_fe = TRUE
  )
)

results_6 <- table6_specs %>%
  map(~ run_table6_regression(.x$outcome, .x$predictors, d6_filtered,
                               .x$label, .x$cluster_var, .x$include_village_fe)) %>%
  compact() %>%
  bind_rows()

cat(sprintf("%-25s %8s %8s %8s %10s %10s\n",
            "Specification", "Coef", "t(HC1)", "t(Caste)", "p(Boot)", "Verdict"))
cat(strrep("-", 70), "\n")

results_6 <- results_6 %>%
  mutate(
    verdict = case_when(
      sig_hc1 & sig_clust & sig_boot ~ "ROBUST",
      sig_hc1 & !sig_boot ~ "FRAGILE",
      !sig_hc1 ~ "Not sig",
      TRUE ~ "Mixed"
    )
  )

results_6 %>%
  rowwise() %>%
  mutate(
    output = sprintf("%-25s %8.4f %8.2f %8.2f %10.3f %10s\n",
                     label, coef, t_hc1,
                     if_else(is.na(t_clust), NA_real_, t_clust),
                     if_else(is.na(p_boot), NA_real_, p_boot),
                     verdict)
  ) %>%
  pull(output) %>%
  cat()

cat("\n\nSUMMARY\n")
cat(strrep("=", 70), "\n")

cat("\nTable 8a (15 state clusters):\n")
cat(sprintf("  HC1 significant: %d / %d\n", sum(results_8$sig_hc1), nrow(results_8)))
cat(sprintf("  State cluster significant: %d / %d\n", sum(results_8$sig_state, na.rm = TRUE), nrow(results_8)))
cat(sprintf("  Wild bootstrap significant: %d / %d\n", sum(results_8$sig_boot, na.rm = TRUE), nrow(results_8)))
flips_8 <- sum(results_8$sig_hc1 & !results_8$sig_boot, na.rm = TRUE)
cat(sprintf("  FLIPS (HC1 sig -> bootstrap not sig): %d\n", flips_8))

cat("\nTable 6 (148 caste clusters):\n")
cat(sprintf("  HC1 significant: %d / %d\n", sum(results_6$sig_hc1), nrow(results_6)))
cat(sprintf("  Caste cluster significant: %d / %d\n", sum(results_6$sig_clust, na.rm = TRUE), nrow(results_6)))
cat(sprintf("  Wild bootstrap significant: %d / %d\n", sum(results_6$sig_boot, na.rm = TRUE), nrow(results_6)))
flips_6 <- sum(results_6$sig_hc1 & !results_6$sig_boot, na.rm = TRUE)
cat(sprintf("  FLIPS (HC1 sig -> bootstrap not sig): %d\n", flips_6))

cat("\n\nVISUALIZATION\n")
cat(strrep("-", 50), "\n")

results_8_plot <- results_8 %>%
  filter(!is.na(t_state), !is.na(p_boot)) %>%
  mutate(cluster_type = "State (15 clusters)")

results_6_plot <- results_6 %>%
  filter(!is.na(t_clust), !is.na(p_boot)) %>%
  rename(t_state = t_clust) %>%
  mutate(cluster_type = "Caste (148 clusters)")

all_results <- bind_rows(results_8_plot, results_6_plot)

p1 <- ggplot(results_8_plot, aes(x = reorder(label, abs(t_hc1)))) +
  geom_segment(aes(xend = label, y = abs(t_hc1), yend = abs(t_state)),
               color = "gray50", linewidth = 1) +
  geom_point(aes(y = abs(t_hc1)), color = "#4a7c9b", size = 3) +
  geom_point(aes(y = abs(t_state)), color = "#d62728", size = 3) +
  geom_hline(yintercept = 1.96, linetype = "dashed", color = "black") +
  coord_flip() +
  labs(
    x = NULL,
    y = "|t-statistic|",
    title = "Table 8a: t-Statistics (HC1 blue, State red)",
    subtitle = "15 state clusters; dashed = 1.96"
  ) +
  theme_minimal()

p2 <- ggplot(results_8_plot, aes(x = reorder(label, -p_boot), y = p_boot)) +
  geom_col(aes(fill = p_boot < 0.05), alpha = 0.85) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("TRUE" = "#2ca02c", "FALSE" = "#d62728"), guide = "none") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Wild Bootstrap p-value",
    title = "Table 8a: Wild Bootstrap p-values",
    subtitle = "Green = sig (p<0.05)"
  ) +
  theme_minimal()

p3 <- ggplot(results_6_plot, aes(x = reorder(label, abs(t_hc1)))) +
  geom_segment(aes(xend = label, y = abs(t_hc1), yend = abs(t_state)),
               color = "gray50", linewidth = 1) +
  geom_point(aes(y = abs(t_hc1)), color = "#4a7c9b", size = 3) +
  geom_point(aes(y = abs(t_state)), color = "#d62728", size = 3) +
  geom_hline(yintercept = 1.96, linetype = "dashed", color = "black") +
  coord_flip() +
  labs(
    x = NULL,
    y = "|t-statistic|",
    title = "Table 6: t-Statistics (HC1 blue, Caste red)",
    subtitle = "148 caste clusters; dashed = 1.96"
  ) +
  theme_minimal()

p4 <- ggplot(results_6_plot, aes(x = reorder(label, -p_boot), y = p_boot)) +
  geom_col(aes(fill = p_boot < 0.05), alpha = 0.85) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("TRUE" = "#2ca02c", "FALSE" = "#d62728"), guide = "none") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Wild Bootstrap p-value",
    title = "Table 6: Wild Bootstrap p-values",
    subtitle = "Green = sig (p<0.05)"
  ) +
  theme_minimal()

combined <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Munshi & Rosenzweig (2015) Replication: Does Clustering Matter?",
    subtitle = "Networks, migration, and the rural-urban wage gap",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

ggsave(
  file.path(OUTPUT_DIR, "munshi_rosenzweig_replication.png"),
  combined,
  width = 14,
  height = 10,
  dpi = 150
)
cat(sprintf("\nSaved: %s\n", file.path(OUTPUT_DIR, "munshi_rosenzweig_replication.png")))

cat("\n\nKEY TAKEAWAY\n")
cat(strrep("=", 70), "\n")
cat("Table 8a uses wild cluster bootstrap in the original paper (15 states).\n")
cat("Table 6 uses standard bootstrap with caste clustering (148 castes).\n")
cat("With many clusters, results are generally stable. With few clusters,\n")
cat("wild bootstrap is essential for valid inference.\n")
