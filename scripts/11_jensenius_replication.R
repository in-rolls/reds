library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(patchwork)
library(sandwich)
library(lmtest)
library(fwildclusterboot)

OUTPUT_DIR <- "figs"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("JENSENIUS (2015) REPLICATION WITH WILD CLUSTER BOOTSTRAP\n")
cat(strrep("=", 70), "\n")
cat("Original paper: 'Development from Representation?'\n")
cat("AEJ: Applied Economics, Vol. 7, No. 3, pp. 196-220\n\n")

set.seed(42)

load("data/replications/jensenius/AEJApp-2014-0201_Dataset/devDTA.Rdata")
devDTA <- as_tibble(devDTA)

cat("DATA SUMMARY\n")
cat(strrep("-", 50), "\n")
cat(sprintf("Observations (constituencies): %d\n", nrow(devDTA)))
cat(sprintf("States (clusters): %d\n", n_distinct(devDTA$State_no_2001_old)))
cat(sprintf("SC reserved: %d\n", sum(devDTA$AC_type_noST == "SC", na.rm = TRUE)))
cat(sprintf("General: %d\n", sum(devDTA$AC_type_noST == "GEN", na.rm = TRUE)))

outcomes <- tribble(
  ~varname,        ~label,
  "Psc",           "% SC population",
  "Plit_7",        "Literacy rate",
  "P_W",           "Employment rate",
  "P_al",          "Agricultural laborers",
  "P_elecVD01",    "Electricity in village",
  "P_educVD01",    "School in village",
  "P_medicVD01",   "Medical facility",
  "P_commVD01",    "Communication channel",
  "Plit_7_gap",    "Literacy gap (SC-nonSC)",
  "P_W_gap",       "Employment gap",
  "P_al_gap",      "Agri laborer gap"
)

run_regression <- function(varname, label, data) {
  if (!varname %in% names(data)) {
    return(NULL)
  }

  data_clean <- data %>%
    filter(!is.na(.data[[varname]]), !is.na(AC_type_noST)) %>%
    mutate(treatment = if_else(AC_type_noST == "SC", 1L, 0L))

  if (nrow(data_clean) < 100) {
    return(NULL)
  }

  formula <- as.formula(paste(varname, "~ treatment"))
  model <- lm(formula, data = data_clean)

  beta <- coef(model)["treatment"]

  se_hc1 <- sqrt(vcovHC(model, type = "HC1")["treatment", "treatment"])
  t_hc1 <- beta / se_hc1

  se_state <- tryCatch(
    sqrt(vcovCL(model, cluster = data_clean$State_no_2001_old, type = "HC1")["treatment", "treatment"]),
    error = function(e) NA_real_
 )
  t_state <- if (!is.na(se_state)) beta / se_state else NA_real_

  p_boot <- tryCatch({
    boot_result <- boottest(
      model,
      clustid = "State_no_2001_old",
      param = "treatment",
      B = 999,
      type = "webb"
    )
    boot_result$p_val
  }, error = function(e) NA_real_)

  tibble(
    outcome = label,
    varname = varname,
    coef = beta,
    t_hc1 = t_hc1,
    t_state = t_state,
    p_boot = p_boot,
    sig_hc1 = abs(t_hc1) > 1.96,
    sig_state = !is.na(t_state) && abs(t_state) > 1.96,
    sig_boot = !is.na(p_boot) && p_boot < 0.05
  )
}

cat("\n\nINFERENCE COMPARISON\n")
cat(strrep("-", 70), "\n")
cat(sprintf("%-30s %8s %8s %8s %10s %10s\n",
            "Outcome", "Coef", "t(HC1)", "t(State)", "p(Boot)", "Verdict"))
cat(strrep("-", 70), "\n")

results <- outcomes %>%
  pmap(function(varname, label) run_regression(varname, label, devDTA)) %>%
  compact() %>%
  bind_rows() %>%
  mutate(
    verdict = case_when(
      sig_hc1 & sig_state & sig_boot ~ "ROBUST",
      sig_hc1 & !sig_boot ~ "FRAGILE",
      !sig_hc1 ~ "Not sig",
      TRUE ~ "Mixed"
    )
  )

results %>%
  rowwise() %>%
  mutate(
    output = sprintf("%-30s %8.2f %8.2f %8.2f %10.3f %10s\n",
                     outcome, coef, t_hc1,
                     if_else(is.na(t_state), NA_real_, t_state),
                     if_else(is.na(p_boot), NA_real_, p_boot),
                     verdict)
  ) %>%
  pull(output) %>%
  cat()

cat("\n\nSUMMARY\n")
cat(strrep("-", 50), "\n")
cat(sprintf("Results significant with HC1: %d / %d\n",
            sum(results$sig_hc1), nrow(results)))
cat(sprintf("Results significant with state clustering: %d / %d\n",
            sum(results$sig_state, na.rm = TRUE), nrow(results)))
cat(sprintf("Results significant with wild bootstrap: %d / %d\n",
            sum(results$sig_boot, na.rm = TRUE), nrow(results)))

flips <- sum(results$sig_hc1 & !results$sig_boot, na.rm = TRUE)
cat(sprintf("\nResults that FLIP (HC1 sig -> bootstrap not sig): %d\n", flips))

fragile <- results %>% filter(sig_hc1, !sig_boot, !is.na(p_boot))
if (nrow(fragile) > 0) {
  cat("\nFRAGILE RESULTS (don't trust):\n")
  fragile %>%
    mutate(msg = sprintf("  - %s: t=%.1f (HC1) but p=%.3f (bootstrap)\n",
                         outcome, t_hc1, p_boot)) %>%
    pull(msg) %>%
    cat()
}

robust <- results %>% filter(sig_boot)
cat("\nROBUST RESULTS (can trust):\n")
if (nrow(robust) > 0) {
  robust %>%
    mutate(msg = sprintf("  - %s: t=%.1f (state), p=%.3f (bootstrap)\n",
                         outcome, t_state, p_boot)) %>%
    pull(msg) %>%
    cat()
} else {
  cat("  (none)\n")
}

cat("\n\nVISUALIZATION\n")
cat(strrep("-", 50), "\n")

results_plot <- results %>%
  filter(!is.na(t_state), !is.na(p_boot))

p1 <- ggplot(results_plot, aes(x = reorder(outcome, abs(t_hc1)))) +
  geom_segment(aes(xend = outcome, y = abs(t_hc1), yend = abs(t_state)),
               color = "gray50", linewidth = 1) +
  geom_point(aes(y = abs(t_hc1)), color = "#4a7c9b", size = 3) +
  geom_point(aes(y = abs(t_state)), color = "#d62728", size = 3) +
  geom_hline(yintercept = 1.96, linetype = "dashed", color = "black") +
  coord_flip() +
  labs(
    x = NULL,
    y = "|t-statistic|",
    title = "t-Statistics: HC1 (blue) vs State Cluster (red)",
    subtitle = "Dashed line = significance threshold (1.96)"
  ) +
  theme_minimal()

p2 <- ggplot(results_plot, aes(x = reorder(outcome, -p_boot), y = p_boot)) +
  geom_col(aes(fill = p_boot < 0.05), alpha = 0.85) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("TRUE" = "#2ca02c", "FALSE" = "#d62728"),
                    guide = "none") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Wild Bootstrap p-value",
    title = "Wild Cluster Bootstrap p-values (15 state clusters)",
    subtitle = "Green = significant (p<0.05), Red line = 0.05 threshold"
  ) +
  theme_minimal()

combined <- p1 / p2 +
  plot_annotation(
    title = "Jensenius (2015) Replication: Does Clustering Matter?",
    subtitle = "Effect of SC reservation on development outcomes",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

ggsave(
  file.path(OUTPUT_DIR, "jensenius_replication.png"),
  combined,
  width = 12,
  height = 10,
  dpi = 150
)
cat(sprintf("\nSaved: %s\n", file.path(OUTPUT_DIR, "jensenius_replication.png")))

cat("\n\nKEY TAKEAWAY\n")
cat(strrep("=", 70), "\n")
cat("With only 15 state clusters, wild cluster bootstrap is essential.\n")
cat("Results that look significant with HC1 may not survive proper inference.\n")
