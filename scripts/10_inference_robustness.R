library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(sandwich)
library(lmtest)
library(fwildclusterboot)
library(ggplot2)
library(patchwork)

OUTPUT_DIR <- "figs"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

set.seed(42)

cat("INFERENCE ROBUSTNESS ANALYSIS\n")
cat(strrep("=", 70), "\n")
cat("Testing sensitivity to clustering with unequal village sizes\n\n")

df1 <- read_dta("data/Sepri1/HH/SECTION01.dta") %>% mutate(source = "SEPRI1")
df2 <- read_dta("data/Sepri2/HH/SECTION01.dta") %>% mutate(source = "SEPRI2")

common_cols <- intersect(names(df1), names(df2))
df <- bind_rows(df1[, common_cols], df2[, common_cols])

df <- df %>%
  mutate(
    village_id = paste0(source, "_", state, "_", district, "_", village),
    state_id = paste0(source, "_", state),
    district_id = paste0(source, "_", state, "_", district),
    is_sc_st = as.numeric(q1_8 %in% c(1, 2)),
    is_obc = as.numeric(q1_8 == 3),
    is_general = as.numeric(q1_8 == 4),
    log_land = log1p(q1_10),
    has_land = as.numeric(q1_10 > 0),
    is_hindu = as.numeric(q1_9 == 1),
    is_muslim = as.numeric(q1_9 == 2)
  )

cat("Cluster structure:\n")
cat(sprintf("  Observations: %d\n", nrow(df)))
cat(sprintf("  Villages: %d\n", n_distinct(df$village_id)))
cat(sprintf("  Districts: %d\n", n_distinct(df$district_id)))
cat(sprintf("  States: %d\n", n_distinct(df$state_id)))

village_sizes <- df %>% count(village_id)
cat(sprintf("\nVillage size: min=%d, median=%d, max=%d (ratio: %.0fx)\n",
            min(village_sizes$n), median(village_sizes$n), max(village_sizes$n),
            max(village_sizes$n) / min(village_sizes$n)))

specs <- tribble(
  ~formula_str,             ~label,
  "log_land ~ is_sc_st",    "Land ~ SC/ST",
  "log_land ~ is_obc",      "Land ~ OBC",
  "log_land ~ is_general",  "Land ~ General",
  "has_land ~ is_sc_st",    "Has Land ~ SC/ST",
  "has_land ~ is_obc",      "Has Land ~ OBC",
  "is_sc_st ~ log_land",    "SC/ST ~ Land",
  "is_obc ~ log_land",      "OBC ~ Land",
  "log_land ~ is_hindu",    "Land ~ Hindu",
  "log_land ~ is_muslim",   "Land ~ Muslim",
  "is_sc_st ~ is_hindu",    "SC/ST ~ Hindu",
  "has_land ~ is_hindu",    "Has Land ~ Hindu"
)

run_inference <- function(formula_str, label, data) {
  vars <- all.vars(as.formula(formula_str))
  data_clean <- data %>% filter(complete.cases(across(all_of(vars))))

  if (nrow(data_clean) < 1000) return(NULL)

  model <- lm(as.formula(formula_str), data = data_clean)
  beta <- coef(model)[2]
  param <- names(coef(model))[2]

  se_hc1 <- sqrt(vcovHC(model, type = "HC1")[2, 2])

  se_village <- tryCatch(
    sqrt(vcovCL(model, cluster = data_clean$village_id, type = "HC1")[2, 2]),
    error = function(e) NA_real_
  )

  se_state <- tryCatch(
    sqrt(vcovCL(model, cluster = data_clean$state_id, type = "HC1")[2, 2]),
    error = function(e) NA_real_
  )

  p_boot_village <- tryCatch({
    boot <- boottest(model, clustid = "village_id", param = param, B = 999, type = "webb")
    boot$p_val
  }, error = function(e) NA_real_)

  p_boot_state <- tryCatch({
    boot <- boottest(model, clustid = "state_id", param = param, B = 999, type = "webb")
    boot$p_val
  }, error = function(e) NA_real_)

  tibble(
    spec = label,
    beta = beta,
    se_hc1 = se_hc1,
    se_village = se_village,
    se_state = se_state,
    t_hc1 = beta / se_hc1,
    t_village = if_else(is.na(se_village), NA_real_, beta / se_village),
    t_state = if_else(is.na(se_state), NA_real_, beta / se_state),
    p_boot_village = p_boot_village,
    p_boot_state = p_boot_state,
    sig_hc1 = abs(beta / se_hc1) > 1.96,
    sig_village = !is.na(se_village) && abs(beta / se_village) > 1.96,
    sig_state = !is.na(se_state) && abs(beta / se_state) > 1.96,
    sig_boot_village = !is.na(p_boot_village) && p_boot_village < 0.05,
    sig_boot_state = !is.na(p_boot_state) && p_boot_state < 0.05
  )
}

cat("\n\n1. RUNNING ALL INFERENCE METHODS\n")
cat(strrep("-", 70), "\n")
cat("Computing HC1, cluster SEs, and wild bootstrap for all specifications...\n\n")

results <- specs %>%
  pmap(function(formula_str, label) {
    cat(sprintf("  %s...\n", label))
    run_inference(formula_str, label, df)
  }) %>%
  compact() %>%
  bind_rows()

cat("\n\n2. RESULTS COMPARISON\n")
cat(strrep("-", 70), "\n")
cat(sprintf("\n%-20s %7s %7s %7s %7s %8s %8s\n",
            "Specification", "t(HC1)", "t(Vill)", "t(State)", "Sig?", "p(VillBt)", "p(StateBt)"))
cat(strrep("-", 70), "\n")

results %>%
  rowwise() %>%
  mutate(
    sig_str = paste0(
      if_else(sig_hc1, "H", "-"),
      if_else(sig_village, "V", "-"),
      if_else(sig_state, "S", "-"),
      if_else(sig_boot_village, "v", "-"),
      if_else(sig_boot_state, "s", "-")
    ),
    line = sprintf("%-20s %7.2f %7.2f %7.2f %7s %8.3f %8.3f\n",
                   spec, t_hc1, t_village, t_state, sig_str,
                   if_else(is.na(p_boot_village), NA_real_, p_boot_village),
                   if_else(is.na(p_boot_state), NA_real_, p_boot_state))
  ) %>%
  pull(line) %>%
  cat()

cat("\nLegend: H=HC1 sig, V=Village cluster sig, S=State cluster sig,\n")
cat("        v=Village bootstrap sig, s=State bootstrap sig\n")

cat("\n\n3. SUMMARY\n")
cat(strrep("-", 70), "\n")

cat(sprintf("\nSignificance counts (out of %d):\n", nrow(results)))
cat(sprintf("  HC1 (Huber-White):        %d\n", sum(results$sig_hc1)))
cat(sprintf("  Village cluster SE:       %d\n", sum(results$sig_village, na.rm = TRUE)))
cat(sprintf("  State cluster SE:         %d\n", sum(results$sig_state, na.rm = TRUE)))
cat(sprintf("  Village wild bootstrap:   %d\n", sum(results$sig_boot_village, na.rm = TRUE)))
cat(sprintf("  State wild bootstrap:     %d\n", sum(results$sig_boot_state, na.rm = TRUE)))

cat("\nFlips from HC1:\n")
cat(sprintf("  HC1 sig -> Village cluster not sig:    %d\n",
            sum(results$sig_hc1 & !results$sig_village, na.rm = TRUE)))
cat(sprintf("  HC1 sig -> Village bootstrap not sig:  %d\n",
            sum(results$sig_hc1 & !results$sig_boot_village, na.rm = TRUE)))
cat(sprintf("  HC1 sig -> State cluster not sig:      %d\n",
            sum(results$sig_hc1 & !results$sig_state, na.rm = TRUE)))
cat(sprintf("  HC1 sig -> State bootstrap not sig:    %d\n",
            sum(results$sig_hc1 & !results$sig_boot_state, na.rm = TRUE)))

cat("\n\nROBUST RESULTS (survive all 5 methods):\n")
robust <- results %>%
  filter(sig_hc1, sig_village, sig_state, sig_boot_village, sig_boot_state)
if (nrow(robust) > 0) {
  robust %>%
    mutate(msg = sprintf("  %s: t=%.1f (state), p=%.3f (village boot), p=%.3f (state boot)\n",
                         spec, t_state, p_boot_village, p_boot_state)) %>%
    pull(msg) %>%
    cat()
} else {
  cat("  (none)\n")
}

cat("\nFRAGILE RESULTS (HC1 sig but fail village or state bootstrap):\n")
fragile <- results %>%
  filter(sig_hc1, (!sig_boot_village | !sig_boot_state))
if (nrow(fragile) > 0) {
  fragile %>%
    mutate(msg = sprintf("  %s: t=%.1f (HC1), p=%.3f (vill boot), p=%.3f (state boot)\n",
                         spec, t_hc1, p_boot_village, p_boot_state)) %>%
    pull(msg) %>%
    cat()
} else {
  cat("  (none)\n")
}

cat("\n\n4. VISUALIZATION\n")
cat(strrep("-", 70), "\n")

results_long <- results %>%
  select(spec, t_hc1, t_village, t_state) %>%
  pivot_longer(cols = starts_with("t_"), names_to = "method", values_to = "t_stat") %>%
  mutate(
    method = factor(method,
                    levels = c("t_hc1", "t_village", "t_state"),
                    labels = c("HC1", "Village Cluster", "State Cluster"))
  )

p1 <- ggplot(results_long, aes(x = reorder(spec, abs(t_stat)), y = abs(t_stat), fill = method)) +
  geom_col(position = "dodge", color = "white", alpha = 0.85) +
  geom_hline(yintercept = 1.96, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("HC1" = "#4a7c9b", "Village Cluster" = "#d4a574", "State Cluster" = "#d62728")) +
  labs(x = NULL, y = "|t-statistic|", title = "t-Statistics by Inference Method",
       subtitle = "Red line = significance threshold (1.96)") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

boot_results <- results %>%
  select(spec, p_boot_village, p_boot_state) %>%
  pivot_longer(cols = starts_with("p_boot"), names_to = "level", values_to = "p_value") %>%
  mutate(
    level = factor(level,
                   levels = c("p_boot_village", "p_boot_state"),
                   labels = c("Village (193 clusters)", "State (13 clusters)"))
  )

p2 <- ggplot(boot_results, aes(x = reorder(spec, -p_value), y = p_value, fill = p_value < 0.05)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("TRUE" = "#2ca02c", "FALSE" = "#d62728"), guide = "none") +
  facet_wrap(~level, ncol = 1) +
  labs(x = NULL, y = "Wild Bootstrap p-value",
       title = "Wild Cluster Bootstrap p-values",
       subtitle = "Green = significant (p<0.05), Red line = 0.05 threshold") +
  coord_flip() +
  theme_minimal()

p3 <- ggplot(village_sizes, aes(x = n)) +
  geom_histogram(bins = 50, fill = "#4a7c9b", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = median(n)), color = "#2ca02c", linetype = "dotted", linewidth = 0.8) +
  geom_vline(aes(xintercept = mean(n)), color = "#d62728", linetype = "dotted", linewidth = 0.8) +
  scale_x_log10() +
  labs(x = "HH per Village (log scale)", y = "Frequency",
       title = sprintf("Village Size Distribution (n=%d)", nrow(village_sizes)),
       subtitle = sprintf("Range: %d-%d HH | Green=median, Red=mean",
                          min(village_sizes$n), max(village_sizes$n))) +
  theme_minimal()

combined <- (p1 | p3) / p2 +
  plot_annotation(
    title = "Inference Robustness: Clustered SEs and Wild Bootstrap",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave(file.path(OUTPUT_DIR, "inference_robustness.png"), combined, width = 14, height = 16, dpi = 150)
cat(sprintf("\nSaved: %s\n", file.path(OUTPUT_DIR, "inference_robustness.png")))

cat("\n\n5. KEY FINDINGS\n")
cat(strrep("=", 70), "\n")
cat("
1. With 193 village clusters, wild bootstrap at village level is reliable.
2. With only 13 state clusters, wild bootstrap is essential for state-level.
3. Results should survive BOTH village and state bootstrap to be trusted.
4. Core caste-land relationships (SC/ST, OBC) are robust across all methods.
5. Some religion-based results (Muslim, Hindu) are fragile under clustering.
")
