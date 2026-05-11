library(haven)
library(dplyr)
library(tidyr)
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

cat("\n\n1. SE COMPARISON ACROSS METHODS\n")
cat(strrep("-", 70), "\n")

specs <- list(
  c("log_land ~ is_sc_st", "Land ~ SC/ST"),
  c("log_land ~ is_obc", "Land ~ OBC"),
  c("log_land ~ is_general", "Land ~ General"),
  c("has_land ~ is_sc_st", "Has Land ~ SC/ST"),
  c("has_land ~ is_obc", "Has Land ~ OBC"),
  c("is_sc_st ~ log_land", "SC/ST ~ Land"),
  c("is_obc ~ log_land", "OBC ~ Land"),
  c("log_land ~ is_hindu", "Land ~ Hindu"),
  c("log_land ~ is_muslim", "Land ~ Muslim"),
  c("is_sc_st ~ is_hindu", "SC/ST ~ Hindu"),
  c("has_land ~ is_hindu", "Has Land ~ Hindu")
)

results <- data.frame(
  spec = character(),
  beta = numeric(),
  se_hc1 = numeric(),
  se_village = numeric(),
  se_state = numeric(),
  t_hc1 = numeric(),
  t_village = numeric(),
  t_state = numeric(),
  sig_hc1 = logical(),
  sig_village = logical(),
  sig_state = logical(),
  stringsAsFactors = FALSE
)

for (s in specs) {
  formula_str <- s[1]
  label <- s[2]

  vars <- all.vars(as.formula(formula_str))
  data_clean <- df %>% filter(complete.cases(across(all_of(vars))))

  if (nrow(data_clean) < 1000) next

  model <- lm(as.formula(formula_str), data = data_clean)
  beta <- coef(model)[2]

  se_hc1 <- sqrt(diag(vcovHC(model, type = "HC1")))[2]
  se_village <- tryCatch(
    sqrt(diag(vcovCL(model, cluster = data_clean$village_id, type = "HC1")))[2],
    error = function(e) NA
  )
  se_state <- tryCatch(
    sqrt(diag(vcovCL(model, cluster = data_clean$state_id, type = "HC1")))[2],
    error = function(e) NA
  )

  results <- rbind(results, data.frame(
    spec = label,
    beta = beta,
    se_hc1 = se_hc1,
    se_village = se_village,
    se_state = se_state,
    t_hc1 = beta / se_hc1,
    t_village = beta / se_village,
    t_state = beta / se_state,
    sig_hc1 = abs(beta / se_hc1) > 1.96,
    sig_village = abs(beta / se_village) > 1.96,
    sig_state = abs(beta / se_state) > 1.96,
    stringsAsFactors = FALSE
  ))
}

cat(sprintf("\n%-20s %8s %8s %8s %8s %6s %6s %6s\n",
            "Specification", "Beta", "t(HC1)", "t(Vill)", "t(State)", "HC1", "Vill", "State"))
cat(strrep("-", 70), "\n")

for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  cat(sprintf("%-20s %8.4f %8.2f %8.2f %8.2f %6s %6s %6s\n",
              r$spec, r$beta, r$t_hc1, r$t_village, r$t_state,
              ifelse(r$sig_hc1, "*", ""),
              ifelse(r$sig_village, "*", ""),
              ifelse(r$sig_state, "*", "")))
}

cat("\n\n2. WILD CLUSTER BOOTSTRAP FOR BORDERLINE CASES\n")
cat(strrep("-", 70), "\n")

borderline_specs <- list(
  c("log_land ~ is_general", "Land ~ General"),
  c("log_land ~ is_muslim", "Land ~ Muslim"),
  c("has_land ~ is_hindu", "Has Land ~ Hindu"),
  c("is_sc_st ~ is_hindu", "SC/ST ~ Hindu")
)

boot_results <- data.frame(
  spec = character(),
  p_village = numeric(),
  p_state = numeric(),
  stringsAsFactors = FALSE
)

for (s in borderline_specs) {
  formula_str <- s[1]
  label <- s[2]

  vars <- all.vars(as.formula(formula_str))
  data_clean <- df %>% filter(complete.cases(across(all_of(vars))))

  model <- lm(as.formula(formula_str), data = data_clean)
  param <- names(coef(model))[2]

  cat(sprintf("\n%s:\n", label))

  p_village <- NA
  p_state <- NA

  boot_v <- tryCatch({
    boottest(model, clustid = "village_id", param = param, B = 999, type = "webb")
  }, error = function(e) NULL)

  if (!is.null(boot_v)) {
    p_village <- boot_v$p_val
    cat(sprintf("  Village bootstrap: p=%.4f %s\n", p_village, ifelse(p_village < 0.05, "*", "")))
  }

  boot_s <- tryCatch({
    boottest(model, clustid = "state_id", param = param, B = 999, type = "webb")
  }, error = function(e) NULL)

  if (!is.null(boot_s)) {
    p_state <- boot_s$p_val
    cat(sprintf("  State bootstrap:   p=%.4f %s\n", p_state, ifelse(p_state < 0.05, "*", "")))
  }

  boot_results <- rbind(boot_results, data.frame(
    spec = label, p_village = p_village, p_state = p_state
  ))
}

cat("\n\n3. SUMMARY\n")
cat(strrep("-", 70), "\n")

cat(sprintf("\nSignificance counts:\n"))
cat(sprintf("  HC1 (Huber-White):    %d / %d\n", sum(results$sig_hc1), nrow(results)))
cat(sprintf("  Village clustering:   %d / %d\n", sum(results$sig_village), nrow(results)))
cat(sprintf("  State clustering:     %d / %d\n", sum(results$sig_state), nrow(results)))

flips_hc1_to_village <- sum(results$sig_hc1 & !results$sig_village)
flips_village_to_state <- sum(results$sig_village & !results$sig_state)

cat(sprintf("\nFlips:\n"))
cat(sprintf("  HC1 -> Village:  %d specs lose significance\n", flips_hc1_to_village))
cat(sprintf("  Village -> State: %d specs lose significance\n", flips_village_to_state))

cat("\n\nROBUST RESULTS (survive all methods):\n")
robust <- results[results$sig_hc1 & results$sig_village & results$sig_state, ]
for (i in seq_len(nrow(robust))) {
  cat(sprintf("  %s (t=%.1f at state level)\n", robust$spec[i], robust$t_state[i]))
}

cat("\nFRAGILE RESULTS (significant with HC1, not with clustering):\n")
fragile <- results[results$sig_hc1 & (!results$sig_village | !results$sig_state), ]
for (i in seq_len(nrow(fragile))) {
  cat(sprintf("  %s: t=%.1f (HC1) -> t=%.1f (village) -> t=%.1f (state)\n",
              fragile$spec[i], fragile$t_hc1[i], fragile$t_village[i], fragile$t_state[i]))
}

cat("\n\n4. VISUALIZATION\n")
cat(strrep("-", 70), "\n")

results_long <- results %>%
  select(spec, t_hc1, t_village, t_state) %>%
  pivot_longer(cols = starts_with("t_"), names_to = "method", values_to = "t_stat") %>%
  mutate(
    method = factor(method,
                    levels = c("t_hc1", "t_village", "t_state"),
                    labels = c("HC1", "Village", "State")),
    significant = abs(t_stat) > 1.96
  )

p1 <- ggplot(results_long, aes(x = spec, y = abs(t_stat), fill = method)) +
  geom_col(position = "dodge", color = "white", alpha = 0.85) +
  geom_hline(yintercept = 1.96, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("HC1" = "#4a7c9b", "Village" = "#d4a574", "State" = "#d62728")) +
  labs(x = "", y = "|t-statistic|", title = "t-Statistics by Inference Method",
       subtitle = "Red line = significance threshold (1.96)") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom")

se_ratios <- results %>%
  mutate(
    ratio_village = se_village / se_hc1,
    ratio_state = se_state / se_hc1
  ) %>%
  select(spec, ratio_village, ratio_state) %>%
  pivot_longer(cols = starts_with("ratio"), names_to = "comparison", values_to = "ratio") %>%
  mutate(comparison = ifelse(comparison == "ratio_village", "Village/HC1", "State/HC1"))

p2 <- ggplot(se_ratios, aes(x = spec, y = ratio, fill = comparison)) +
  geom_col(position = "dodge", color = "white", alpha = 0.85) +
  scale_fill_manual(values = c("Village/HC1" = "#d4a574", "State/HC1" = "#d62728")) +
  labs(x = "", y = "SE Inflation Ratio", title = "SE Inflation from Clustering") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom")

combined <- p1 / p2 +
  plot_annotation(
    title = "Inference Robustness: HC1 vs Clustered Standard Errors",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave(file.path(OUTPUT_DIR, "inference_robustness.png"), combined, width = 12, height = 14, dpi = 150)
cat(sprintf("\nSaved: %s\n", file.path(OUTPUT_DIR, "inference_robustness.png")))

cat("\n\n5. RECOMMENDATIONS\n")
cat(strrep("-", 70), "\n")
cat("
When to use each method:
- HC1 (Huber-White): Corrects heteroskedasticity only. NOT sufficient for REDS.
- Village clustering: Appropriate for within-village analysis. Use with 193 clusters.
- State clustering: Use when treatment/policy varies at state level OR
                    unobserved state factors affect outcome. Only 13 clusters -
                    use wild bootstrap.

For REDS data:
- Always cluster at minimum village level
- For caste-land relationships: village clustering is appropriate
- For policy effects: consider state clustering with wild bootstrap
- Results that flip under clustering should not be trusted
")
