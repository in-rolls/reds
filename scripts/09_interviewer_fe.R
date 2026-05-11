library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(lme4)
library(broom)

OUTPUT_DIR <- "figs"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("INTERVIEWER FIXED EFFECTS ANALYSIS\n")
cat(strrep("=", 60), "\n\n")

df1 <- read_dta("data/Sepri1/HH/SECTION01.dta") %>% mutate(source = "SEPRI1")
df2 <- read_dta("data/Sepri2/HH/SECTION01.dta") %>% mutate(source = "SEPRI2")

common_cols <- intersect(names(df1), names(df2))
df <- bind_rows(df1[, common_cols], df2[, common_cols])

df <- df %>%
  mutate(
    interviewer_id = as.factor(paste0(source, "_", interviewer_name)),
    village_id = as.factor(paste0(source, "_", state, "_", district, "_", village)),
    state_id = as.factor(paste0(source, "_", state)),
    is_sc_st = as.numeric(q1_8 %in% c(1, 2)),
    is_obc = as.numeric(q1_8 == 3),
    log_land = log1p(q1_10)
  )

cat("Sample sizes:\n")
cat(sprintf("  Total observations: %d\n", nrow(df)))
cat(sprintf("  Unique interviewers: %d\n", n_distinct(df$interviewer_id)))
cat(sprintf("  Unique villages: %d\n", n_distinct(df$village_id)))

cat("\n\n1. VARIANCE DECOMPOSITION (Random Effects)\n")
cat(strrep("-", 50), "\n")

run_variance_decomp <- function(data, outcome_var, outcome_label) {
  data_clean <- data %>%
    filter(!is.na(.data[[outcome_var]])) %>%
    filter(n_distinct(interviewer_id) > 10, n_distinct(village_id) > 10)

  if (nrow(data_clean) < 100) {
    cat(sprintf("\n%s: Insufficient data\n", outcome_label))
    return(NULL)
  }

  formula_full <- as.formula(paste0(outcome_var, " ~ 1 + (1|village_id) + (1|interviewer_id)"))
  formula_village <- as.formula(paste0(outcome_var, " ~ 1 + (1|village_id)"))
  formula_interviewer <- as.formula(paste0(outcome_var, " ~ 1 + (1|interviewer_id)"))

  tryCatch({
    model_full <- lmer(formula_full, data = data_clean, REML = TRUE)
    model_village <- lmer(formula_village, data = data_clean, REML = TRUE)
    model_interviewer <- lmer(formula_interviewer, data = data_clean, REML = TRUE)

    vc_full <- as.data.frame(VarCorr(model_full))

    var_village <- vc_full$vcov[vc_full$grp == "village_id"]
    var_interviewer <- vc_full$vcov[vc_full$grp == "interviewer_id"]
    var_residual <- vc_full$vcov[vc_full$grp == "Residual"]
    var_total <- var_village + var_interviewer + var_residual

    icc_village <- var_village / var_total
    icc_interviewer <- var_interviewer / var_total
    icc_residual <- var_residual / var_total

    cat(sprintf("\n%s:\n", outcome_label))
    cat(sprintf("  N = %d observations\n", nrow(data_clean)))
    cat(sprintf("  Village variance:      %.4f (%.1f%%)\n", var_village, 100 * icc_village))
    cat(sprintf("  Interviewer variance:  %.4f (%.1f%%)\n", var_interviewer, 100 * icc_interviewer))
    cat(sprintf("  Residual variance:     %.4f (%.1f%%)\n", var_residual, 100 * icc_residual))
    cat(sprintf("  ICC (Interviewer):     %.3f\n", icc_interviewer))

    return(list(
      outcome = outcome_label,
      n = nrow(data_clean),
      var_village = var_village,
      var_interviewer = var_interviewer,
      var_residual = var_residual,
      icc_village = icc_village,
      icc_interviewer = icc_interviewer
    ))
  }, error = function(e) {
    cat(sprintf("\n%s: Model failed - %s\n", outcome_label, e$message))
    return(NULL)
  })
}

results <- list()

results$land_s1 <- run_variance_decomp(
  filter(df, source == "SEPRI1" & q1_10 > 0 & q1_10 < 100),
  "log_land", "Log Land (SEPRI1)"
)

results$land_s2 <- run_variance_decomp(
  filter(df, source == "SEPRI2" & q1_10 > 0 & q1_10 < 100),
  "log_land", "Log Land (SEPRI2)"
)

results$scst_s1 <- run_variance_decomp(
  filter(df, source == "SEPRI1" & !is.na(is_sc_st)),
  "is_sc_st", "SC/ST (SEPRI1)"
)

results$scst_s2 <- run_variance_decomp(
  filter(df, source == "SEPRI2" & !is.na(is_sc_st)),
  "is_sc_st", "SC/ST (SEPRI2)"
)

results$obc_s1 <- run_variance_decomp(
  filter(df, source == "SEPRI1" & !is.na(is_obc)),
  "is_obc", "OBC (SEPRI1)"
)

results$obc_s2 <- run_variance_decomp(
  filter(df, source == "SEPRI2" & !is.na(is_obc)),
  "is_obc", "OBC (SEPRI2)"
)

cat("\n\n2. FIXED EFFECTS R² COMPARISON\n")
cat(strrep("-", 50), "\n")

run_fe_comparison <- function(data, outcome_var, outcome_label) {
  data_clean <- data %>%
    filter(!is.na(.data[[outcome_var]])) %>%
    group_by(village_id) %>%
    filter(n() >= 5) %>%
    ungroup() %>%
    group_by(interviewer_id) %>%
    filter(n() >= 5) %>%
    ungroup()

  if (nrow(data_clean) < 100) {
    return(NULL)
  }

  formula_base <- as.formula(paste0(outcome_var, " ~ 1"))
  formula_village <- as.formula(paste0(outcome_var, " ~ village_id"))
  formula_interviewer <- as.formula(paste0(outcome_var, " ~ interviewer_id"))
  formula_both <- as.formula(paste0(outcome_var, " ~ village_id + interviewer_id"))

  model_base <- lm(formula_base, data = data_clean)
  model_village <- lm(formula_village, data = data_clean)
  model_interviewer <- lm(formula_interviewer, data = data_clean)
  model_both <- lm(formula_both, data = data_clean)

  ss_total <- sum((data_clean[[outcome_var]] - mean(data_clean[[outcome_var]]))^2)

  r2_village <- summary(model_village)$r.squared
  r2_interviewer <- summary(model_interviewer)$r.squared
  r2_both <- summary(model_both)$r.squared

  r2_int_given_village <- r2_both - r2_village

  cat(sprintf("\n%s (N=%d):\n", outcome_label, nrow(data_clean)))
  cat(sprintf("  R² (Village FE only):           %.3f\n", r2_village))
  cat(sprintf("  R² (Interviewer FE only):       %.3f\n", r2_interviewer))
  cat(sprintf("  R² (Village + Interviewer FE):  %.3f\n", r2_both))
  cat(sprintf("  R² added by Interviewer|Village: %.3f\n", r2_int_given_village))

  return(list(
    outcome = outcome_label,
    n = nrow(data_clean),
    r2_village = r2_village,
    r2_interviewer = r2_interviewer,
    r2_both = r2_both,
    r2_int_given_village = r2_int_given_village
  ))
}

fe_results <- list()

fe_results$land_s1 <- run_fe_comparison(
  filter(df, source == "SEPRI1" & q1_10 > 0 & q1_10 < 100),
  "log_land", "Log Land (SEPRI1)"
)

fe_results$land_s2 <- run_fe_comparison(
  filter(df, source == "SEPRI2" & q1_10 > 0 & q1_10 < 100),
  "log_land", "Log Land (SEPRI2)"
)

fe_results$scst_s1 <- run_fe_comparison(
  filter(df, source == "SEPRI1" & !is.na(is_sc_st)),
  "is_sc_st", "SC/ST (SEPRI1)"
)

fe_results$scst_s2 <- run_fe_comparison(
  filter(df, source == "SEPRI2" & !is.na(is_sc_st)),
  "is_sc_st", "SC/ST (SEPRI2)"
)

cat("\n\n3. INTERVIEWER-SPECIFIC MEANS\n")
cat(strrep("-", 50), "\n")

interviewer_means <- df %>%
  filter(!is.na(q1_10) & q1_10 > 0 & q1_10 < 100) %>%
  group_by(source, interviewer_id) %>%
  summarise(
    n = n(),
    mean_land = mean(q1_10, na.rm = TRUE),
    sd_land = sd(q1_10, na.rm = TRUE),
    pct_sc_st = 100 * mean(is_sc_st, na.rm = TRUE),
    n_villages = n_distinct(village_id),
    .groups = "drop"
  ) %>%
  filter(n >= 20)

cat(sprintf("\nInterviewers with 20+ observations: %d\n", nrow(interviewer_means)))

cat("\nVariation in interviewer means (land):\n")
interviewer_means %>%
  group_by(source) %>%
  summarise(
    n_interviewers = n(),
    mean_of_means = mean(mean_land),
    sd_of_means = sd(mean_land),
    cv_of_means = sd(mean_land) / mean(mean_land),
    .groups = "drop"
  ) %>%
  as.data.frame() %>%
  print()

cat("\nVariation in interviewer means (SC/ST %):\n")
interviewer_means %>%
  group_by(source) %>%
  summarise(
    n_interviewers = n(),
    mean_of_means = mean(pct_sc_st),
    sd_of_means = sd(pct_sc_st),
    .groups = "drop"
  ) %>%
  as.data.frame() %>%
  print()

cat("\n\n4. VISUALIZATION\n")
cat(strrep("-", 50), "\n")

p1 <- ggplot(interviewer_means, aes(x = mean_land, fill = source)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.85) +
  facet_wrap(~source, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("SEPRI1" = "#4a7c9b", "SEPRI2" = "#d4a574")) +
  labs(x = "Mean Land Ownership (acres)", y = "Number of Interviewers",
       title = "Distribution of Interviewer Mean Land") +
  theme_minimal() +
  theme(legend.position = "none")

p2 <- ggplot(interviewer_means, aes(x = pct_sc_st, fill = source)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.85) +
  facet_wrap(~source, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("SEPRI1" = "#4a7c9b", "SEPRI2" = "#d4a574")) +
  labs(x = "Mean SC/ST %", y = "Number of Interviewers",
       title = "Distribution of Interviewer Mean SC/ST %") +
  theme_minimal() +
  theme(legend.position = "none")

valid_results <- results[!sapply(results, is.null)]
if (length(valid_results) > 0) {
  variance_df <- do.call(rbind, lapply(valid_results, function(x) {
    data.frame(
      outcome = x$outcome,
      component = c("Village", "Interviewer", "Residual"),
      variance_share = c(x$icc_village, x$icc_interviewer, 1 - x$icc_village - x$icc_interviewer)
    )
  }))

  p3 <- ggplot(variance_df, aes(x = outcome, y = variance_share, fill = component)) +
    geom_col(position = "stack", color = "white") +
    scale_fill_manual(values = c("Village" = "#4a7c9b", "Interviewer" = "#d62728", "Residual" = "#cccccc")) +
    labs(x = "", y = "Variance Share", title = "Variance Decomposition",
         fill = "Component") +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom")
} else {
  p3 <- ggplot() + theme_void() + ggtitle("Variance decomposition failed")
}

p4 <- ggplot(interviewer_means, aes(x = n_villages, y = sd_land, color = source)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("SEPRI1" = "#4a7c9b", "SEPRI2" = "#d4a574")) +
  labs(x = "Number of Villages Covered", y = "SD of Land Within Interviewer",
       title = "Within-Interviewer Variance vs Coverage") +
  theme_minimal()

combined <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title = "Interviewer Fixed Effects Analysis",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave(file.path(OUTPUT_DIR, "interviewer_fe.png"), combined, width = 14, height = 12, dpi = 150)
cat(sprintf("\nSaved: %s\n", file.path(OUTPUT_DIR, "interviewer_fe.png")))

cat("\n\n5. SUMMARY\n")
cat(strrep("-", 50), "\n")
cat("\nInterpretation guide:\n")
cat("- ICC (Interviewer) > 0.05: Notable interviewer effect\n")
cat("- ICC (Interviewer) > 0.10: Strong interviewer effect (potential concern)\n")
cat("- R² added by Interviewer|Village > 0.05: Interviewers explain variance beyond geography\n")
