library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

OUTPUT_DIR <- "figs"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

df2 <- read_dta("data/Sepri2/HH/SECTION01.dta")

cat("PANEL ATTRITION ANALYSIS (SEPRI2 ONLY)\n")
cat(strrep("=", 60), "\n\n")

cat("Note: SEPRI2 has panel_hh, locked_house, and reason columns\n")
cat("      that SEPRI1 does not have.\n\n")

cat("1. PANEL VS NON-PANEL HOUSEHOLDS\n")
cat(strrep("-", 40), "\n")

if ("panel_hh" %in% names(df2)) {
  panel_dist <- df2 %>%
    count(panel_hh) %>%
    mutate(pct = 100 * n / sum(n))

  cat("\nPanel household distribution:\n")
  print(as.data.frame(panel_dist))

  panel_by_state <- df2 %>%
    group_by(state) %>%
    summarise(
      n = n(),
      n_panel = sum(panel_hh == 1, na.rm = TRUE),
      pct_panel = 100 * n_panel / n,
      .groups = "drop"
    ) %>%
    arrange(desc(pct_panel))

  cat("\nPanel households by state:\n")
  print(as.data.frame(panel_by_state))

  p_panel <- ggplot(panel_by_state, aes(x = factor(state), y = pct_panel)) +
    geom_col(fill = "#4a7c9b", color = "white", alpha = 0.85) +
    geom_text(aes(label = sprintf("%.0f%%", pct_panel)), vjust = -0.5, size = 3) +
    labs(x = "State", y = "% Panel Households",
         title = "Panel Household Rate by State") +
    theme_minimal()
} else {
  cat("panel_hh column not found\n")
  p_panel <- ggplot() + theme_void()
}

cat("\n\n2. LOCKED HOUSES / NON-RESPONSE\n")
cat(strrep("-", 40), "\n")

if ("locked_house" %in% names(df2)) {
  locked_dist <- df2 %>%
    count(locked_house) %>%
    mutate(pct = 100 * n / sum(n))

  cat("\nLocked house distribution:\n")
  print(as.data.frame(locked_dist))

  locked_by_state <- df2 %>%
    group_by(state) %>%
    summarise(
      n = n(),
      n_locked = sum(locked_house == 1, na.rm = TRUE),
      pct_locked = 100 * n_locked / n,
      .groups = "drop"
    ) %>%
    arrange(desc(pct_locked))

  cat("\nLocked houses by state:\n")
  print(as.data.frame(locked_by_state))

  p_locked <- ggplot(locked_by_state, aes(x = factor(state), y = pct_locked)) +
    geom_col(fill = "#d4a574", color = "white", alpha = 0.85) +
    geom_text(aes(label = sprintf("%.1f%%", pct_locked)), vjust = -0.5, size = 3) +
    labs(x = "State", y = "% Locked Houses",
         title = "Locked House Rate by State") +
    theme_minimal()
} else {
  cat("locked_house column not found\n")
  p_locked <- ggplot() + theme_void()
}

cat("\n\n3. REASONS FOR NON-INTERVIEW\n")
cat(strrep("-", 40), "\n")

if ("reason" %in% names(df2)) {
  reason_labels <- attr(df2$reason, "labels")
  if (!is.null(reason_labels)) {
    cat("\nReason value labels:\n")
    print(reason_labels)
  }

  reason_dist <- df2 %>%
    filter(!is.na(reason)) %>%
    count(reason) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    arrange(desc(n))

  cat("\nReason distribution (non-NA only):\n")
  print(as.data.frame(reason_dist))

  p_reason <- ggplot(reason_dist, aes(x = reorder(factor(reason), n), y = n)) +
    geom_col(fill = "#7a9b4a", color = "white", alpha = 0.85) +
    coord_flip() +
    labs(x = "Reason Code", y = "Count",
         title = "Reasons for Non-Interview") +
    theme_minimal()
} else {
  cat("reason column not found\n")
  p_reason <- ggplot() + theme_void()
}

cat("\n\n4. ATTRITION BY CHARACTERISTICS\n")
cat(strrep("-", 40), "\n")

if ("panel_hh" %in% names(df2) && "locked_house" %in% names(df2)) {
  attrition_analysis <- df2 %>%
    filter(panel_hh == 1) %>%
    mutate(
      interviewed = locked_house == 0 | is.na(locked_house),
      not_interviewed = locked_house == 1
    )

  cat(sprintf("\nPanel households: %d\n", nrow(attrition_analysis)))
  cat(sprintf("Interviewed: %d (%.1f%%)\n",
              sum(attrition_analysis$interviewed),
              100 * mean(attrition_analysis$interviewed)))
  cat(sprintf("Not interviewed (locked): %d (%.1f%%)\n",
              sum(attrition_analysis$not_interviewed, na.rm = TRUE),
              100 * mean(attrition_analysis$not_interviewed, na.rm = TRUE)))

  attrition_by_state <- attrition_analysis %>%
    group_by(state) %>%
    summarise(
      n_panel = n(),
      n_interviewed = sum(interviewed),
      pct_interviewed = 100 * n_interviewed / n_panel,
      .groups = "drop"
    ) %>%
    arrange(pct_interviewed)

  cat("\nPanel interview rate by state:\n")
  print(as.data.frame(attrition_by_state))

  p_attrition <- ggplot(attrition_by_state,
                        aes(x = reorder(factor(state), pct_interviewed), y = pct_interviewed)) +
    geom_col(fill = "#9b4a7c", color = "white", alpha = 0.85) +
    geom_text(aes(label = sprintf("%.0f%%", pct_interviewed)), hjust = -0.2, size = 3) +
    coord_flip() +
    ylim(0, 105) +
    labs(x = "State", y = "% Panel HH Interviewed",
         title = "Panel Interview Success Rate by State") +
    theme_minimal()
} else {
  p_attrition <- ggplot() + theme_void()
}

cat("\n\n5. INTERVIEWER PATTERNS IN ATTRITION\n")
cat(strrep("-", 40), "\n")

if ("locked_house" %in% names(df2)) {
  interviewer_attrition <- df2 %>%
    group_by(interviewer_name) %>%
    summarise(
      n = n(),
      n_locked = sum(locked_house == 1, na.rm = TRUE),
      pct_locked = 100 * n_locked / n,
      .groups = "drop"
    ) %>%
    filter(n >= 20)

  cat(sprintf("\nInterviewers with 20+ visits: %d\n", nrow(interviewer_attrition)))
  cat(sprintf("Interviewers with >10%% locked houses: %d\n",
              sum(interviewer_attrition$pct_locked > 10)))

  cat("\nTop 10 interviewers by locked house rate:\n")
  print(head(as.data.frame(arrange(interviewer_attrition, desc(pct_locked))), 10))

  p_int_attrition <- ggplot(interviewer_attrition, aes(x = pct_locked)) +
    geom_histogram(bins = 20, fill = "#4a7c9b", color = "white", alpha = 0.85) +
    geom_vline(aes(xintercept = median(pct_locked), color = "Median"),
               linetype = "dotted", linewidth = 0.6) +
    scale_color_manual(name = "", values = c("Median" = "#2ca02c")) +
    labs(x = "% Locked Houses", y = "Number of Interviewers",
         title = "Locked House Rate Distribution Across Interviewers") +
    theme_minimal() +
    theme(legend.position = "bottom")
} else {
  p_int_attrition <- ggplot() + theme_void()
}

combined <- (p_panel | p_locked) / (p_reason | p_attrition) / p_int_attrition +
  plot_annotation(title = "Panel Attrition Analysis (SEPRI2)",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

ggsave(file.path(OUTPUT_DIR, "panel_attrition.png"), combined, width = 14, height = 14, dpi = 150)
cat(sprintf("\nSaved: %s\n", file.path(OUTPUT_DIR, "panel_attrition.png")))
