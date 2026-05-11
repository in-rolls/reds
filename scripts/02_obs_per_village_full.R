library(haven)
library(dplyr)
library(ggplot2)
library(patchwork)

OUTPUT_DIR <- "figs"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

df1 <- read_dta("data/Sepri1/HH/SECTION01.dta") %>% mutate(source = "SEPRI1")
df2 <- read_dta("data/Sepri2/HH/SECTION01.dta") %>% mutate(source = "SEPRI2")

common_cols <- intersect(names(df1), names(df2))
df <- bind_rows(df1[, common_cols], df2[, common_cols])

cat("COMBINED SEPRI1 + SEPRI2\n")
cat(strrep("=", 60), "\n")
cat(sprintf("Total rows: %s\n", format(nrow(df), big.mark = ",")))
cat(sprintf("  SEPRI1: %s\n", format(nrow(df1), big.mark = ",")))
cat(sprintf("  SEPRI2: %s\n", format(nrow(df2), big.mark = ",")))
cat(sprintf("Total states: %d\n", n_distinct(df$state)))
cat(sprintf("Total districts: %d\n", n_distinct(df$district)))
cat(sprintf("Total villages: %d\n", n_distinct(df$village)))

cat("\n")
cat(strrep("=", 60), "\n")
cat("SUMMARY BY STATE (COMBINED)\n")
cat(strrep("=", 60), "\n")

state_summary <- df %>%
  group_by(source, state) %>%
  summarise(
    villages = n_distinct(village),
    districts = n_distinct(district),
    observations = n(),
    .groups = "drop"
  ) %>%
  arrange(source, desc(observations))

print(as.data.frame(state_summary))

cat(sprintf(
  "\nTotal: %s observations, %d villages, %d districts\n",
  format(sum(state_summary$observations), big.mark = ","),
  sum(state_summary$villages),
  sum(state_summary$districts)
))

obs_per_village <- df %>%
  group_by(source, village) %>%
  summarise(n = n(), .groups = "drop")

cat("\n")
cat(strrep("=", 60), "\n")
cat("OBSERVATIONS PER VILLAGE\n")
cat(strrep("=", 60), "\n")
cat("\nSEPRI1:\n")
print(summary(filter(obs_per_village, source == "SEPRI1")$n))
cat("\nSEPRI2:\n")
print(summary(filter(obs_per_village, source == "SEPRI2")$n))

stats_village <- obs_per_village %>%
  group_by(source) %>%
  summarise(mean_val = mean(n), median_val = median(n), .groups = "drop")

p1 <- ggplot(obs_per_village, aes(x = n)) +
  geom_histogram(bins = 30, fill = "#4a7c9b", color = "white", alpha = 0.85) +
  facet_wrap(~source, ncol = 1) +
  geom_vline(data = stats_village, aes(xintercept = mean_val, color = "Mean"),
             linetype = "dotted", linewidth = 0.6) +
  geom_vline(data = stats_village, aes(xintercept = median_val, color = "Median"),
             linetype = "dotted", linewidth = 0.6) +
  scale_color_manual(name = "", values = c("Mean" = "#d62728", "Median" = "#2ca02c")) +
  labs(
    x = "Observations per Village",
    y = "Frequency",
    title = sprintf("Distribution of Observations per Village (n=%d villages)", nrow(obs_per_village))
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "obs_per_village_full_hist.png"), p1, width = 10, height = 8, dpi = 150)
cat(sprintf("\nSaved: %s\n", file.path(OUTPUT_DIR, "obs_per_village_full_hist.png")))

if ("panchayat" %in% names(df)) {
  obs_per_panchayat <- df %>%
    group_by(source, state, district, panchayat) %>%
    summarise(n = n(), .groups = "drop")

  cat("\n")
  cat(strrep("=", 60), "\n")
  cat("OBSERVATIONS PER PANCHAYAT (state+district+panchayat)\n")
  cat(strrep("=", 60), "\n")
  cat("\nSEPRI1:\n")
  print(summary(filter(obs_per_panchayat, source == "SEPRI1")$n))
  cat("\nSEPRI2:\n")
  print(summary(filter(obs_per_panchayat, source == "SEPRI2")$n))

  stats_panchayat <- obs_per_panchayat %>%
    group_by(source) %>%
    summarise(mean_val = mean(n), median_val = median(n), .groups = "drop")

  p2 <- ggplot(obs_per_panchayat, aes(x = n)) +
    geom_histogram(bins = 30, fill = "#4a7c9b", color = "white", alpha = 0.85) +
    facet_wrap(~source, ncol = 1) +
    geom_vline(data = stats_panchayat, aes(xintercept = mean_val, color = "Mean"),
               linetype = "dotted", linewidth = 0.6) +
    geom_vline(data = stats_panchayat, aes(xintercept = median_val, color = "Median"),
               linetype = "dotted", linewidth = 0.6) +
    scale_color_manual(name = "", values = c("Mean" = "#d62728", "Median" = "#2ca02c")) +
    labs(
      x = "Observations per Panchayat",
      y = "Frequency",
      title = sprintf("Distribution of Observations per Panchayat (n=%d panchayats)", nrow(obs_per_panchayat))
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggsave(file.path(OUTPUT_DIR, "obs_per_panchayat_full_hist.png"), p2, width = 10, height = 8, dpi = 150)
  cat(sprintf("Saved: %s\n", file.path(OUTPUT_DIR, "obs_per_panchayat_full_hist.png")))
}
