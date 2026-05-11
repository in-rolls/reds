library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

OUTPUT_DIR <- "figs"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

df1 <- read_dta("data/Sepri1/HH/SECTION01.dta") %>% mutate(source = "SEPRI1")
df2 <- read_dta("data/Sepri2/HH/SECTION01.dta") %>% mutate(source = "SEPRI2")

common_cols <- intersect(names(df1), names(df2))
df <- bind_rows(df1[, common_cols], df2[, common_cols])

df <- df %>%
  mutate(
    start_mins = as.integer(hour_start) * 60 + as.integer(minute_start),
    end_mins = as.integer(hour_end) * 60 + as.integer(minute_end),
    duration_mins = end_mins - start_mins
  )

cat("INTERVIEWER EFFECTS ANALYSIS\n")
cat(strrep("=", 60), "\n\n")

cat("1. DIGIT PREFERENCE / HEAPING\n")
cat(strrep("-", 40), "\n")

land_data <- df %>%
  filter(!is.na(q1_10) & q1_10 > 0 & q1_10 < 100)

land_data <- land_data %>%
  mutate(
    is_whole = q1_10 == floor(q1_10),
    is_half = (q1_10 * 2) == floor(q1_10 * 2)
  )

cat("\nLand ownership (q1_10) heaping analysis:\n")
cat(sprintf("Total valid responses: %d\n", nrow(land_data)))
cat(sprintf("Whole numbers: %d (%.1f%%) - expected ~10%%\n",
            sum(land_data$is_whole), 100 * mean(land_data$is_whole)))
cat(sprintf("Multiples of 0.5: %d (%.1f%%) - expected ~20%%\n",
            sum(land_data$is_half), 100 * mean(land_data$is_half)))

heaping_summary <- land_data %>%
  group_by(source) %>%
  summarise(
    n = n(),
    pct_whole = 100 * mean(is_whole),
    pct_half = 100 * mean(is_half),
    .groups = "drop"
  )
cat("\nBy source:\n")
print(as.data.frame(heaping_summary))

decimal_part <- land_data %>%
  mutate(decimal = round((q1_10 - floor(q1_10)) * 10) / 10) %>%
  count(decimal) %>%
  mutate(pct = 100 * n / sum(n))

p_digit <- ggplot(decimal_part, aes(x = factor(decimal), y = pct)) +
  geom_col(fill = "#4a7c9b", color = "white", alpha = 0.85) +
  geom_hline(yintercept = 10, linetype = "dotted", color = "#d62728") +
  labs(x = "Decimal Part (rounded to 0.1)", y = "Percentage",
       title = "Land Ownership: Decimal Part Distribution (red = expected 10%)") +
  theme_minimal()

interviewer_heaping <- land_data %>%
  group_by(source, interviewer_name) %>%
  summarise(
    n = n(),
    pct_whole = 100 * mean(is_whole),
    pct_half = 100 * mean(is_half),
    .groups = "drop"
  ) %>%
  filter(n >= 20)

cat(sprintf("\nInterviewers with 20+ land responses: %d\n", nrow(interviewer_heaping)))
cat(sprintf("Interviewers with >60%% whole numbers: %d (%.1f%%)\n",
            sum(interviewer_heaping$pct_whole > 60),
            100 * sum(interviewer_heaping$pct_whole > 60) / nrow(interviewer_heaping)))

p_heaping <- ggplot(interviewer_heaping, aes(x = pct_whole, fill = source)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.85) +
  facet_wrap(~source, ncol = 1) +
  geom_vline(xintercept = 10, linetype = "dotted", color = "#d62728") +
  scale_fill_manual(values = c("SEPRI1" = "#4a7c9b", "SEPRI2" = "#d4a574")) +
  labs(x = "% Whole Number Responses", y = "Number of Interviewers",
       title = "Heaping on Whole Acres by Interviewer (red = expected ~10%)") +
  theme_minimal() +
  theme(legend.position = "none")

cat("\n\n2. RESPONSE DISTRIBUTIONS BY INTERVIEWER\n")
cat(strrep("-", 40), "\n")

caste_by_interviewer <- df %>%
  filter(!is.na(q1_8)) %>%
  group_by(source, interviewer_name) %>%
  summarise(
    n = n(),
    pct_sc_st = 100 * sum(q1_8 %in% c(1, 2), na.rm = TRUE) / n(),
    pct_obc = 100 * sum(q1_8 == 3, na.rm = TRUE) / n(),
    pct_general = 100 * sum(q1_8 == 4, na.rm = TRUE) / n(),
    .groups = "drop"
  ) %>%
  filter(n >= 20)

cat("\nCaste distribution variation across interviewers (n>=20):\n")
caste_summary <- caste_by_interviewer %>%
  group_by(source) %>%
  summarise(
    n_interviewers = n(),
    sc_st_mean = mean(pct_sc_st),
    sc_st_sd = sd(pct_sc_st),
    obc_mean = mean(pct_obc),
    obc_sd = sd(pct_obc),
    .groups = "drop"
  )
print(as.data.frame(caste_summary))

p_caste <- ggplot(caste_by_interviewer, aes(x = pct_sc_st, y = pct_obc, color = source)) +
  geom_point(aes(size = n), alpha = 0.5) +
  scale_color_manual(values = c("SEPRI1" = "#4a7c9b", "SEPRI2" = "#d4a574")) +
  scale_size_continuous(range = c(1, 6), name = "N Interviews") +
  labs(x = "% SC/ST", y = "% OBC",
       title = "Caste Distribution by Interviewer") +
  theme_minimal()

cat("\n\n3. DURATION VS RESPONSE QUALITY\n")
cat(strrep("-", 40), "\n")

response_quality <- df %>%
  filter(duration_mins >= 5 & duration_mins <= 240) %>%
  mutate(duration_cat = cut(duration_mins,
                            breaks = c(5, 30, 60, 90, 120, 180, 240),
                            labels = c("5-30", "30-60", "60-90", "90-120", "120-180", "180-240"))) %>%
  filter(!is.na(duration_cat))

quality_by_duration <- response_quality %>%
  group_by(source, duration_cat) %>%
  summarise(
    n = n(),
    pct_missing_caste = 100 * sum(is.na(q1_7)) / n(),
    pct_missing_religion = 100 * sum(is.na(q1_9)) / n(),
    pct_missing_land = 100 * sum(is.na(q1_10)) / n(),
    .groups = "drop"
  )

cat("\nMissing data by interview duration:\n")
print(as.data.frame(quality_by_duration))

quality_long <- quality_by_duration %>%
  pivot_longer(cols = starts_with("pct_missing"),
               names_to = "variable",
               values_to = "pct_missing") %>%
  mutate(variable = gsub("pct_missing_", "", variable))

p_quality <- ggplot(quality_long, aes(x = duration_cat, y = pct_missing, fill = variable)) +
  geom_col(position = "dodge", color = "white", alpha = 0.85) +
  facet_wrap(~source, ncol = 1) +
  labs(x = "Interview Duration (mins)", y = "% Missing",
       title = "Missing Data by Interview Duration") +
  theme_minimal() +
  theme(legend.position = "bottom")

cat("\n\n4. INTERVIEWER PRODUCTIVITY VS QUALITY\n")
cat(strrep("-", 40), "\n")

interviewer_stats <- df %>%
  filter(duration_mins >= 5 & duration_mins <= 240) %>%
  group_by(source, interviewer_name) %>%
  summarise(
    n_interviews = n(),
    avg_duration = mean(duration_mins),
    pct_missing_any = 100 * sum(is.na(q1_7) | is.na(q1_9) | is.na(q1_10)) / n(),
    .groups = "drop"
  ) %>%
  filter(n_interviews >= 20)

cat("\nCorrelation between avg duration and missing data:\n")
for (src in c("SEPRI1", "SEPRI2")) {
  sub <- filter(interviewer_stats, source == src)
  cor_val <- cor(sub$avg_duration, sub$pct_missing_any, use = "complete.obs")
  cat(sprintf("  %s: r = %.3f\n", src, cor_val))
}

p_prod <- ggplot(interviewer_stats, aes(x = avg_duration, y = pct_missing_any, color = source)) +
  geom_point(aes(size = n_interviews), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  scale_color_manual(values = c("SEPRI1" = "#4a7c9b", "SEPRI2" = "#d4a574")) +
  scale_size_continuous(range = c(1, 6), name = "N Interviews") +
  labs(x = "Average Duration (mins)", y = "% Missing Any Key Variable",
       title = "Interview Duration vs Data Quality") +
  theme_minimal()

combined <- (p_digit | p_heaping) / (p_caste | p_quality) / p_prod +
  plot_annotation(title = "Interviewer Effects Analysis",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

ggsave(file.path(OUTPUT_DIR, "interviewer_effects.png"), combined, width = 14, height = 16, dpi = 150)
cat(sprintf("\nSaved: %s\n", file.path(OUTPUT_DIR, "interviewer_effects.png")))
