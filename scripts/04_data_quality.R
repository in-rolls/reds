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

cat("DATA QUALITY ANALYSIS\n")
cat(strrep("=", 60), "\n\n")

cat("1. MISSING DATA ANALYSIS\n")
cat(strrep("-", 40), "\n")

missing_by_var <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = 100 * n_missing / nrow(df)) %>%
  filter(n_missing > 0) %>%
  arrange(desc(pct_missing))

cat(sprintf("Variables with missing data: %d of %d\n", nrow(missing_by_var), ncol(df)))
cat("\nTop 20 variables by missing %:\n")
print(head(as.data.frame(missing_by_var), 20))

missing_by_source <- df %>%
  group_by(source) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(-source, names_to = "variable", values_to = "n_missing")

missing_wide <- missing_by_source %>%
  pivot_wider(names_from = source, values_from = n_missing) %>%
  filter(SEPRI1 > 0 | SEPRI2 > 0)

key_vars <- c("interviewer_name", "supervisor_name", "date_interview", "month_interview",
              "hour_start", "minute_start", "hour_end", "minute_end",
              "q1_7", "q1_8", "q1_9", "q1_10")

missing_heatmap <- df %>%
  select(source, state, all_of(key_vars[key_vars %in% names(df)])) %>%
  group_by(source, state) %>%
  summarise(across(everything(), ~100 * sum(is.na(.)) / n()), .groups = "drop") %>%
  pivot_longer(-c(source, state), names_to = "variable", values_to = "pct_missing")

p_missing <- ggplot(missing_heatmap, aes(x = variable, y = factor(state), fill = pct_missing)) +
  geom_tile(color = "white") +
  facet_wrap(~source, ncol = 1) +
  scale_fill_gradient(low = "white", high = "#d62728", name = "% Missing") +
  labs(x = "", y = "State", title = "Missing Data by State and Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("\n\n2. IMPOSSIBLE VALUES\n")
cat(strrep("-", 40), "\n")

df <- df %>%
  mutate(
    start_mins = as.integer(hour_start) * 60 + as.integer(minute_start),
    end_mins = as.integer(hour_end) * 60 + as.integer(minute_end),
    duration_mins = end_mins - start_mins
  )

impossible_duration <- df %>%
  filter(duration_mins < 5 | duration_mins > 240)

cat(sprintf("Interviews with impossible duration (<5min or >4hrs): %d (%.2f%%)\n",
            nrow(impossible_duration), 100 * nrow(impossible_duration) / nrow(df)))

impossible_by_source <- impossible_duration %>%
  count(source) %>%
  mutate(pct = n / nrow(df) * 100)
print(as.data.frame(impossible_by_source))

impossible_times <- df %>%
  filter(hour_start < 0 | hour_start > 23 | hour_end < 0 | hour_end > 23 |
         minute_start < 0 | minute_start > 59 | minute_end < 0 | minute_end > 59)
cat(sprintf("\nInterviews with invalid time values: %d\n", nrow(impossible_times)))

impossible_dates <- df %>%
  filter(date_interview < 1 | date_interview > 31 | month_interview < 1 | month_interview > 12)
cat(sprintf("Interviews with invalid date values: %d\n", nrow(impossible_dates)))

p_duration <- ggplot(df %>% filter(duration_mins > -60 & duration_mins < 600),
                     aes(x = duration_mins, fill = source)) +
  geom_histogram(bins = 50, color = "white", alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 0, color = "red", linetype = "dotted") +
  facet_wrap(~source, ncol = 1) +
  labs(x = "Duration (minutes)", y = "Frequency",
       title = "Interview Duration Distribution (red line = 0)") +
  theme_minimal() +
  theme(legend.position = "none")

duration_binned <- df %>%
  filter(!is.na(duration_mins)) %>%
  mutate(duration_bin = cut(duration_mins,
                            breaks = c(-Inf, 5, 30, 60, 90, 120, 180, 240, Inf),
                            labels = c("<5", "5-30", "30-60", "60-90", "90-120", "120-180", "180-240", ">240"),
                            right = FALSE)) %>%
  count(source, duration_bin) %>%
  group_by(source) %>%
  mutate(pct = 100 * n / sum(n),
         suspicious = duration_bin %in% c("<5", ">240"))

p_duration_binned <- ggplot(duration_binned, aes(x = duration_bin, y = pct, fill = suspicious)) +
  geom_col(color = "white", alpha = 0.85) +
  facet_wrap(~source, ncol = 1) +
  scale_fill_manual(values = c("FALSE" = "#4a7c9b", "TRUE" = "#d62728"), guide = "none") +
  labs(x = "Duration (minutes)", y = "Percentage",
       title = "Duration Distribution by Bin (red = suspicious)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("\n\n3. DUPLICATE DETECTION\n")
cat(strrep("-", 40), "\n")

dup_by_id <- df %>%
  filter(!is.na(q1_1)) %>%
  group_by(source, state, district, village, q1_1) %>%
  filter(n() > 1) %>%
  ungroup()

cat(sprintf("Potential duplicate records (same HH ID within village): %d\n", nrow(dup_by_id)))

dup_summary <- dup_by_id %>%
  count(source, state) %>%
  arrange(desc(n))

if (nrow(dup_summary) > 0) {
  cat("\nDuplicates by source and state:\n")
  print(as.data.frame(dup_summary))
}

exact_dups <- df %>%
  group_by(across(c(source, state, district, village, q1_1,
                    date_interview, month_interview, hour_start, minute_start))) %>%
  filter(n() > 1) %>%
  ungroup()

cat(sprintf("\nExact duplicates (same ID + same interview time): %d\n", nrow(exact_dups)))

cat("\n\n4. INTERVIEWER DATA QUALITY\n")
cat(strrep("-", 40), "\n")

interviewer_quality <- df %>%
  group_by(source, interviewer_name) %>%
  summarise(
    n_interviews = n(),
    n_states = n_distinct(state),
    pct_missing_land = 100 * sum(is.na(q1_10)) / n(),
    pct_impossible_duration = 100 * sum(duration_mins < 5 | duration_mins > 240, na.rm = TRUE) / n(),
    avg_duration = mean(duration_mins[duration_mins >= 5 & duration_mins <= 240], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_interviews >= 10)

cat(sprintf("Interviewers with 10+ interviews: %d\n", nrow(interviewer_quality)))

problematic <- interviewer_quality %>%
  filter(pct_impossible_duration > 5 | pct_missing_land > 20)

cat(sprintf("Interviewers with >5%% impossible durations OR >20%% missing land: %d\n", nrow(problematic)))

if (nrow(problematic) > 0) {
  cat("\nTop problematic interviewers:\n")
  print(head(as.data.frame(arrange(problematic, desc(pct_impossible_duration))), 10))
}

p_interviewer <- ggplot(interviewer_quality, aes(x = pct_impossible_duration, y = pct_missing_land)) +
  geom_point(aes(size = n_interviews, color = source), alpha = 0.5) +
  geom_hline(yintercept = 20, linetype = "dotted", color = "red") +
  geom_vline(xintercept = 5, linetype = "dotted", color = "red") +
  scale_size_continuous(range = c(1, 8), name = "N Interviews") +
  labs(x = "% Impossible Duration", y = "% Missing Land Ownership",
       title = "Interviewer Data Quality (red lines = thresholds)") +
  theme_minimal()

combined <- (p_missing | p_duration | p_duration_binned) / p_interviewer +
  plot_annotation(title = "Data Quality Analysis",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

ggsave(file.path(OUTPUT_DIR, "data_quality.png"), combined, width = 14, height = 12, dpi = 150)
cat(sprintf("\nSaved: %s\n", file.path(OUTPUT_DIR, "data_quality.png")))
