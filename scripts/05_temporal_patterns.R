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

df <- df %>%
  mutate(
    start_mins = as.integer(hour_start) * 60 + as.integer(minute_start),
    end_mins = as.integer(hour_end) * 60 + as.integer(minute_end),
    duration_mins = end_mins - start_mins,
    start_hour = as.integer(hour_start),
    month_day = sprintf("%02d-%02d", as.integer(month_interview), as.integer(date_interview))
  )

cat("TEMPORAL PATTERNS ANALYSIS\n")
cat(strrep("=", 60), "\n\n")

cat("1. FIELDWORK TIMELINE\n")
cat(strrep("-", 40), "\n")

timeline <- df %>%
  count(source, month_interview, date_interview) %>%
  mutate(month_day = sprintf("%02d-%02d", as.integer(month_interview), as.integer(date_interview))) %>%
  arrange(source, month_interview, date_interview)

cat("\nSEPRI1 fieldwork period:\n")
s1 <- filter(timeline, source == "SEPRI1")
cat(sprintf("  First date: month %d, day %d\n", min(s1$month_interview), min(s1$date_interview[s1$month_interview == min(s1$month_interview)])))
cat(sprintf("  Last date: month %d, day %d\n", max(s1$month_interview), max(s1$date_interview[s1$month_interview == max(s1$month_interview)])))
cat(sprintf("  Total fieldwork days: %d\n", nrow(s1)))

cat("\nSEPRI2 fieldwork period:\n")
s2 <- filter(timeline, source == "SEPRI2")
cat(sprintf("  First date: month %d, day %d\n", min(s2$month_interview), min(s2$date_interview[s2$month_interview == min(s2$month_interview)])))
cat(sprintf("  Last date: month %d, day %d\n", max(s2$month_interview), max(s2$date_interview[s2$month_interview == max(s2$month_interview)])))
cat(sprintf("  Total fieldwork days: %d\n", nrow(s2)))

interviews_per_month <- df %>%
  count(source, month_interview) %>%
  arrange(source, month_interview)

p_timeline <- ggplot(interviews_per_month, aes(x = factor(month_interview), y = n, fill = source)) +
  geom_col(position = "dodge", color = "white", alpha = 0.85) +
  scale_fill_manual(values = c("SEPRI1" = "#4a7c9b", "SEPRI2" = "#d4a574")) +
  labs(x = "Month", y = "Number of Interviews", title = "Interviews by Month") +
  theme_minimal() +
  theme(legend.position = "bottom")

cat("\n\n2. INTERVIEWS PER DAY\n")
cat(strrep("-", 40), "\n")

daily_volume <- df %>%
  count(source, month_interview, date_interview, name = "n_interviews")

cat("\nDaily interview volume:\n")
cat("SEPRI1:\n")
print(summary(filter(daily_volume, source == "SEPRI1")$n_interviews))
cat("\nSEPRI2:\n")
print(summary(filter(daily_volume, source == "SEPRI2")$n_interviews))

p_daily <- ggplot(daily_volume, aes(x = n_interviews, fill = source)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.85) +
  facet_wrap(~source, ncol = 1) +
  scale_fill_manual(values = c("SEPRI1" = "#4a7c9b", "SEPRI2" = "#d4a574")) +
  geom_vline(data = daily_volume %>% group_by(source) %>% summarise(m = median(n_interviews)),
             aes(xintercept = m, color = "Median"), linetype = "dotted", linewidth = 0.6) +
  scale_color_manual(name = "", values = c("Median" = "#2ca02c")) +
  labs(x = "Interviews per Day", y = "Frequency", title = "Daily Interview Volume") +
  theme_minimal() +
  theme(legend.position = "bottom")

cat("\n\n3. TIME OF DAY PATTERNS\n")
cat(strrep("-", 40), "\n")

hourly <- df %>%
  filter(!is.na(start_hour) & start_hour >= 0 & start_hour <= 23) %>%
  count(source, start_hour)

cat("\nPeak interview hours:\n")
peak_hours <- hourly %>%
  group_by(source) %>%
  slice_max(n, n = 3) %>%
  arrange(source, desc(n))
print(as.data.frame(peak_hours))

p_hourly <- ggplot(hourly, aes(x = start_hour, y = n, fill = source)) +
  geom_col(color = "white", alpha = 0.85) +
  facet_wrap(~source, ncol = 1) +
  scale_fill_manual(values = c("SEPRI1" = "#4a7c9b", "SEPRI2" = "#d4a574")) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(x = "Start Hour", y = "Number of Interviews", title = "Interview Start Times") +
  theme_minimal() +
  theme(legend.position = "none")

cat("\n\n4. END-OF-DAY RUSHING\n")
cat(strrep("-", 40), "\n")

valid_df <- df %>%
  filter(duration_mins >= 5 & duration_mins <= 240 & !is.na(start_hour))

duration_by_hour <- valid_df %>%
  group_by(source, start_hour) %>%
  summarise(
    n = n(),
    mean_duration = mean(duration_mins),
    median_duration = median(duration_mins),
    .groups = "drop"
  ) %>%
  filter(n >= 50)

cat("\nMean duration by start hour (min 50 interviews):\n")
print(as.data.frame(duration_by_hour %>% select(source, start_hour, n, mean_duration, median_duration)))

p_rushing <- ggplot(duration_by_hour, aes(x = start_hour, y = median_duration, color = source)) +
  geom_line(linewidth = 1) +
  geom_point(aes(size = n), alpha = 0.7) +
  scale_color_manual(values = c("SEPRI1" = "#4a7c9b", "SEPRI2" = "#d4a574")) +
  scale_size_continuous(range = c(2, 6), name = "N Interviews") +
  labs(x = "Start Hour", y = "Median Duration (mins)",
       title = "Interview Duration by Start Hour (End-of-Day Rushing?)") +
  theme_minimal() +
  theme(legend.position = "bottom")

cat("\n\n5. DATE-OF-MONTH PATTERNS\n")
cat(strrep("-", 40), "\n")

day_of_month <- df %>%
  count(source, date_interview) %>%
  filter(!is.na(date_interview))

p_dom <- ggplot(day_of_month, aes(x = date_interview, y = n, fill = source)) +
  geom_col(color = "white", alpha = 0.85) +
  facet_wrap(~source, ncol = 1) +
  scale_fill_manual(values = c("SEPRI1" = "#4a7c9b", "SEPRI2" = "#d4a574")) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30)) +
  labs(x = "Day of Month", y = "Number of Interviews",
       title = "Interviews by Day of Month") +
  theme_minimal() +
  theme(legend.position = "none")

combined <- (p_timeline | p_hourly) / (p_daily | p_rushing) / p_dom +
  plot_annotation(title = "Temporal Patterns Analysis",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

ggsave(file.path(OUTPUT_DIR, "temporal_patterns.png"), combined, width = 14, height = 16, dpi = 150)
cat(sprintf("\nSaved: %s\n", file.path(OUTPUT_DIR, "temporal_patterns.png")))
