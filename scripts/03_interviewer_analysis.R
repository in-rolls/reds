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
    interview_date = paste0(month_interview, "-", date_interview),
    start_mins = as.integer(hour_start) * 60 + as.integer(minute_start),
    end_mins = as.integer(hour_end) * 60 + as.integer(minute_end),
    duration_mins = end_mins - start_mins,
    start_hour = as.integer(hour_start)
  )

estimate_min_people <- function(group) {
  if (nrow(group) <= 1) return(1)
  time_slots <- group %>%
    group_by(hour_start, minute_start, hour_end, minute_end) %>%
    summarise(n = n(), .groups = "drop")
  return(max(time_slots$n))
}

analyze_interviewers <- function(data, label) {
  cat("\n")
  cat(strrep("=", 60), "\n")
  cat(sprintf("%s: ESTIMATING MIN PEOPLE PER INTERVIEWER NAME\n", label))
  cat(strrep("=", 60), "\n")

  min_people <- data %>%
    group_by(interviewer_name, interview_date) %>%
    group_modify(~ tibble(min_people = estimate_min_people(.x))) %>%
    ungroup()

  min_people_per_name <- min_people %>%
    group_by(interviewer_name) %>%
    summarise(min_people = max(min_people), .groups = "drop")

  cat(sprintf("\nInterviewer names: %d\n", nrow(min_people_per_name)))
  cat(sprintf("Names representing 1 person: %d\n", sum(min_people_per_name$min_people == 1)))
  cat(sprintf("Names representing 2+ people: %d\n", sum(min_people_per_name$min_people >= 2)))
  cat(sprintf("Names representing 4+ people: %d\n", sum(min_people_per_name$min_people >= 4)))

  interviews_per_day_raw <- data %>%
    group_by(interviewer_name, interview_date) %>%
    summarise(n = n(), .groups = "drop")

  cat(sprintf("\nRaw interviews per interviewer-day - Median: %.0f, Mean: %.1f\n",
              median(interviews_per_day_raw$n), mean(interviews_per_day_raw$n)))

  valid_duration <- data %>% filter(duration_mins >= 5 & duration_mins <= 240)
  cat(sprintf("Interview duration (mins) - Median: %.0f, Mean: %.1f\n",
              median(valid_duration$duration_mins), mean(valid_duration$duration_mins)))

  list(
    min_people_per_name = min_people_per_name,
    min_people = min_people,
    interviews_per_day_raw = interviews_per_day_raw,
    data = data
  )
}

results1 <- analyze_interviewers(filter(df, source == "SEPRI1"), "SEPRI1")
results2 <- analyze_interviewers(filter(df, source == "SEPRI2"), "SEPRI2")

make_plots <- function(results, label) {
  min_people_per_name <- results$min_people_per_name
  min_people <- results$min_people
  interviews_per_day_raw <- results$interviews_per_day_raw
  data <- results$data

  adjusted_per_day <- interviews_per_day_raw %>%
    left_join(min_people, by = c("interviewer_name", "interview_date")) %>%
    mutate(adjusted = n / min_people) %>%
    filter(!is.na(adjusted))

  single_person_names <- min_people_per_name %>%
    filter(min_people == 1) %>%
    pull(interviewer_name)

  single_person_daily <- interviews_per_day_raw %>%
    filter(interviewer_name %in% single_person_names)

  p1 <- ggplot(min_people_per_name, aes(x = min_people)) +
    geom_histogram(binwidth = 1, fill = "#4a7c9b", color = "white", alpha = 0.85) +
    scale_x_continuous(breaks = 1:10) +
    labs(x = "Min People per Name", y = "Frequency",
         title = sprintf("%s: Min People per Name (n=%d)", label, nrow(min_people_per_name))) +
    theme_minimal()

  p2 <- ggplot(interviews_per_day_raw, aes(x = n)) +
    geom_histogram(binwidth = 1, fill = "#4a7c9b", color = "white", alpha = 0.85) +
    geom_vline(aes(xintercept = median(n), color = "Median"),
               linetype = "dotted", linewidth = 0.6) +
    scale_color_manual(name = "", values = c("Median" = "#2ca02c")) +
    labs(x = "Interviews per Day (Raw)", y = "Frequency",
         title = sprintf("%s: Raw Interviews/Day", label)) +
    theme_minimal() +
    theme(legend.position = "bottom")

  p3 <- ggplot(data, aes(x = start_hour)) +
    geom_histogram(binwidth = 1, fill = "#4a7c9b", color = "white", alpha = 0.85) +
    scale_x_continuous(breaks = seq(0, 24, 2)) +
    labs(x = "Start Hour", y = "Frequency",
         title = sprintf("%s: Interview Start Times", label)) +
    theme_minimal()

  valid_duration <- data %>% filter(duration_mins >= 5 & duration_mins <= 240)
  p4 <- ggplot(valid_duration, aes(x = duration_mins)) +
    geom_histogram(bins = 40, fill = "#4a7c9b", color = "white", alpha = 0.85) +
    geom_vline(aes(xintercept = median(duration_mins), color = "Median"),
               linetype = "dotted", linewidth = 0.6) +
    scale_color_manual(name = "", values = c("Median" = "#2ca02c")) +
    labs(x = "Duration (minutes)", y = "Frequency",
         title = sprintf("%s: Interview Duration", label)) +
    theme_minimal() +
    theme(legend.position = "bottom")

  p5 <- ggplot(single_person_daily, aes(x = n)) +
    geom_histogram(binwidth = 1, fill = "#4a7c9b", color = "white", alpha = 0.85) +
    geom_vline(aes(xintercept = median(n), color = "Median"),
               linetype = "dotted", linewidth = 0.6) +
    scale_color_manual(name = "", values = c("Median" = "#2ca02c")) +
    labs(x = "Interviews per Day", y = "Frequency",
         title = sprintf("%s: Single-Person Names (n=%d days)", label, nrow(single_person_daily))) +
    theme_minimal() +
    theme(legend.position = "bottom")

  p6 <- ggplot(adjusted_per_day, aes(x = adjusted)) +
    geom_histogram(bins = 30, fill = "#4a7c9b", color = "white", alpha = 0.85) +
    geom_vline(aes(xintercept = median(adjusted), color = "Median"),
               linetype = "dotted", linewidth = 0.6) +
    scale_color_manual(name = "", values = c("Median" = "#2ca02c")) +
    labs(x = "Interviews per Day (Adjusted)", y = "Frequency",
         title = sprintf("%s: Adjusted Interviews/Day", label)) +
    theme_minimal() +
    theme(legend.position = "bottom")

  (p1 | p2 | p3) / (p4 | p5 | p6)
}

plot1 <- make_plots(results1, "SEPRI1")
plot2 <- make_plots(results2, "SEPRI2")

combined <- plot1 / plot2 + plot_annotation(
  title = "Interviewer Analysis: SEPRI1 vs SEPRI2",
  theme = theme(plot.title = element_text(size = 16, face = "bold"))
)

ggsave(file.path(OUTPUT_DIR, "interviewer_time_analysis.png"), combined, width = 16, height = 14, dpi = 150)
cat(sprintf("\nSaved: %s\n", file.path(OUTPUT_DIR, "interviewer_time_analysis.png")))
