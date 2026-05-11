library(haven)
library(dplyr)
library(tidyr)
library(stringdist)
library(ggplot2)
library(patchwork)

OUTPUT_DIR <- "figs"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("REDS-SHRUG MERGE AND VALIDATION\n")
cat(strrep("=", 60), "\n\n")

cat("1. LOADING REDS VILLAGE DATA\n")
cat(strrep("-", 40), "\n")

reds_vill <- read_dta("data/Sepri2/Village/SECTION_02_NOW.dta") %>%
  select(village_sr_no, village_name, gram_panchayat, tehsil_taluka, district) %>%
  distinct() %>%
  mutate(
    village_name_clean = toupper(trimws(village_name)),
    district_clean = toupper(trimws(as.character(district)))
  )

cat(sprintf("REDS villages: %d\n", nrow(reds_vill)))
cat("\nSample REDS village names:\n")
print(head(reds_vill$village_name_clean, 10))

cat("\n\n2. LOADING LGD CROSSWALK DATA\n")
cat(strrep("-", 40), "\n")

lgd_up <- read.csv("../quota/data/lgd/up_village_gp_mapping_2024.csv") %>%
  select(
    lgd_district_name = District.Name,
    lgd_subdistrict_name = Subdistrict.Name,
    lgd_village_name = Village.Name,
    lgd_village_code = Village.Code,
    census_2011_code = Village.Census.2011.Code,
    lgd_gp_name = Local.Body.Name
  ) %>%
  mutate(
    state = "UP",
    village_name_clean = toupper(trimws(lgd_village_name)),
    district_clean = toupper(trimws(lgd_district_name))
  )

lgd_raj <- read.csv("../quota/data/lgd/raj_village_gp_mapping_2024.csv") %>%
  select(
    lgd_district_name = District.Name,
    lgd_subdistrict_name = Subdistrict.Name,
    lgd_village_name = Village.Name,
    lgd_village_code = Village.Code,
    census_2011_code = Village.Census.2011.Code,
    lgd_gp_name = Local.Body.Name
  ) %>%
  mutate(
    state = "Rajasthan",
    village_name_clean = toupper(trimws(lgd_village_name)),
    district_clean = toupper(trimws(lgd_district_name))
  )

lgd <- bind_rows(lgd_up, lgd_raj)
cat(sprintf("LGD villages - UP: %d, Rajasthan: %d, Total: %d\n",
            nrow(lgd_up), nrow(lgd_raj), nrow(lgd)))

cat("\n\n3. FUZZY MATCHING REDS TO LGD\n")
cat(strrep("-", 40), "\n")

fuzzy_match_village <- function(reds_name, lgd_names, max_dist = 2) {
  if (is.na(reds_name) || reds_name == "") return(NA_integer_)

  distances <- stringdist(reds_name, lgd_names, method = "lv")
  min_dist <- min(distances)

  if (min_dist <= max_dist) {
    return(which.min(distances))
  }
  return(NA_integer_)
}

reds_vill$lgd_match_idx <- NA_integer_
reds_vill$match_distance <- NA_integer_

cat("Matching REDS villages to LGD (this may take a moment)...\n")

for (i in seq_len(nrow(reds_vill))) {
  reds_name <- reds_vill$village_name_clean[i]

  distances <- stringdist(reds_name, lgd$village_name_clean, method = "lv")
  min_dist <- min(distances)

  if (min_dist <= 3) {
    reds_vill$lgd_match_idx[i] <- which.min(distances)
    reds_vill$match_distance[i] <- min_dist
  }
}

reds_matched <- reds_vill %>%
  filter(!is.na(lgd_match_idx)) %>%
  mutate(
    lgd_village_name = lgd$lgd_village_name[lgd_match_idx],
    lgd_district_name = lgd$lgd_district_name[lgd_match_idx],
    census_2011_code = lgd$census_2011_code[lgd_match_idx],
    lgd_state = lgd$state[lgd_match_idx]
  )

cat(sprintf("\nMatched: %d of %d REDS villages (%.1f%%)\n",
            nrow(reds_matched), nrow(reds_vill),
            100 * nrow(reds_matched) / nrow(reds_vill)))

cat("\nMatch quality:\n")
print(table(reds_matched$match_distance))

cat("\nSample matches:\n")
sample_matches <- reds_matched %>%
  select(village_name, lgd_village_name, lgd_district_name, match_distance) %>%
  head(15)
print(as.data.frame(sample_matches))

cat("\n\n4. LOADING SHRUG DATA\n")
cat(strrep("-", 40), "\n")

shrug_pca <- read.csv("../quota/data/shrug/shrug-pca11-csv/pc11_pca_clean_shrid.csv") %>%
  select(
    shrid2,
    shrug_pop = pc11_pca_tot_p,
    shrug_hh = pc11_pca_no_hh,
    shrug_sc = pc11_pca_p_sc,
    shrug_st = pc11_pca_p_st,
    shrug_lit = pc11_pca_p_lit,
    shrug_workers = pc11_pca_tot_work_p
  ) %>%
  mutate(
    shrug_sc_st_pct = 100 * (shrug_sc + shrug_st) / shrug_pop,
    shrug_lit_rate = 100 * shrug_lit / shrug_pop,
    shrug_worker_rate = 100 * shrug_workers / shrug_pop
  )

cat(sprintf("SHRUG villages loaded: %d\n", nrow(shrug_pca)))

shrug_keys <- read.csv("../quota/data/shrug/shrug-pc-keys-csv/pc11r_shrid_key.csv") %>%
  select(shrid2, pc11_state_id, pc11_district_id, pc11_subdistrict_id, pc11_village_id)

cat(sprintf("SHRUG keys loaded: %d\n", nrow(shrug_keys)))

cat("\n\n5. MATCHING TO SHRUG VIA VILLAGE ID\n")
cat(strrep("-", 40), "\n")

reds_matched <- reds_matched %>%
  mutate(census_2011_code = as.numeric(census_2011_code))

cat("Sample census codes from matched data:\n")
print(head(reds_matched$census_2011_code, 10))

shrug_keys <- shrug_keys %>%
  mutate(pc11_village_id = as.numeric(pc11_village_id))

cat("\nSample SHRUG village IDs:\n")
print(head(shrug_keys$pc11_village_id[shrug_keys$pc11_state_id == 9], 10))

reds_shrug <- reds_matched %>%
  left_join(
    shrug_keys %>% select(shrid2, pc11_village_id, pc11_state_id),
    by = c("census_2011_code" = "pc11_village_id")
  ) %>%
  filter(!is.na(shrid2)) %>%
  distinct(village_name, .keep_all = TRUE)

cat(sprintf("REDS villages matched to SHRUG: %d\n", nrow(reds_shrug)))

reds_shrug <- reds_shrug %>%
  left_join(shrug_pca, by = "shrid2")

cat(sprintf("With PCA data: %d\n", sum(!is.na(reds_shrug$shrug_pop))))

cat("\n\n6. LOADING REDS HH DATA FOR VALIDATION\n")
cat(strrep("-", 40), "\n")

reds_hh <- read_dta("data/Sepri2/HH/SECTION01.dta") %>%
  mutate(village_sr_no = village) %>%
  group_by(village_sr_no) %>%
  summarise(
    reds_n_obs = n(),
    reds_sc_st_pct = 100 * sum(q1_8 %in% c(1, 2), na.rm = TRUE) / sum(!is.na(q1_8)),
    reds_avg_land = mean(q1_10, na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("REDS HH aggregates: %d villages\n", nrow(reds_hh)))

validation <- reds_shrug %>%
  inner_join(reds_hh, by = "village_sr_no") %>%
  filter(!is.na(shrug_pop) & !is.na(reds_n_obs))

cat(sprintf("Villages for validation: %d\n", nrow(validation)))

cat("\n\n7. VALIDATION ANALYSIS\n")
cat(strrep("-", 40), "\n")

if (nrow(validation) > 5) {
  cat("\nCorrelations:\n")

  cor_pop <- cor(validation$reds_n_obs, validation$shrug_hh, use = "complete.obs")
  cat(sprintf("  REDS sample size vs SHRUG households: r = %.3f\n", cor_pop))

  cor_caste <- cor(validation$reds_sc_st_pct, validation$shrug_sc_st_pct, use = "complete.obs")
  cat(sprintf("  REDS SC/ST %% vs SHRUG SC/ST %%: r = %.3f\n", cor_caste))

  cat("\nSummary of matched villages:\n")
  cat(sprintf("  SHRUG population: median %.0f, mean %.0f\n",
              median(validation$shrug_pop, na.rm = TRUE),
              mean(validation$shrug_pop, na.rm = TRUE)))
  cat(sprintf("  SHRUG SC/ST %%: median %.1f, mean %.1f\n",
              median(validation$shrug_sc_st_pct, na.rm = TRUE),
              mean(validation$shrug_sc_st_pct, na.rm = TRUE)))
  cat(sprintf("  REDS SC/ST %%: median %.1f, mean %.1f\n",
              median(validation$reds_sc_st_pct, na.rm = TRUE),
              mean(validation$reds_sc_st_pct, na.rm = TRUE)))

  p1 <- ggplot(validation, aes(x = shrug_hh, y = reds_n_obs)) +
    geom_point(alpha = 0.6, color = "#4a7c9b") +
    geom_smooth(method = "lm", se = TRUE, color = "#d62728", linetype = "dashed") +
    labs(
      x = "SHRUG Households (Census 2011)",
      y = "REDS Sample Size",
      title = sprintf("Sample Size vs Village Size (r=%.2f)", cor_pop)
    ) +
    theme_minimal()

  p2 <- ggplot(validation, aes(x = shrug_sc_st_pct, y = reds_sc_st_pct)) +
    geom_point(alpha = 0.6, color = "#4a7c9b") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50") +
    geom_smooth(method = "lm", se = TRUE, color = "#d62728", linetype = "dashed") +
    labs(
      x = "SHRUG SC/ST % (Census 2011)",
      y = "REDS SC/ST %",
      title = sprintf("Caste Composition Validation (r=%.2f)", cor_caste)
    ) +
    theme_minimal()

  p3 <- ggplot(validation, aes(x = shrug_pop)) +
    geom_histogram(bins = 30, fill = "#4a7c9b", color = "white", alpha = 0.85) +
    labs(
      x = "Village Population (Census 2011)",
      y = "Frequency",
      title = "Population Distribution of Matched Villages"
    ) +
    theme_minimal()

  p4 <- ggplot(validation, aes(x = shrug_lit_rate)) +
    geom_histogram(bins = 30, fill = "#d4a574", color = "white", alpha = 0.85) +
    labs(
      x = "Literacy Rate % (Census 2011)",
      y = "Frequency",
      title = "Literacy Rate of Matched Villages"
    ) +
    theme_minimal()

  combined <- (p1 | p2) / (p3 | p4) +
    plot_annotation(
      title = "REDS-SHRUG Validation",
      subtitle = sprintf("N = %d matched villages", nrow(validation)),
      theme = theme(plot.title = element_text(size = 16, face = "bold"))
    )

  ggsave(file.path(OUTPUT_DIR, "shrug_validation.png"), combined, width = 12, height = 10, dpi = 150)
  cat(sprintf("\nSaved: %s\n", file.path(OUTPUT_DIR, "shrug_validation.png")))

} else {
  cat("\nInsufficient matches for validation plots.\n")
}

cat("\n\n8. EXPORT MATCHED DATA\n")
cat(strrep("-", 40), "\n")

export_data <- validation %>%
  select(
    village_name, lgd_village_name, lgd_district_name, lgd_state,
    match_distance, shrid2, census_2011_code,
    reds_n_obs, reds_sc_st_pct, reds_avg_land,
    shrug_pop, shrug_hh, shrug_sc_st_pct, shrug_lit_rate, shrug_worker_rate
  )

write.csv(export_data, "data/reds_shrug_matched.csv", row.names = FALSE)
cat(sprintf("Exported matched data: data/reds_shrug_matched.csv (%d rows)\n", nrow(export_data)))

cat("\n\nDONE\n")
