reds <- read.csv("data/reds/reds06/clean_reds.csv")

obs_per_village <- as.data.frame(table(reds$village_name))
names(obs_per_village) <- c("village_name", "n_obs")
obs_per_village <- obs_per_village[order(-obs_per_village$n_obs), ]

cat("Observations per Village Summary\n")
cat("================================\n")
cat("Total observations:", nrow(reds), "\n")
cat("Unique villages:", nrow(obs_per_village), "\n")
cat("Min obs per village:", min(obs_per_village$n_obs), "\n")
cat("Max obs per village:", max(obs_per_village$n_obs), "\n")
cat("Mean obs per village:", round(mean(obs_per_village$n_obs), 1), "\n")
cat("Median obs per village:", median(obs_per_village$n_obs), "\n\n")

cat("Village-level counts:\n")
print(obs_per_village, row.names = FALSE)

png("figs/obs_per_village_hist.png", width = 800, height = 600)
hist(obs_per_village$n_obs,
     breaks = 10,
     col = "steelblue",
     border = "white",
     main = paste0("Distribution of Observations per Village (N = ", nrow(obs_per_village), " villages)"),
     xlab = "Number of Observations",
     ylab = "Count of Villages")
dev.off()

cat("\nHistogram saved to figs/obs_per_village_hist.png\n")
