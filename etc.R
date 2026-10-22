df <- data.frame(
  x = c(1, 3, 5, 2, 4, 6, 1, 3, 5, 2, 1, 4),
  y = c(1.4, 2, 3.2, 4, 5, 6, 8, 7, 9, 10, 13, 12),
  z = c(10, 20, 30, 15, 25, 35, 12, 22, 32, 43, 55, 60)
)

grid <- akima::interp(df$x + rnorm(nrow(df), 0, 1e-8),
               df$y + rnorm(nrow(df), 0, 1e-8),
               df$z, nx = 100, ny = 100, linear = FALSE)
grid_df <- akima::interp2xyz(grid, data.frame = TRUE)  # data.frame with columns x, y, z

ggplot(grid_df, aes(x, y)) +
  stat_contour(aes(z = z), colour = "blue") +
  geom_point(data = df, aes(x, y), colour = "red", size = 2) +
  coord_equal()


lp <- readRDS("data/lp_cleaned.rds")

date_means <- lp |>
  filter(station %in% c("lp2", "lp3"), year == 2026) |>
  group_by(station, date) |>
  summarize(mean_do = mean(do, na.rm = TRUE), .groups = "drop")

date_means |>
  group_by(station) |>
  summarize(
    n = n(),
    avg = mean(mean_do),
    sd_do = sd(mean_do),
    se = sd_do / sqrt(n),
    ci_low = avg - qt(0.975, n - 1) * se,
    ci_high = avg + qt(0.975, n - 1) * se
  )

