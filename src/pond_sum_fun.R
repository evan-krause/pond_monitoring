
pond_sum <- function(data, group.by) {
  data |>
    group_by({{group.by}}) |>
    summarize(
      max_depth = max(depth_m),
      #ph_range = round(max(pH) - min(pH), 2),
      mean_temp = mean(temp_c),
      mean_do = mean(do),
      avg_chl_mgl = mean(chla),
      n = n()
    ) |>
    arrange(desc(n))
}
