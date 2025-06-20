
pond_sum <- function(data, group.by) {
  data |>
    group_by({{group.by}}) |>
    summarize(
      max_depth = max(vpos_m),
      ph_range = round(max(ph) - min(ph), 2),
      avg_temp = mean(temp),
      avg_do_pct = mean(do_pct),
      avg_chl_ugl = mean(chl_ugl),
      n = n()
    ) |>
    arrange(desc(n))
}
