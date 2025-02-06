km_table <- function(data, times, formula, time_units = 1) {
  # Perform KM fit
  km_fit <- survfit(formula = formula, data = data)

  # Create the initial KM table
  km_summary <- summary(km_fit)
  km_table <- data.frame(km_summary$table) |>
    rownames_to_column() |>
    select(rowname, records, events, median, X0.95LCL, X0.95UCL) |>
    rename(
      Group = rowname,
      N = records,
      Events = events,
      Median = median,
      LowerBound = X0.95LCL,
      UpperBound = X0.95UCL
    ) |>
    mutate(`Median Survival` = paste0(
      format(Median / time_units, digits = 2),
      " (",
      format(LowerBound / time_units, digits = 2),
      ", ",
      format(UpperBound / time_units, digits = 2),
      ")"
    )) |>
    select(Group, N, Events, `Median Survival`)

  # Reverse KM fit (if applicable)
  rev_km <- survfit(
    formula = update(formula, . ~ .),
    data = data |> mutate(across(where(is.numeric), ~ . / time_units))
  )

  reverse_km_table <- summary(rev_km)$table |>
    data.frame() |>
    rownames_to_column() |>
    select(rowname, records, events, median, X0.95LCL, X0.95UCL) |>
    rename(
      Group = rowname,
      N = records,
      Events = events,
      Median = median,
      LowerBound = X0.95LCL,
      UpperBound = X0.95UCL
    ) |>
    mutate(`Median Follow-Up` = paste0(
      format(Median / time_units, digits = 2),
      " (",
      format(LowerBound / time_units, digits = 2),
      ", ",
      format(UpperBound / time_units, digits = 2),
      ")"
    )) |>
    select(`Median Follow-Up`)

  # Add reverse KM table to main table
  km_table <- cbind(km_table, reverse_km_table)

  # Perform log-rank test
  km_diff <- survdiff(formula = formula, data = data)
  log_rank_p <- pchisq(km_diff$chisq, length(km_diff$n) - 1, lower.tail = FALSE)

  # Format p-value
  log_rank_p <- case_when(
    log_rank_p < 0.001 ~ "<0.001",
    log_rank_p < 0.01 ~ format(log_rank_p, digits = 3),
    TRUE ~ format(log_rank_p, digits = 2)
  )

  # Add log-rank p-value to the table
  km_table <- cbind(km_table, `Log Rank P-Value` = c(log_rank_p, rep(NA, nrow(km_table) - 1)))

  return(km_table)
}
