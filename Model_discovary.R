cts_data <- data.frame(wt20$date, wt20$win_by_runs, wt20$target, wt20$current_run_rate, wt20$required_run_rate)

pairs(cts_data)