source("setup.R")

tourism_agg <- tourism |>
  aggregate_key(state / zone / region, visitors = sum(visitors))
tourism_stretch <- tourism_agg |>
  stretch_tsibble(.init = 48, .step = 1)

if (fs::file_exists("fc.rds")) {
  fc <- readRDS("fc.rds")
} else {
  if (fs::file_exists("fit.rds")) {
    fit <- readRDS("fit.rds")
  } else {
    fit <- tourism_stretch |>
      model(
        ets = ETS(visitors),
        arima = ARIMA(visitors)
      )
    fit <- fit |>
      mutate(comb = (ets + arima) / 2)
    saveRDS(fit, "fit.rds")
  }
  fc <- list()
  for (i in seq(max(tourism_stretch$.id))) {
    fc[[i]] <- fit |>
      filter(.id == i) |>
      reconcile(
        ols = min_trace(ets, method = "ols"),
        wlsv = min_trace(ets, method = "wls_var"),
        wlss = min_trace(ets, method = "wls_struct"),
        mint_s = min_trace(ets, method = "mint_shrink"),
      ) |>
      forecast(h = "2 years")
  }
  fc <- bind_rows(fc)
  fc <- fc |>
    group_by(.id, state, zone, region, .model) |>
    mutate(h = row_number()) |>
    ungroup() |>
    as_fable(response = "visitors", distribution = visitors)
  saveRDS(fc, "fc.rds")
}

if(fs::file_exists("acc.rds")) {
  acc <- readRDS("acc.rds")
} else {
  denom <- tourism_agg |>
    mutate(diff = difference(visitors, lag=12)) |>
    as_tibble() |>
    group_by(state, zone, region) |>
    summarise(
      scale1 = mean(abs(diff), na.rm=TRUE),
      scale2 = mean(diff^2, na.rm=TRUE)
    )
  e <- as_tibble(fc) |>
    select(-visitors) |>
    left_join(as_tibble(tourism_agg),
              by = c("month","state","zone","region")) |>
    mutate(e = visitors - .mean) |>
    left_join(denom, by=c("state","zone","region"))
  acc <- e |>
    group_by(.model, h, state, zone, region) |>
    summarise(
      mase = mean(abs(e)/scale1, na.rm=TRUE),
      rmsse = sqrt(mean(e^2/scale2, na.rm=TRUE))
    )
  saveRDS(acc,"acc.rds")
}

