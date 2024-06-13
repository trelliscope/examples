
read_and_calc <- function(x) {
  dt <- as.Date(Sys.time()) - lubridate::years(1)

  symbol <- tools::file_path_sans_ext(basename(x))
  message(symbol)
  tmp <- vroom(x, col_types = "Dnnnnnn", progress = FALSE) |>
    dplyr::filter(Index >= dt - lubridate::days(300))
  names(tmp) <- c("date", "open", "high", "low", "close", "vol", "adj")
  if (any(is.na(tmp$close))) {
    idx_dt <- tmp$date[max(which(is.na(tmp$close)))]
    if (idx_dt > dt) {
      message("Missing values... ignoring")
      return(NULL)
    } else {
      tmp <- dplyr::filter(tmp, date > idx_dt)
    }
  }
  if (nrow(tmp) < 200) {
    message("Not enough data... ignoring")
    return(NULL)
  }
  tmp$close[tmp$close <= 0] <- 1e-6
  tmp$sma200 <- SMA(tmp$close, n = 200)
  tmp$sma50 <- SMA(tmp$close, n = 50)
  tmp$sma20 <- SMA(tmp$close, n = 20)
  tmp$rsi14 <- RSI(tmp$close)
  tmp$obv <- OBV(tmp$close, tmp$vol)
  tmp <- bind_cols(tmp, aroon(tmp[, c("high", "low")], n = 25) |>
    as_tibble() |>
    rename(arn_up = aroonUp, arn_dn = aroonDn, arn_osc = oscillator))
  tmp <- bind_cols(tmp, ADX(tmp[, c("high", "low", "close")])[, c(1, 2, 4)] |>
    as_tibble() |>
    rename(adx_dip = DIp, adx_n = DIn, adx = ADX))
  pd <- dplyr::filter(tmp, date >= dt)
  idx <- seq_len(nrow(pd))
  capture.output(segs <- dpseg(x = idx, y = pd$close, jumps = FALSE,
    type = "var", maxl = 50, minl = 15, store.matrix = TRUE))
  pd$cls_seg <- predict(segs)$y
  capture.output(segs <- dpseg(x = idx, y = pd$obv, jumps = TRUE,
    type = "var", maxl = 50, minl = 15, store.matrix = TRUE))
  pd$obv_seg <- predict(segs)$y

  nn <- nrow(pd)
  cls <- pd$close

  tibble(
    symbol = symbol,
    # vol[nn] / median(tail(vol, 6)[-5])
    vol_med10 = pd$vol[nn] / median(tail(pd$vol, 11)[-11]),
    vol_med100 = pd$vol[nn] / median(tail(pd$vol, 101)[-100]),
    prc_min_pct_chg = 100 * (cls[nn] - min(cls)) / min(cls),
    prc_max_pct_chg = 100 * (cls[nn] - max(cls)) / max(cls),
    days_min = nn - which.min(cls),
    days_max = nn - which.max(cls),
    prc_5d_pct_chg = 100 * (cls[nn] - cls[nn - 5]) / cls[nn - 5],
    prc_10d_pct_chg = 100 * (cls[nn] - cls[nn - 10]) / cls[nn - 10],
    prc_100d_pct_chg = 100 * (cls[nn] - cls[nn - 100]) / cls[nn - 100],
    prc_sma20_pct_diff = 100 * (pd$sma20[nn] - cls[nn]) / cls[nn],
    prc_sma50_pct_diff = 100 * (pd$sma50[nn] - cls[nn]) / cls[nn],
    prc_sma200_pct_diff = 100 * (pd$sma200[nn] - cls[nn]) / cls[nn],
    obv_dir = ifelse(pd$obv_seg[nn] > pd$obv_seg[nn - 1], "up", "down"),
    prc_dir = ifelse(pd$cls_seg[nn] > pd$cls_seg[nn - 1], "up", "down"),
    rsi_cat = as.character(cut(pd$rsi14[nn], breaks = c(0, 30, 70, 100),
      labels = c("oversold", "neutral", "overbought"))),
    cur_rsi = pd$rsi14[nn],
    prices = list(pd)
  )
}

get_fundamentals <- function(dpath) {
  ff <- list.files(file.path(dpath, "metrics"), full.names = TRUE)
  md <- readr::read_csv(ff[1], show_col_types = FALSE, progress = FALSE) |>
    dplyr::rename_all(tolower) |>
    dplyr::select(-rank)
  for (f in ff[-1]) {
    tmp <- readr::read_csv(f, show_col_types = FALSE, progress = FALSE) |>
      dplyr::rename_all(tolower) |>
    dplyr::select(3:4)
    md <- dplyr::left_join(md, tmp, by = "symbol")
  }
  md
}

plot_fn <- function(symbol, prices) {
  pd <- prices[[1]]
  udts <- unique(pd$date)
  dt <- as.Date(Sys.time()) - lubridate::years(1)
  brks <- setdiff(seq(dt, as.Date(Sys.time()), by = "1 day"), udts)

  # line_attr <- list(color = "black", width = 0.75)
  # "#4E79A7" "#A0CBE8" "#F28E2B" "#FFBE7D" "#59A14F" "#8CD17D" "#B6992D"
  # "#F1CE63" "#499894" "#86BCB6" "#E15759" "#FF9D9A" "#79706E" "#BAB0AC"
  # "#D37295" "#FABFD2" "#B07AA1" "#D4A6C8" "#9D7660" "#D7B5A6"

  bb <- list(
    type = "rect",
    x0 = 0, x1 = 1, xref = "paper",
    y0 = 0, y1 = 1, yref = "paper",
    line = list(width = 1, color = "#444444")
  )

  library(plotly)
  f1 <- plot_ly(pd) |>
    add_trace(type = "candlestick",
    x = ~ date, open = ~ open, close = ~ close, high = ~ high, low = ~ low,
    # increasing = list(line = line_attr, fillcolor = "#89B4E7"),
    # decreasing = list(line = line_attr, fillcolor = "#ffffff"),
    increasing = list(line = list(color = "#59A14F", width = 0.75)),
    decreasing = list(line = list(color = "#E15759", width = 0.75)),
    name = "Adjusted Price"
  ) |>
    add_lines(x = ~ date, y = ~ round(cls_seg, 2), name = "Segmented",
      line = list(color = "black", width = 1), opacity = 0.3) |>
    add_lines(x = ~ date, y = ~ round(sma20, 2), name = "20-day SMA",
      line = list(color = "#4E79A7", width = 2), opacity = 0.65) |>
    add_lines(x = ~ date, y = ~ round(sma50, 2), name = "50-day SMA",
      line = list(color = "#77A2C8", width = 2), opacity = 0.65) |>
    add_lines(x = ~ date, y = ~ round(sma200, 2), name = "200-day SMA",
      line = list(color = "#A0CBE8", width = 2), opacity = 0.65) |>
    layout(yaxis = list(title = "Price"), shapes = list(bb))
  f2 <- plot_ly(pd, x = ~ date, y = ~ vol, type = "bar", name = "Volume",
    color = ~ close > open, colors = c("#E15759", "#59A14F"), opacity = 0.8) |>
    layout(yaxis = list(title = "Volume"), shapes = list(bb))
  f3 <- plot_ly(pd) |>
    add_lines(x = ~ date, y = ~ obv, name = "On-balance volume",
      line = list(color = "black", width = 2), opacity = 0.8) |>
    add_lines(x = ~ date, y = ~ obv_seg, name = "OBV segments",
      line = list(color = "black", width = 2), opacity = 0.3) |>
    layout(yaxis = list(title = "OBV"), shapes = list(bb))
  f4 <- plot_ly(pd) |>
    add_lines(x = ~ date, y = ~ rsi14, name = "Relative strength index",
      line = list(color = "black", width = 2), opacity = 0.8) |>
    layout(
      yaxis = list(title = "RSI"),
      shapes = list(
        bb,
        list(
          type = "line",
          x0 = 0, x1 = 1, xref = "paper",
          y0 = 30, y1 = 30,
          line = list(width = 2, color = "#F28E2B", dash = 4),
          opacity = 0.9,
          layer = "below"
        ),
        list(
          type = "line",
          x0 = 0, x1 = 1, xref = "paper",
          y0 = 70, y1 = 70,
          line = list(width = 2, color = "#B07AA1", dash = 4),
          opacity = 0.9,
          layer = "below"
        )
      )
    ) |>
    add_annotations(
      x = c(1, 1), xref = "paper",
      y = c(70, 30),
      text = c("Overbought", "Oversold"),
      font = list(color = "#000000"), showarrow = FALSE)
  subplot(list(f1, f2, f3, f4), nrows = 4, shareX = TRUE, titleY = TRUE,
    heights = c(0.5, 0.1, rep(0.2, 2)), margin = 0) |>
    layout(
      xaxis = list(
        rangeslider = list(visible = FALSE),
        rangebreaks = list(list(values = as.Date(brks))),
        title = NULL
      ),
      hovermode = "x unified",
      showlegend = FALSE,
      margin = list(r = 3, l = 3, t = 3, b = 0, pad = 0)
    ) |>
    plotly::config(displaylogo = FALSE)
}


# turn each of the following into a text description
set_stock_data_labels <- function(d) {
  set_var_labels(
    d,
    total_assets = "Total assets", # log 10
    cost_to_borrow = "Cost to borrow", # log 10
    cash_on_hand = "Cash on hand", # log 10
    total_debt = "Total debt", # log 10
    employees_count = "Number of employees", # log 10
    total_liabilities = "Total liabilities", # log 10
    marketcap = "Market capitalization", # log 10
    net_assets = "Net assets", # log 10 (but has 192 negative values...)
    operating_margin_ttm = "Percent of revenues remaining after paying all operating expenses over past 12 months", # percent
    pb_ratio = "Price-to-book ratio",
    pe_ratio_ttm = "Price-to-earnings ratio over past 12 months",
    earnings_ttm = "Earnings over past 12 months",
    revenue_ttm = "Revenue over past 12 months",
    dividend_yield_ttm = "Dividend yield over past 12 months", # log 10
    market_cap_cat = "Market cap category",
    vol_med10 = "Volume relative to median of last 10 days",
    vol_med100 = "Volume relative to median of last 100 days",
    prc_min_pct_chg = "Percent change from minimum price over last year",
    prc_max_pct_chg = "Percent change from maximum price over last year",
    days_min = "Days since minimum price over last year",
    days_max = "Days since maximum price over last year",
    prc_5d_pct_chg = "Percent change from price 5 days ago",
    prc_10d_pct_chg = "Percent change from price 10 days ago",
    prc_100d_pct_chg = "Percent change from price 100 days ago",
    prc_sma20_pct_diff = "Percent difference from 20-day simple moving average",
    prc_sma50_pct_diff = "Percent difference from 50-day simple moving average",
    prc_sma200_pct_diff = "Percent difference from 200-day simple moving average",
    obv_dir = "Direction of on-balance volume",
    prc_dir = "Direction of price",
    rsi_cat = "Relative Strength Indicator overbought or oversold",
    cur_rsi = "Current Relative Strength Indicator value"
  )
}
