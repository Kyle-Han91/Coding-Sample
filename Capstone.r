workdir   <- "~/downloads/Capstone"
gpt_date  <- as.Date("2022-11-30")
post_days <- 13L
min_k     <- 5L

for (p in c("readxl","fixest","modelsummary","dplyr",
            "readr","ggplot2","tidyr","lubridate","purrr")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(readxl)
library(fixest)
library(modelsummary)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(purrr)

## Helper functions

to_date <- function(x) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  as.Date(x, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y"))
}

standardize_tp_cols <- function(df) {
  orig <- names(df)
  norm <- tolower(gsub("[[:space:].]", "", orig))
  
  i_name <- which(norm %in% c("name","analyst","analystname"))
  if (!length(i_name)) stop("TP file missing 'Name' column.")
  names(df)[i_name[1]] <- "Name"
  
  i_date <- which(norm %in% c("date","datetime"))
  if (!length(i_date)) stop("TP file missing 'Date' column.")
  names(df)[i_date[1]] <- "Date"
  
  i_tp <- which(norm %in% c("targetprice","tp","targetpriceusd"))
  if (!length(i_tp)) {
    i_tp <- which(orig %in% c("Target Price","TargetPrice","TP"))
    if (!length(i_tp)) stop("TP file missing Target Price column.")
  }
  names(df)[i_tp[1]] <- "TP"
  df
}

standardize_ern_cols <- function(df) {
  orig <- names(df)
  norm <- tolower(gsub("[[:space:].]", "", orig))
  
  i_date <- which(norm %in% c("date","erndate","earningsdate","ecdate"))
  if (!length(i_date)) {
    i_date <- which(orig == "Date")
    if (!length(i_date)) stop("ERN file missing 'Date' column.")
  }
  names(df)[i_date[1]] <- "Date"
  df
}

collapse_last_by_analyst <- function(df_sub) {
  if (!nrow(df_sub)) {
    return(data.frame(Name = character(0), TP = numeric(0)))
  }
  df_sub$.rowid <- seq_len(nrow(df_sub))
  key <- aggregate(Date ~ Name, data = df_sub, FUN = max, na.rm = TRUE)
  merged <- merge(df_sub, key, by = c("Name","Date"))
  o <- order(merged$Name, merged$.rowid)
  merged <- merged[o, ]
  keep <- !duplicated(merged$Name, fromLast = TRUE)
  merged <- merged[keep, ]
  merged[, c("Name","TP")]
}


## Pair TP/ERN

setwd(workdir)
all_files <- list.files(pattern = "\\.xlsx$")

tp_raw  <- all_files[grepl("^[^()]+\\.xlsx$", all_files)]
ern_raw <- all_files[grepl("^ERN\\([^()]+\\)\\.xlsx$", all_files)]

companies_tp_raw  <- gsub("\\.xlsx$", "", tp_raw)
companies_ern_raw <- gsub("^ERN\\(|\\)\\.xlsx$", "", ern_raw)

company_names <- sort(intersect(companies_tp_raw, companies_ern_raw))
if (!length(company_names)) {
  stop("not finding pairing file：Company.xlsx and ERN(Company).xlsx")
}
cat("Number of companies to be processed:", length(company_names), "\n")


## event layer：quarter-interval pre/post compute ev_all

analyze_company_quarter_prepost <- function(tp_df, ern_df, ticker,
                                            post_days, min_k) {
  tp_df  <- standardize_tp_cols(tp_df)
  ern_df <- standardize_ern_cols(ern_df)
  
  tp_df$Date  <- as.Date(to_date(tp_df$Date))
  ern_df$Date <- as.Date(to_date(ern_df$Date))
  
  tp_df <- tp_df[!is.na(tp_df$Date) & !is.na(tp_df$TP), ]
  tp_df <- tp_df[tp_df$TP > 0, ]
  tp_df <- unique(tp_df)
  
  ern_df <- ern_df[!is.na(ern_df$Date) & ern_df$Date >= as.Date("2020-01-01"), ]
  if (nrow(ern_df) < 2L) return(data.frame())
  
  ## analyst with at least 1 record preGPT
  has_pre <- tapply(tp_df$Date < gpt_date, tp_df$Name,
                    function(v) any(v, na.rm = TRUE))
  keep_names <- names(has_pre)[has_pre]
  tp_df <- tp_df[tp_df$Name %in% keep_names, ]
  if (!nrow(tp_df)) return(data.frame())
  
  ec_vec <- sort(unique(ern_df$Date))
  if (length(ec_vec) < 2L) return(data.frame())
  
  out_ev <- data.frame()
  
  for (j in seq_len(length(ec_vec) - 1L)) {
    ec      <- as.Date(ec_vec[j])
    next_ec <- as.Date(ec_vec[j + 1L])
    
    pre_start <- ec
    pre_end   <- next_ec - 1L
    if (pre_end <= pre_start) next
    
    ## this period window, only [ec, next_ec) TP
    win_tp <- tp_df[tp_df$Date >= pre_start & tp_df$Date <= pre_end,
                    , drop = FALSE]
    if (!nrow(win_tp)) next
    
    ## post window in this period
    post_start <- ec
    post_end   <- ec + as.integer(post_days)
    if (post_end > pre_end) post_end <- pre_end
    
    ## post window: collapse last by analyst
    post_sub <- win_tp[win_tp$Date >= post_start & win_tp$Date <= post_end,
                       , drop = FALSE]
    if (!nrow(post_sub)) next
    post_last <- collapse_last_by_analyst(post_sub)
    if (!nrow(post_last)) next
    
    ## pre window
    pre_sub <- win_tp[
      win_tp$Date >= pre_start & win_tp$Date <= pre_end &
        win_tp$Name %in% post_last$Name,
      , drop = FALSE
    ]
    if (!nrow(pre_sub)) next
    pre_last <- collapse_last_by_analyst(pre_sub)
    if (!nrow(pre_last)) next
    
    matched <- intersect(pre_last$Name, post_last$Name)
    if (length(matched) < min_k) next
    
    pre_m  <- pre_last[pre_last$Name %in% matched, , drop = FALSE]
    post_m <- post_last[post_last$Name %in% matched, , drop = FALSE]
    
    log_pre  <- log(pre_m$TP)
    log_post <- log(post_m$TP)
    
    var_pre  <- var(log_pre,  na.rm = TRUE)
    var_post <- var(log_post, na.rm = TRUE)
    if (!is.finite(var_pre)  || !is.finite(var_post)) next
    if (var_pre <= 0 || var_post <= 0) next
    
    var_diff <- var_post - var_pre
    Y_log    <- log(var_post) - log(var_pre)
    
    phase <- if (ec < gpt_date) "Pre-GPT" else "Post-GPT"
    
    out_ev <- rbind(
      out_ev,
      data.frame(
        ticker       = ticker,
        ern_date     = ec,
        next_ern     = next_ec,
        pre_start    = pre_start,
        pre_end      = pre_end,
        post_start   = post_start,
        post_end     = post_end,
        q_len        = as.integer(pre_end - pre_start),
        n_matched    = length(matched),
        var_pre_log  = var_pre,   # variance of log TP
        var_post_log = var_post,
        var_diff_log = var_diff,
        Y_log        = Y_log,     # log(var_post) - log(var_pre)
        ern_phase_vs_gpt = phase,
        stringsAsFactors = FALSE
      )
    )
  }
  
  out_ev
}


ev_all <- data.frame()

for (comp in company_names) {
  tp_file  <- paste0(comp, ".xlsx")
  ern_file <- paste0("ERN(", comp, ").xlsx")
  
  if (!(tp_file %in% all_files && ern_file %in% all_files)) {
    cat("⚠️ Jump", comp, "（lack TP or ERN file）\n")
    next
  }
  cat("Processing events:", comp, "\n")
  
  tp_df  <- read_excel(tp_file)
  ern_df <- read_excel(ern_file)
  
  tmp_ev <- analyze_company_quarter_prepost(
    tp_df, ern_df, ticker = comp,
    post_days = post_days,
    min_k     = min_k
  )
  if (nrow(tmp_ev) > 0) ev_all <- rbind(ev_all, tmp_ev)
}

if (!nrow(ev_all)) stop("No event-level results were obtained
                        (check data or parameter settings).")

write_csv(ev_all, "GPT_dispersion_quarter_prepost_events.csv")
cat("Saved → GPT_dispersion_quarter_prepost_events.csv\n")

## basic parameters
ev_all$ern_phase_vs_gpt <- factor(ev_all$ern_phase_vs_gpt, levels = c("Pre-GPT","Post-GPT"))
ev_all$ticker           <- factor(ev_all$ticker)
ev_all$month_of_year    <- factor(format(ev_all$ern_date, "%m"))

ev_all <- ev_all %>%
  mutate(
    rel_gpt   = (year(ern_date) - year(gpt_date)) * 12 +
      (month(ern_date) - month(gpt_date)),
    months_gpt = rel_gpt,
    PostGPT    = as.integer(ern_date >= gpt_date)
  )


## day layer： variance path (LOCF) ev_daily_qtr

build_daily_paths_one <- function(tp_df,
                                  ern_df,
                                  ticker) {
  # -------- Standardize column names, dates, and cleaning --------
  tp_df  <- standardize_tp_cols(tp_df)
  ern_df <- standardize_ern_cols(ern_df)
  
  tp_df$Date  <- as.Date(to_date(tp_df$Date))
  ern_df$Date <- as.Date(to_date(ern_df$Date))
  
  tp_df <- tp_df[!is.na(tp_df$Date) & !is.na(tp_df$TP), ]
  tp_df <- tp_df[tp_df$TP > 0, ]
  tp_df <- unique(tp_df)
  
  ern_df <- ern_df[!is.na(ern_df$Date) & ern_df$Date >= as.Date("2020-01-01"), ]
  if (nrow(ern_df) < 2L) return(data.frame())
  
  #analyst before GPT
  has_pre <- tapply(tp_df$Date < gpt_date,
                    tp_df$Name,
                    function(v) any(v, na.rm = TRUE))
  keep_names <- names(has_pre)[has_pre]
  tp_df <- tp_df[tp_df$Name %in% keep_names, ]
  if (!nrow(tp_df)) return(data.frame())
  
  ec_vec <- sort(unique(ern_df$Date))
  if (length(ec_vec) < 2L) return(data.frame())
  
  out_daily <- list()
  
  # -------- Construct the daily path for each period based on (ec, next_ec) --------
  for (j in seq_len(length(ec_vec) - 1L)) {
    ec      <- as.Date(ec_vec[j])
    next_ec <- as.Date(ec_vec[j + 1L])
    
    pre_start <- ec
    pre_end   <- next_ec - 1L
    if (pre_end <= pre_start) next
    
    sub_tp <- tp_df[tp_df$Date >= pre_start & tp_df$Date <= pre_end,
                    , drop = FALSE]
    if (!nrow(sub_tp)) next
    
    # sort by analyst, date
    sub_tp <- sub_tp[order(sub_tp$Name, sub_tp$Date), , drop = FALSE]
    
    rel_days <- 0:as.numeric(pre_end - pre_start)
    all_rows <- list()
    
    for (d in rel_days) {
      day_date <- pre_start + d
      
      cur_sub <- sub_tp[sub_tp$Date <= day_date, , drop = FALSE]
      if (!nrow(cur_sub)) next
      
      cur_last <- collapse_last_by_analyst(cur_sub)
      if (!nrow(cur_last)) next
      
      log_tp  <- log(cur_last$TP)
      var_log <- var(log_tp, na.rm = TRUE)
      if (!is.finite(var_log) || var_log <= 0) next
      
      n_analyst <- length(unique(cur_last$Name))
      
      all_rows[[length(all_rows) + 1L]] <- data.frame(
        ticker        = ticker,
        ern_date      = ec,
        next_ern      = next_ec,
        date          = day_date,
        rel_day       = d,
        var_daily_log = var_log,   # variance of log TP at day d
        n_analyst     = n_analyst,
        stringsAsFactors = FALSE
      )
    }
    
    if (length(all_rows) > 0L) {
      out_daily[[length(out_daily) + 1L]] <- dplyr::bind_rows(all_rows)
    }
  }
  
  if (!length(out_daily)) return(data.frame())
  dplyr::bind_rows(out_daily)
}

ev_daily_qtr <- data.frame()

for (comp in company_names) {
  tp_file  <- paste0(comp, ".xlsx")
  ern_file <- paste0("ERN(", comp, ").xlsx")
  
  if (!(tp_file %in% all_files && ern_file %in% all_files)) next
  
  cat("Building daily paths:", comp, "\n")
  
  tp_df  <- read_excel(tp_file)
  ern_df <- read_excel(ern_file)
  
  tmp_daily <- build_daily_paths_one(tp_df, ern_df, ticker = comp)
  if (nrow(tmp_daily) > 0) ev_daily_qtr <- rbind(ev_daily_qtr, tmp_daily)
}

if (!nrow(ev_daily_qtr)) stop("ev_daily_qtr is empty；check data。")

ev_daily_qtr <- ev_daily_qtr %>%
  left_join(
    ev_all %>%
      select(ticker, ern_date, PostGPT, rel_gpt, ern_phase_vs_gpt, month_of_year, n_matched) %>%
      distinct(),
    by = c("ticker","ern_date")
  )

ev_daily_qtr$phase <- factor(ev_daily_qtr$ern_phase_vs_gpt, levels = c("Pre-GPT","Post-GPT"))


## ai_high

if (file.exists("ai_high_list.csv")) {
  ai_list <- read_csv("ai_high_list.csv", show_col_types = FALSE)
  names(ai_list)[1] <- "ticker"
  
  ai_list <- ai_list %>%
    mutate(
      ticker  = as.character(ticker),
      ai_high = 1L
    ) %>%
    select(ticker, ai_high)
  
  ev_all <- ev_all %>%
    mutate(ticker = as.character(ticker)) %>%
    left_join(ai_list, by = "ticker") %>%
    mutate(
      ai_high = ifelse(is.na(ai_high), 0L, ai_high),
      ticker  = factor(ticker)
    )
  
  ev_daily_qtr <- ev_daily_qtr %>%
    mutate(ticker = as.character(ticker)) %>%
    left_join(ai_list, by = "ticker") %>%
    mutate(
      ai_high = ifelse(is.na(ai_high), 0L, ai_high),
      ticker  = factor(ticker)
    )
  
  cat("AI exposure successfully merged.\n")
  
} else {
  ev_all$ai_high       <- 0L
  ev_daily_qtr$ai_high <- 0L
  cat("⚠️notfind ai_high_list.csv，default ai_high = 0。\n")
}


## Shortest-quarter alignment & filtering cross-GPT / latest period

ev_all_flagged <- ev_all %>%
  mutate(
    cross_gpt = (pre_start <= gpt_date & pre_end >= gpt_date)
  ) %>%
  group_by(ticker) %>%
  mutate(
    is_latest_period = (ern_date == max(ern_date, na.rm = TRUE))
  ) %>%
  ungroup()

ev_all_filtered <- ev_all_flagged %>%
  filter(!cross_gpt, !is_latest_period)

valid_events <- ev_all_filtered %>%
  select(ticker, ern_date) %>%
  distinct()

ev_daily_qtr_filt <- ev_daily_qtr %>%
  inner_join(valid_events, by = c("ticker","ern_date"))

q_lengths <- ev_daily_qtr_filt %>%
  group_by(ticker, ern_date) %>%
  summarize(
    q_len = max(rel_day, na.rm = TRUE),
    .groups = "drop"
  )

min_len <- min(q_lengths$q_len, na.rm = TRUE)
cat("Shortest quarter length (days):", min_len, "\n")

ev_daily_qtr_filt <- ev_daily_qtr_filt %>%
  filter(rel_day <= min_len)

## ===========================================================
## 6. Signal effects：AI as common signal（Day1 & D13）
## ===========================================================

## 6.1 Day1 dispersion: var1 ~ PostGPT * ai_high | ticker + month
day1_disp <- ev_daily_qtr_filt %>%
  filter(rel_day == 1) %>%
  group_by(ticker, ern_date) %>%
  summarize(
    var1       = mean(log(var_daily_log), na.rm = TRUE),
    n_analyst1 = sum(n_analyst, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  left_join(
    ev_all_filtered %>%
      select(ticker, ern_date, PostGPT, rel_gpt, month_of_year, ai_high, n_matched) %>%
      distinct(),
    by = c("ticker","ern_date")
  )

day1_disp$ticker        <- factor(day1_disp$ticker)
day1_disp$month_of_year <- factor(day1_disp$month_of_year)

m_D1_base <- feols(
  var1 ~ PostGPT | ticker + month_of_year,
  data    = day1_disp,
  weights = ~ n_analyst1,
  cluster = ~ ticker
)

m_D1_AI <- feols(
  var1 ~ PostGPT * ai_high | ticker + month_of_year,
  data    = day1_disp,
  weights = ~ n_analyst1,
  cluster = ~ ticker
)

## 6.2 D13 dispersion: logVar_h ~ PostGPT * ai_high | ticker + month
h_D <- 13L

daily_D13 <- ev_daily_qtr_filt %>%
  filter(rel_day == h_D,
         is.finite(var_daily_log),
         var_daily_log > 0) %>%
  group_by(ticker, ern_date) %>%
  summarize(
    var_h       = mean(var_daily_log, na.rm = TRUE),
    n_analyst_h = sum(n_analyst,     na.rm = TRUE),
    PostGPT     = first(PostGPT),
    rel_gpt     = first(rel_gpt),
    month_of_year = first(month_of_year),
    ai_high     = first(ai_high),
    .groups     = "drop"
  ) %>%
  mutate(
    logVar_h = log(var_h)
  )

daily_D13$ticker        <- factor(daily_D13$ticker)
daily_D13$month_of_year <- factor(daily_D13$month_of_year)

m_D13_base <- feols(
  logVar_h ~ PostGPT | ticker + month_of_year,
  data    = daily_D13,
  weights = ~ n_analyst_h,
  cluster = ~ ticker
)

m_D13_AI <- feols(
  logVar_h ~ PostGPT * ai_high | ticker + month_of_year,
  data    = daily_D13,
  weights = ~ n_analyst_h,
  cluster = ~ ticker
)

msummary(
  list(
    "Day1_Base" = m_D1_base,
    "Day1_AI"   = m_D1_AI,
    "D13_Base"  = m_D13_base,
    "D13_AI"    = m_D13_AI
  ),
  stars    = TRUE,
  fmt      = "%.3e",
  gof_omit = "IC|Log|Adj|Within|Pseudo|F$",
  output   = "signal_effects_D1_D13.html"
)
cat("Saved → signal_effects_D1_D13.html\n")

## ===========================================================
## 7. Collection effects：AI as imformation collecting tool
## ===========================================================

## 7.1 relative speed：ΔlogVar(d) = logVar(d) - logVar(D1)
panel_ratio_ai <- ev_daily_qtr_filt %>%
  filter(rel_day >= 1,
         is.finite(var_daily_log),
         var_daily_log > 0) %>%
  group_by(ticker, ern_date) %>%
  mutate(
    log_var    = log(var_daily_log),
    log_var_D1 = log_var[rel_day == 1][1],
    dlog_var   = log_var - log_var_D1
  ) %>%
  ungroup()

panel_ratio_ai$ticker        <- factor(panel_ratio_ai$ticker)
panel_ratio_ai$month_of_year <- factor(panel_ratio_ai$month_of_year)

m_ratio_ai <- feols(
  dlog_var ~ PostGPT * ai_high * rel_day | ticker + month_of_year,
  data    = panel_ratio_ai,
  weights = ~ n_analyst,
  cluster = ~ ticker
)

## 7.2 abs speed：ΔVar(d) = Var(d) - Var(D1)
panel_diff_ai <- ev_daily_qtr_filt %>%
  filter(rel_day >= 1,
         is.finite(var_daily_log)) %>%
  group_by(ticker, ern_date) %>%
  mutate(
    var_D1 = var_daily_log[rel_day == 1][1],
    dvar   = var_daily_log - var_D1
  ) %>%
  ungroup()

panel_diff_ai$ticker        <- factor(panel_diff_ai$ticker)
panel_diff_ai$month_of_year <- factor(panel_diff_ai$month_of_year)

m_diff_ai <- feols(
  dvar ~ PostGPT * ai_high * rel_day | ticker + month_of_year,
  data    = panel_diff_ai,
  weights = ~ n_analyst,
  cluster = ~ ticker
)

msummary(
  list(
    "RelSpeed_logratio" = m_ratio_ai,
    "AbsSpeed_diff"     = m_diff_ai
  ),
  stars    = TRUE,
  fmt      = "%.3e",
  gof_omit = "IC|Log|Adj|Within|Pseudo|F$",
  output   = "collection_effects_paths.html"
)
cat("Saved → collection_effects_paths.html\n")

## ===========================================================
## 8. slope: variance change per day
## ===========================================================

slope_raw <- ev_daily_qtr_filt %>%
  filter(rel_day %in% c(0L, min_len),
         is.finite(var_daily_log)) %>%
  group_by(ticker, ern_date, rel_day) %>%
  summarize(
    var = mean(var_daily_log, na.rm = TRUE),
    n   = mean(n_analyst,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols    = c(ticker, ern_date),
    names_from = rel_day,
    values_from = c(var, n),
    names_glue = "{.value}_{rel_day}"
  )

slope_df <- slope_raw %>%
  mutate(
    var_T = .data[[paste0("var_", min_len)]],
    n_T   = .data[[paste0("n_", min_len)]],
    slope = (var_T - var_0) / min_len,
    n_avg = 0.5 * (n_0 + n_T)
  ) %>%
  select(-var_T, -n_T) %>%
  filter(is.finite(slope), !is.na(slope)) %>%
  left_join(
    ev_all_filtered %>%
      select(
        ticker, ern_date,
        PostGPT, rel_gpt,
        month_of_year,
        ai_high,
        n_matched
      ) %>%
      distinct(),
    by = c("ticker","ern_date")
  )

slope_df$ticker        <- factor(slope_df$ticker)
slope_df$month_of_year <- factor(slope_df$month_of_year)

## ===========================================================
## 9. plot：variance path / ratio / AI division / D13 dist / slope dist
## ===========================================================

## 9.1 shortest-quarter aligned variance path (pre vs post)
var_path <- ev_daily_qtr_filt %>%
  group_by(phase, rel_day) %>%
  summarize(
    mean_var = mean(var_daily_log, na.rm = TRUE),
    .groups  = "drop"
  )

p_var <- ggplot(var_path, aes(x = rel_day, y = mean_var, color = phase)) +
  geom_line(size = 1.2) +
  labs(
    title = "Average variance path (shortest-quarter aligned, filtered periods)",
    x = "Days since earnings (rel_day)",
    y = "Mean cross-sectional log variance",
    color = "GPT phase"
  ) +
  theme_minimal(base_size = 16)

ggsave("variance_path_shortest_filtered.png", p_var,
       width = 8, height = 4.5, dpi = 300)

## 9.2 variance ratio path: Post / Pre
var_wide <- var_path %>%
  pivot_wider(names_from = phase, values_from = mean_var) %>%
  mutate(ratio_post_pre = `Post-GPT` / `Pre-GPT`)

p_ratio <- ggplot(var_wide, aes(x = rel_day, y = ratio_post_pre)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_line(size = 1.2) +
  labs(
    title = "Variance ratio path: Post-GPT / Pre-GPT",
    x = "Days since earnings (rel_day)",
    y = "Mean variance (Post) / mean variance (Pre)"
  ) +
  theme_minimal(base_size = 16)

ggsave("variance_ratio_path_Post_over_Pre.png", p_ratio,
       width = 8, height = 4.5, dpi = 300)

## 9.3 variance path by GPT phase × AI exposure
var_path_ai <- ev_daily_qtr_filt %>%
  group_by(rel_day, PostGPT, ai_high) %>%
  summarize(mean_var = mean(var_daily_log, na.rm = TRUE), .groups = "drop") %>%
  mutate(Group = paste0(ifelse(PostGPT==1,"Post","Pre"), "-GPT_AI", ai_high))

p_var_ai <- ggplot(var_path_ai, aes(x = rel_day, y = mean_var, color = Group)) +
  geom_line(size = 1.1) +
  labs(
    title = "Average variance path by GPT phase and AI exposure",
    x = "Days since earnings (rel_day)",
    y = "Mean cross-sectional log variance"
  ) +
  theme_minimal(base_size = 16)

ggsave("variance_path_phase_ai_high.png", p_var_ai,
       width = 8, height = 4.5, dpi = 300)

## 9.4 D13 density: Pre vs Post
p_density_D13 <- daily_D13 %>%
  ggplot(aes(x = logVar_h, fill = factor(PostGPT))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("pink","turquoise"),
                    labels = c("Pre","Post")) +
  labs(
    title = "Distribution of log variance at D13: Pre vs Post GPT",
    x = "logVar_h at D13",
    fill = "GPT phase"
  ) +
  theme_minimal(base_size = 16)

ggsave("density_logVar_D13_pre_post.png", p_density_D13,
       width = 8, height = 4.5, dpi = 300)

## 9.5 slope distribution：by GPT phase × AI-high
slope_plot_df <- slope_df %>%
  mutate(
    phase = ifelse(PostGPT == 1, "Post-GPT", "Pre-GPT"),
    AI_group = ifelse(ai_high == 1, "AI-high", "Non-high")
  )

p_slope <- ggplot(slope_plot_df,
                  aes(x = phase, y = slope)) +
  geom_jitter(alpha = 0.4, size = 1) +
  geom_boxplot(outlier.shape = NA, fill = "white") +
  facet_wrap(~ AI_group) +
  labs(
    title = "Distribution of slope (variance change per day)",
    x = "GPT phase",
    y = "Slope"
  ) +
  theme_minimal(base_size = 16)

ggsave("boxplot_slope_phase_AIhigh.png", p_slope,
       width = 12, height = 4.5, dpi = 300)




## =============================================================================
## NEW MODULE (REVISED): 30-Day Rolling Window Variance (+/- 60 around Earnings)
## =============================================================================

ROLLING_WINDOW <- 30L
EVENT_WINDOW   <- 60L
MIN_K_ROLLING  <- 5L

## robust to_date: handles Excel numeric dates (readxl)
to_date <- function(x) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  as.Date(x, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y"))
}

## helper: within [win_start, win_end], keep last obs per analyst with stable tie-break
last_in_window_by_analyst <- function(df_window) {
  if (!nrow(df_window)) return(df_window[0, c("Name","TP"), drop = FALSE])
  df_window$.rowid <- seq_len(nrow(df_window))
  # order: most recent date first; if tie same day, later rowid wins
  df_window <- df_window[order(df_window$Date, df_window$.rowid, decreasing = TRUE), ]
  df_window <- df_window[!duplicated(df_window$Name), ]
  df_window[, c("Name","TP"), drop = FALSE]
}

calc_rolling_30d_event <- function(tp_df, ern_df, ticker) {
  
  tp_df  <- standardize_tp_cols(tp_df)
  ern_df <- standardize_ern_cols(ern_df)
  
  tp_df$Date  <- as.Date(to_date(tp_df$Date))
  ern_df$Date <- as.Date(to_date(ern_df$Date))
  
  tp_df <- tp_df[!is.na(tp_df$Date) & !is.na(tp_df$TP) & tp_df$TP > 0, ]
  ern_df <- ern_df[!is.na(ern_df$Date) & ern_df$Date >= as.Date("2020-01-01"), ]
  
  if (!nrow(tp_df) || !nrow(ern_df)) return(data.frame())
  
  unique_erns <- sort(unique(ern_df$Date))
  if (!length(unique_erns)) return(data.frame())
  
  # (optional but consistent with your pipeline): keep analysts with >=1 record pre-GPT
  has_pre <- tapply(tp_df$Date < gpt_date, tp_df$Name, function(v) any(v, na.rm = TRUE))
  keep_names <- names(has_pre)[has_pre]
  tp_df <- tp_df[tp_df$Name %in% keep_names, ]
  if (!nrow(tp_df)) return(data.frame())
  
  res_events <- vector("list", length(unique_erns))
  
  for (i in seq_along(unique_erns)) {
    e_date <- as.Date(unique_erns[i])
    if (is.na(e_date)) next
    
    start_study <- e_date - EVENT_WINDOW
    end_study   <- e_date + EVENT_WINDOW
    
    # IMPORTANT: seq step = 1 day (avoid "day" parsing issues)
    days_seq <- seq(from = start_study, to = end_study, by = 1)
    
    # prefilter to only necessary TP range: from (start_study - (ROLLING_WINDOW-1)) to end_study
    tp_rel <- tp_df[tp_df$Date >= (start_study - (ROLLING_WINDOW - 1L)) & tp_df$Date <= end_study, , drop = FALSE]
    if (!nrow(tp_rel)) next
    
    daily_list <- vector("list", length(days_seq))
    
    for (j in seq_along(days_seq)) {
      curr_date <- as.Date(days_seq[j])
      win_start <- curr_date - (ROLLING_WINDOW - 1L)
      
      in_window <- tp_rel[tp_rel$Date >= win_start & tp_rel$Date <= curr_date, , drop = FALSE]
      if (!nrow(in_window)) next
      
      unique_recs <- last_in_window_by_analyst(in_window)
      n_a <- nrow(unique_recs)
      if (n_a < MIN_K_ROLLING) next
      
      v <- var(log(unique_recs$TP), na.rm = TRUE)
      if (!is.finite(v) || is.na(v)) next
      
      daily_list[[j]] <- data.frame(
        ticker     = ticker,
        ern_date   = e_date,
        study_date = curr_date,
        rel_day    = as.integer(curr_date - e_date),
        var_log    = v,
        n_analyst  = n_a,
        stringsAsFactors = FALSE
      )
    }
    
    tmp <- dplyr::bind_rows(daily_list)
    if (nrow(tmp)) res_events[[i]] <- tmp
  }
  
  dplyr::bind_rows(res_events)
}

## =============================================================================
## EXECUTION LOOP (FAST: collect in list, then bind_rows once)
## =============================================================================

cat("Starting 30-Day Rolling Window Analysis (+/- 60 days)...\n")

rolling_list <- vector("list", length(company_names))

for (idx in seq_along(company_names)) {
  comp <- company_names[idx]
  tp_file  <- paste0(comp, ".xlsx")
  ern_file <- paste0("ERN(", comp, ").xlsx")
  
  if (!(tp_file %in% all_files && ern_file %in% all_files)) next
  
  tp_raw  <- read_excel(tp_file)
  ern_raw <- read_excel(ern_file)
  
  tmp_res <- calc_rolling_30d_event(tp_raw, ern_raw, ticker = comp)
  if (nrow(tmp_res)) rolling_list[[idx]] <- tmp_res
}

rolling_results <- dplyr::bind_rows(rolling_list)
cat("Calculation Complete. Rows:", nrow(rolling_results), "\n")

## =============================================================================
## AGGREGATION & PLOTTING
## =============================================================================

if (nrow(rolling_results) > 0) {
  
  rolling_results <- rolling_results %>%
    mutate(
      Phase = ifelse(ern_date < gpt_date, "Pre-GPT", "Post-GPT")
    )
  
  # merge ai_high if available
  if (exists("ai_list")) {
    rolling_results <- rolling_results %>%
      mutate(ticker = as.character(ticker)) %>%
      left_join(ai_list, by = "ticker") %>%
      mutate(ai_high = ifelse(is.na(ai_high), 0L, ai_high))
  } else {
    rolling_results$ai_high <- 0L
  }
  
  plot_data <- rolling_results %>%
    group_by(Phase, rel_day) %>%
    summarize(
      mean_var = mean(var_log, na.rm = TRUE),
      count    = dplyr::n(),
      .groups  = "drop"
    )
  
  p_rolling <- ggplot(plot_data, aes(x = rel_day, y = mean_var, color = Phase)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(linewidth = 1.1) +
    labs(
      title = "30-Day Rolling Window Variance around Earnings",
      subtitle = "Each day t: use analysts with TP in [t-29, t], keep each analyst's latest TP in-window; compute var(log TP)",
      x = "Days Relative to Earnings (-60 to +60)",
      y = "Mean var(log TP) (Rolling 30d)",
      color = "GPT Phase"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top")
  
  print(p_rolling)
  
  ggsave("rolling_30d_variance_around_earnings.png", p_rolling,
         width = 10, height = 6, dpi = 300)
  
  write_csv(rolling_results, "rolling_30d_variance_data.csv")
  cat("Saved plot + CSV for Rolling Window Analysis.\n")
  
} else {
  cat("No data generated. Check input files/date ranges/min_k.\n")
}



library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)

## =============================================================================
## 1. 准备工作
## =============================================================================

if (!exists("ev_all")) stop("请先加载 ev_all 数据表！")

# 获取公司列表
ticker_list <- unique(ev_all$ticker)
vol_list <- list()

cat("开始处理", length(ticker_list), "家公司的波动率数据 (Bloomberg格式)...\n")

## =============================================================================
## 2. 辅助函数
## =============================================================================
clean_date <- function(x) {
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  } else {
    return(as.Date(x, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")))
  }
}

## =============================================================================
## 3. 核心循环
## =============================================================================

for (i in seq_along(ticker_list)) {
  
  tkr <- ticker_list[i]
  
  # --- 修改 1: 适配文件名 (优先找 HP_ 开头的) ---
  # 先试 HP_ISRG.xlsx，如果没有，再试 ISRG.xlsx
  f1 <- paste0("HP_", tkr, ".xlsx")
  f2 <- paste0(tkr, ".xlsx")
  
  if (file.exists(f1)) {
    file_name <- f1
    skip_rows <- 6  # Bloomberg 格式通常前6行是废话
  } else if (file.exists(f2)) {
    file_name <- f2
    skip_rows <- 0  # 假设普通文件没有废话 (根据实际情况调整)
  } else {
    # cat("跳过: 找不到文件", tkr, "\n")
    next
  }
  
  # --- 修改 2: 读取时跳过表头 (skip = 6) ---
  # 这一步非常关键，否则 R 会把 "Security" 当成列名
  price_data <- tryCatch({
    read_excel(file_name, skip = skip_rows)
  }, error = function(e) return(NULL))
  
  if (is.null(price_data) || nrow(price_data) < 2) next
  
  # --- 修改 3: 识别 PX_LAST ---
  names(price_data) <- tolower(names(price_data)) # 转小写
  
  # 增加 "px_last" 到匹配列表
  date_col  <- names(price_data)[grep("date|time", names(price_data))[1]]
  close_col <- names(price_data)[grep("close|adj|price|px_last", names(price_data))[1]]
  
  if (is.na(date_col) || is.na(close_col)) next
  
  # --- 数据清洗、计算、去重 ---
  stock_calc <- price_data %>%
    select(date = all_of(date_col), close = all_of(close_col)) %>%
    mutate(date = clean_date(date)) %>%
    filter(!is.na(date) & !is.na(close)) %>%
    
    # [去重保险]: 针对同一天，只保留最后一行
    arrange(date) %>% 
    group_by(date) %>%
    slice_tail(n = 1) %>% 
    ungroup() %>%
    
    # 计算波动率
    mutate(
      ret = (close - lag(close)) / lag(close),
      ret_sq = ret^2,
      ret_sq = if_else(ret_sq > 0, (ret_sq), NA_real_)
    ) %>%
    filter(!is.na(ret_sq))
  
  # --- 匹配财报日 ---
  target_dates <- ev_all %>% 
    filter(ticker == tkr) %>%
    select(ticker, ern_date) %>%
    distinct()
  
  matched_data <- target_dates %>%
    inner_join(stock_calc, by = c("ern_date" = "date")) %>%
    select(ticker, ern_date, ret_sq)
  
  if (nrow(matched_data) > 0) {
    vol_list[[i]] <- matched_data
  }
  
  if (i %% 100 == 0) cat("已处理", i, "/", length(ticker_list), "\n")
}

## =============================================================================
## 4. 合并结果
## =============================================================================

if (length(vol_list) > 0) {
  vol_data <- bind_rows(vol_list)
  cat("✅ 计算完成！共生成", nrow(vol_data), "条波动率数据。\n")
  print(head(vol_data))
} else {
  warning("⚠️ 未生成数据。请检查目录下是否有 HP_xxx.xlsx 文件。")
}


library(fixest)
library(modelsummary)
library(dplyr)

## =============================================================================
## 1. 准备数据：确保包含 ret_sq (保持你原来的逻辑)
## =============================================================================

if (exists("vol_data") && nrow(vol_data) > 0) {
  
  # 为了保险起见，确保只取需要的列，并且去重
  # (虽然上一步代码已经做过处理，但在合并前再次确保唯一性是好习惯)
  vol_data <- vol_data %>%
    select(ticker, ern_date, ret_sq) %>%
    distinct()
  
  cat("✅ 成功加载 vol_data，准备合并。记录数：", nrow(vol_data), "\n")
  
} else {
  stop("❌ 错误：找不到 'vol_data'！\n请先运行之前那个【批量读取 Excel 计算波动率】的代码块。")
}
# 2. 将波动率数据合并进主数据
# 注意：这里我们先不取 log(var)，留到循环里做聚合时再统一处理
reg_data_multi <- ev_daily_qtr_filt %>%
  left_join(vol_data, by = c("ticker", "ern_date")) %>%
  filter(is.finite(ret_sq))

## =============================================================================
## 2. 循环运行回归 (D1, D13, D30, D60) - 【核心修改区】
## =============================================================================

horizons <- c(1, 13, 30, 60)
models_vol <- list()

for (h in horizons) {
  
  # 1. 筛选特定天数 + 植入 Code B 的聚合逻辑
  df_h <- reg_data_multi %>% 
    filter(rel_day == h) %>%
    
    # 过滤无效方差 (避免 log 报错)
    filter(is.finite(var_daily_log) & var_daily_log > 0) %>%
    
    # --- [关键插入]: Code B 的聚合步骤 ---
    group_by(ticker, ern_date) %>%
    summarize(
      # 1. 因变量: 统一使用 Mean of Logs (数学性质更好)
      log_var_daily = mean(log(var_daily_log), na.rm = TRUE),
      
      # 2. 权重: 该天分析师总人数
      n_analyst = sum(n_analyst, na.rm = TRUE),
      
      # 3. 自变量: 因为是静态变量，用 first() 携带过来
      PostGPT = first(PostGPT),
      ai_high = first(ai_high),
      ret_sq = first(ret_sq),
      month_of_year = first(month_of_year),
      
      .groups = "drop"
    )
  
  # 2. 运行交互模型 (完全保持你原来的公式)
  # log(Var) ~ PostGPT * AI_High + PostGPT * Volatility
  m <- feols(
    log_var_daily ~ PostGPT * ai_high + PostGPT * ret_sq + PostGPT * ai_high * ret_sq | ticker + month_of_year,
    data    = df_h,
    weights = ~ n_analyst, 
    cluster = ~ ticker
  )
  
  # 3. 存入列表
  models_vol[[paste0("Day", h)]] <- m
}

## =============================================================================
## 3. 输出结果 (保持你原来的设置)
## =============================================================================

msummary(
  models_vol,
  stars    = TRUE,
  fmt      = "%.3f",
  gof_omit = "IC|Log|Adj|Within|Pseudo|F$",
  coef_map = c(
    "PostGPT" = "Post-GPT",
    "ai_high" = "High AI Exposure",
    "PostGPT:ai_high" = "Post-GPT x High AI",
    "ret_sq" = "Volatility (Ret Sq)",
    "PostGPT:ret_sq" = "Post-GPT x Volatility"
  ),
  title    = "Does AI Dampen the Impact of Volatility? (Multi-Horizon)",
  output   = "volatility_interaction_multi_horizon.html"
)

print(etable(models_vol, fitstat = c("n", "r2")))


cat("PIPELINE DONE.\n")

library(dplyr)
library(ggplot2)

if (!exists("rolling_results") || nrow(rolling_results) == 0) {
  stop("rolling_results 不存在或为空，请先运行 rolling 30-day 模块。")
}

if (!"ai_high" %in% names(rolling_results)) {
  stop("rolling_results 里没有 ai_high 列，请先 merge ai_high_list。")
}

count_ai_plot <- rolling_results %>%
  mutate(
    AI_group = ifelse(ai_high == 1, "AI-high", "AI-low")
  ) %>%
  group_by(AI_group, rel_day) %>%
  summarize(
    mean_n_targetprice = mean(n_analyst, na.rm = TRUE),
    median_n_targetprice = median(n_analyst, na.rm = TRUE),
    n_event_days = n(),
    .groups = "drop"
  )

p_count_ai <- ggplot(count_ai_plot, aes(x = rel_day, y = mean_n_targetprice, color = AI_group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1.1) +
  labs(
    title = "Average Number of Valid Target Prices in the 30-Day Rolling Window",
    subtitle = "By AI exposure",
    x = "Days Relative to Earnings (-60 to +60)",
    y = "Average Number of Valid Target Prices",
    color = "AI Exposure"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

print(p_count_ai)

ggsave(
  "rolling_30d_valid_targetprice_count_AIhigh_vs_AIlow.png",
  p_count_ai,
  width = 10, height = 6, dpi = 300
)

## =============================================================================
## Robustness: Is the Post-GPT decline driven by AI-related firms?
## =============================================================================

horizons <- c(1, 13)
models_ai_robust <- list()

for (h in horizons) {
  
  df_h <- ev_daily_qtr_filt %>%
    filter(rel_day == h) %>%
    filter(is.finite(var_daily_log), var_daily_log > 0) %>%
    group_by(ticker, ern_date) %>%
    summarize(
      log_var_daily = mean(log(var_daily_log), na.rm = TRUE),
      n_analyst     = sum(n_analyst, na.rm = TRUE),
      PostGPT       = first(PostGPT),
      ai_high       = first(ai_high),
      month_of_year = first(month_of_year),
      .groups       = "drop"
    ) %>%
    filter(
      is.finite(log_var_daily),
      !is.na(PostGPT),
      !is.na(ai_high),
      n_analyst > 0
    ) %>%
    mutate(
      ticker = factor(ticker),
      month_of_year = factor(month_of_year)
    )
  
  m <- feols(
    log_var_daily ~ PostGPT + PostGPT:ai_high | ticker + month_of_year,
    data    = df_h,
    weights = ~ n_analyst,
    cluster = ~ ticker
  )
  
  models_ai_robust[[paste0("Day", h)]] <- m
}

msummary(
  models_ai_robust,
  stars    = TRUE,
  fmt      = "%.3f",
  gof_omit = "IC|Log|Adj|Within|Pseudo|F$",
  coef_map = c(
    "PostGPT"         = "Post-GPT",
    "PostGPT:ai_high" = "Post-GPT x AI-related firm"
  ),
  title    = "Robustness: AI-Related Firms and Post-GPT Disagreement",
  notes    = "Notes: AI-related firm status is used only as a robustness control, not as a treatment group. The main effect of AI-related firm status is absorbed by ticker fixed effects. Standard errors are clustered by ticker.",
  output   = "robustness_ai_related_firms.html"
)

print(etable(models_ai_robust, fitstat = c("n", "r2")))

m <- feols(
  log_var_daily ~ PostGPT * ret_sq | ticker + month_of_year,
  data    = df_h,
  weights = ~ n_analyst,
  cluster = ~ ticker
)
