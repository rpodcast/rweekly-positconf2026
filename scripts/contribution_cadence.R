# contribution_cadence.R
# How R Weekly's community contribution activity has evolved over time.
#
# For each month from the first merged PR to today, report:
#   - merged_prs         : how many PRs were merged that month
#   - unique_authors     : how many distinct people had a merged PR that month
#   - new_authors        : how many of those had never merged a PR before
#   - returning_authors  : unique_authors - new_authors
#   - cumulative_authors : running total of distinct all-time authors
#
# Also emits a yearly summary and prints a compact text "sparkline" of monthly
# PR volume so the trend is visible in the terminal without opening a plot.
#
# Outputs:
#   data/cadence_monthly.csv    one row per month
#   data/cadence_yearly.csv     one row per calendar year
#
# Requires data/merged_prs_cache.rds (built by scripts/fetch_merged_prs.R).

source("/mnt/nvme2/r_projects/rweekly-positconf2026/scripts/fetch_merged_prs.R")

out_dir <- Sys.getenv("RWEEKLY_METRICS_OUT",
                      unset = "/mnt/nvme2/r_projects/rweekly-positconf2026/data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

df <- fetch_merged_prs()
df <- df[order(df$merged_at, df$number), ]

message(sprintf("Analysing %d merged PRs from %s to %s",
                nrow(df),
                format(min(df$merged_at), "%Y-%m-%d"),
                format(max(df$merged_at), "%Y-%m-%d")))

# ---------------------------------------------------------------------------
# Tag each PR with its author's first-ever merge date (repo-local)
# ---------------------------------------------------------------------------
first_merge_by_author <- tapply(df$merged_at, df$author, min)
df$author_first_merge <- as.POSIXct(
  first_merge_by_author[df$author], tz = "UTC", origin = "1970-01-01"
)
df$is_new <- df$merged_at == df$author_first_merge

# ---------------------------------------------------------------------------
# Monthly aggregation over a continuous month index (no gaps)
# ---------------------------------------------------------------------------
df$year_month <- format(df$merged_at, "%Y-%m")

start_date <- as.Date(format(min(df$merged_at), "%Y-%m-01"))
end_date   <- as.Date(format(Sys.Date(),      "%Y-%m-01"))
month_seq  <- seq(start_date, end_date, by = "month")
month_key  <- format(month_seq, "%Y-%m")

monthly <- data.frame(year_month = month_key,
                      month_start = month_seq,
                      stringsAsFactors = FALSE)

by_ym <- split(df, df$year_month)
get_val <- function(k, f) {
  sub <- by_ym[[k]]
  if (is.null(sub)) return(0L)
  f(sub)
}

monthly$merged_prs      <- vapply(month_key, function(k) get_val(k, nrow), integer(1))
monthly$unique_authors  <- vapply(month_key, function(k) get_val(k, function(s) length(unique(s$author))), integer(1))
monthly$new_authors     <- vapply(month_key, function(k) get_val(k, function(s) sum(s$is_new)), integer(1))
monthly$returning_authors <- monthly$unique_authors - monthly$new_authors

# Cumulative distinct authors over time
seen <- character(0)
cum <- integer(nrow(monthly))
for (i in seq_len(nrow(monthly))) {
  sub <- by_ym[[monthly$year_month[i]]]
  if (!is.null(sub)) {
    seen <- unique(c(seen, sub$author))
  }
  cum[i] <- length(seen)
}
monthly$cumulative_authors <- cum

# ---------------------------------------------------------------------------
# Yearly rollup
# ---------------------------------------------------------------------------
df$year <- as.integer(format(df$merged_at, "%Y"))
yearly <- do.call(rbind, lapply(split(df, df$year), function(sub) {
  data.frame(
    year               = sub$year[1],
    merged_prs         = nrow(sub),
    unique_authors     = length(unique(sub$author)),
    new_authors        = sum(sub$is_new),
    returning_authors  = length(unique(sub$author)) - sum(sub$is_new),
    active_months      = length(unique(format(sub$merged_at, "%Y-%m"))),
    avg_prs_per_month  = round(nrow(sub) / max(1, length(unique(format(sub$merged_at, "%Y-%m")))), 1),
    stringsAsFactors = FALSE
  )
}))
yearly <- yearly[order(yearly$year), ]
row.names(yearly) <- NULL

# ---------------------------------------------------------------------------
# Reporting
# ---------------------------------------------------------------------------
message("\n=== Yearly cadence ===")
message(sprintf("%-6s %8s %10s %8s %12s %12s %14s",
                "year", "prs", "authors", "new", "returning",
                "active_mo", "avg_prs/mo"))
for (i in seq_len(nrow(yearly))) {
  message(sprintf("%-6d %8d %10d %8d %12d %12d %14.1f",
                  yearly$year[i], yearly$merged_prs[i],
                  yearly$unique_authors[i], yearly$new_authors[i],
                  yearly$returning_authors[i],
                  yearly$active_months[i],
                  yearly$avg_prs_per_month[i]))
}

# Compact text sparkline of monthly PR volume
message("\n=== Monthly merged PRs (text sparkline; each block = higher volume) ===")
spark_chars <- c(" ", "\u2581", "\u2582", "\u2583", "\u2584",
                 "\u2585", "\u2586", "\u2587", "\u2588")
mx <- max(monthly$merged_prs)
buckets <- if (mx == 0) rep(0L, nrow(monthly)) else
  as.integer(round((monthly$merged_prs / mx) * (length(spark_chars) - 1)))
# Group by year for readable output
for (yr in sort(unique(as.integer(substr(monthly$year_month, 1, 4))))) {
  idx <- which(substr(monthly$year_month, 1, 4) == as.character(yr))
  line <- paste(spark_chars[buckets[idx] + 1L], collapse = "")
  # Pad to 12 for years that don't span the full calendar
  pad <- paste(rep(".", 12 - length(idx)), collapse = "")
  # Position pad at the correct end
  first_month <- as.integer(substr(monthly$year_month[idx[1]], 6, 7))
  last_month  <- as.integer(substr(monthly$year_month[idx[length(idx)]], 6, 7))
  prefix_pad <- paste(rep(".", first_month - 1L), collapse = "")
  suffix_pad <- paste(rep(".", 12 - last_month), collapse = "")
  message(sprintf("  %d  |%s%s%s|  (peak this year: %d PRs)",
                  yr, prefix_pad, line, suffix_pad,
                  max(monthly$merged_prs[idx])))
}
message(sprintf("       Legend: %s = 0 ... %s = ~%d PRs/month (peak overall)",
                spark_chars[1], spark_chars[length(spark_chars)], mx))

# Peak month
top <- monthly[order(-monthly$merged_prs), ][1:5, ]
message("\n=== Top 5 busiest months ===")
for (i in seq_len(nrow(top))) {
  message(sprintf("  %s   %3d merged PRs   (%d unique authors, %d new)",
                  top$year_month[i], top$merged_prs[i],
                  top$unique_authors[i], top$new_authors[i]))
}

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
write.csv(monthly, file.path(out_dir, "cadence_monthly.csv"), row.names = FALSE)
write.csv(yearly,  file.path(out_dir, "cadence_yearly.csv"),  row.names = FALSE)
message(sprintf("\nWrote 2 CSVs to %s", out_dir))
