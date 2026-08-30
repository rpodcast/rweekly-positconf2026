# fetch_merged_prs.R
# Shared helper: fetch every merged PR to rweekly/rweekly.org and cache the
# essential fields to data/merged_prs_cache.rds.
#
# Reused by:
#   - scripts/first_time_contributors.R
#   - scripts/contribution_cadence.R
#
# Rerun with FORCE_REFETCH=1 in the environment to bypass the cache.

suppressPackageStartupMessages({
  library(gh)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

fetch_merged_prs <- function(owner = "rweekly", repo = "rweekly.org",
                              cache_path = NULL, force = FALSE) {
  if (is.null(cache_path)) {
    out_dir <- Sys.getenv("RWEEKLY_METRICS_OUT",
                          unset = "/mnt/nvme2/r_projects/rweekly-positconf2026/data")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    cache_path <- file.path(out_dir, "merged_prs_cache.rds")
  }

  if (!force && file.exists(cache_path) &&
      !nzchar(Sys.getenv("FORCE_REFETCH"))) {
    message(sprintf("Loading merged-PR cache from %s", cache_path))
    return(readRDS(cache_path))
  }

  message("Fetching all closed PRs from GitHub (may take ~1 minute)...")
  all_prs <- gh(
    "/repos/{owner}/{repo}/pulls",
    owner = owner, repo = repo,
    state = "closed",
    per_page = 100,
    .limit = Inf
  )
  message(sprintf("  fetched %d closed PRs", length(all_prs)))

  # Keep only merged ones and flatten to a compact data frame
  is_merged <- vapply(all_prs, function(pr) !is.null(pr$merged_at), logical(1))
  merged <- all_prs[is_merged]
  message(sprintf("  merged: %d", length(merged)))

  df <- data.frame(
    number     = vapply(merged, function(pr) as.integer(pr$number %||% NA), integer(1)),
    author     = vapply(merged, function(pr) pr$user$login %||% NA_character_, character(1)),
    created_at = vapply(merged, function(pr) pr$created_at %||% NA_character_, character(1)),
    merged_at  = vapply(merged, function(pr) pr$merged_at  %||% NA_character_, character(1)),
    author_association = vapply(
      merged, function(pr) pr$author_association %||% NA_character_, character(1)
    ),
    stringsAsFactors = FALSE
  )
  df$created_at <- as.POSIXct(df$created_at, tz = "UTC",
                              format = "%Y-%m-%dT%H:%M:%SZ")
  df$merged_at  <- as.POSIXct(df$merged_at,  tz = "UTC",
                              format = "%Y-%m-%dT%H:%M:%SZ")
  df <- df[!is.na(df$author) & !is.na(df$merged_at), ]
  df <- df[order(df$merged_at), ]
  row.names(df) <- NULL

  saveRDS(df, cache_path)
  message(sprintf("Cached %d merged PRs to %s", nrow(df), cache_path))
  df
}

# Allow this file to be sourced or run directly for a refresh:
#   Rscript scripts/fetch_merged_prs.R
if (sys.nframe() == 0L && !interactive()) {
  invisible(fetch_merged_prs(force = nzchar(Sys.getenv("FORCE_REFETCH"))))
}
