# first_time_contributors.R
# Compute the share of merged PRs that came from someone who had never
# previously merged a PR into rweekly/rweekly.org.
#
# Primary metric: repo-local first-time
#   Sort every merged PR by merge time. For each PR, ask whether the
#   author has any earlier merged PR in the repo. If not -> first-time.
#   Robust, deterministic, doesn't depend on GitHub's live state.
#
# Note on GitHub's built-in FIRST_TIME_CONTRIBUTOR tag: the `/pulls` list
# endpoint reports `author_association` as of the current moment, not as of
# the PR's creation. Once a user has any merged PR, all their historical
# PRs re-report as CONTRIBUTOR. So this API cannot be used to reconstruct
# historical first-timer counts; we compute them locally instead.
# We DO still summarise author_association as a "outsider vs regular vs
# maintainer" split at fetch time.
#
# Outputs:
#   data/first_time_contributors_by_year.csv    yearly summary
#   data/first_time_contributors_overall.csv    one-row headline metrics
#   data/first_time_contributors_by_pr.csv      one row per PR (audit trail)
#   data/author_association_snapshot.csv        current MEMBER/CONTRIBUTOR/NONE mix
#
# Requires data/merged_prs_cache.rds (built by scripts/fetch_merged_prs.R).

source("/mnt/nvme2/r_projects/rweekly-positconf2026/scripts/fetch_merged_prs.R")

out_dir <- Sys.getenv("RWEEKLY_METRICS_OUT",
                      unset = "/mnt/nvme2/r_projects/rweekly-positconf2026/data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

df <- fetch_merged_prs()
message(sprintf("Analysing %d merged PRs", nrow(df)))

# ---------------------------------------------------------------------------
# (a) Repo-local first-time (primary metric)
# ---------------------------------------------------------------------------
df$merge_year <- as.integer(format(df$merged_at, "%Y"))

# Guard against out-of-order rows just in case
df <- df[order(df$merged_at, df$number), ]

# For each row, does an *earlier* merged PR exist by the same author?
seen <- new.env(parent = emptyenv())
df$repo_first_time <- vapply(df$author, function(a) {
  is_new <- !exists(a, envir = seen, inherits = FALSE)
  if (is_new) assign(a, TRUE, envir = seen)
  is_new
}, logical(1))

# ---------------------------------------------------------------------------
# Author association snapshot (current MEMBER/CONTRIBUTOR/NONE mix)
# ---------------------------------------------------------------------------
assoc_tab <- as.data.frame(table(df$author_association),
                            stringsAsFactors = FALSE)
names(assoc_tab) <- c("author_association", "merged_prs")
assoc_tab$pct <- round(100 * assoc_tab$merged_prs / sum(assoc_tab$merged_prs), 1)
# Higher up the list = more central
assoc_order <- c("OWNER", "MEMBER", "COLLABORATOR", "CONTRIBUTOR",
                 "FIRST_TIME_CONTRIBUTOR", "FIRST_TIMER", "NONE")
assoc_tab <- assoc_tab[order(match(assoc_tab$author_association, assoc_order)), ]

# ---------------------------------------------------------------------------
# Reporting
# ---------------------------------------------------------------------------
n_total    <- nrow(df)
n_repo_ft  <- sum(df$repo_first_time)
n_authors  <- length(unique(df$author))

message("\n=== Overall ===")
message(sprintf("Merged PRs total:                                   %d", n_total))
message(sprintf("Unique merged-PR authors:                           %d", n_authors))
message(sprintf("PRs that were the author's first (repo-local):      %d  (%.1f%%)",
                n_repo_ft, 100 * n_repo_ft / n_total))
message(sprintf("Repeat / returning-contributor PRs:                 %d  (%.1f%%)",
                n_total - n_repo_ft, 100 * (n_total - n_repo_ft) / n_total))

# Sanity: n_repo_ft should equal n_authors (every author has exactly one first PR)
if (n_repo_ft != n_authors) {
  message(sprintf(
    "  NOTE: repo-first-time count (%d) differs from unique-author count (%d).",
    n_repo_ft, n_authors))
}

message("\n=== Current author_association mix (snapshot; not historical) ===")
for (i in seq_len(nrow(assoc_tab))) {
  message(sprintf("  %-22s %5d PRs  (%.1f%%)",
                  assoc_tab$author_association[i],
                  assoc_tab$merged_prs[i],
                  assoc_tab$pct[i]))
}

# ---------------------------------------------------------------------------
# Yearly breakdown
# ---------------------------------------------------------------------------
yearly <- do.call(rbind, lapply(split(df, df$merge_year), function(sub) {
  data.frame(
    year                    = sub$merge_year[1],
    merged_prs              = nrow(sub),
    unique_authors          = length(unique(sub$author)),
    new_contributors        = sum(sub$repo_first_time),
    returning_contributors  = length(unique(sub$author)) -
                              sum(sub$repo_first_time),
    pct_prs_from_first_time = round(100 * mean(sub$repo_first_time), 1),
    stringsAsFactors = FALSE
  )
}))
yearly <- yearly[order(yearly$year), ]
row.names(yearly) <- NULL

message("\n=== Yearly breakdown ===")
message(sprintf("%-6s %8s %10s %6s %12s %14s",
                "year", "prs", "authors", "new", "returning", "pct_new_pr"))
for (i in seq_len(nrow(yearly))) {
  message(sprintf("%-6d %8d %10d %6d %12d %13.1f%%",
                  yearly$year[i], yearly$merged_prs[i],
                  yearly$unique_authors[i], yearly$new_contributors[i],
                  yearly$returning_contributors[i],
                  yearly$pct_prs_from_first_time[i]))
}

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
overall <- data.frame(
  merged_prs_total          = n_total,
  unique_authors            = n_authors,
  repo_local_first_time_prs = n_repo_ft,
  repo_local_first_time_pct = round(100 * n_repo_ft / n_total, 2),
  stringsAsFactors = FALSE
)
write.csv(overall, file.path(out_dir, "first_time_contributors_overall.csv"),
          row.names = FALSE)
write.csv(yearly,  file.path(out_dir, "first_time_contributors_by_year.csv"),
          row.names = FALSE)
write.csv(assoc_tab, file.path(out_dir, "author_association_snapshot.csv"),
          row.names = FALSE)

audit <- data.frame(
  pr_number         = df$number,
  author            = df$author,
  merged_at         = format(df$merged_at, "%Y-%m-%d"),
  year              = df$merge_year,
  author_association = df$author_association,
  repo_first_time   = df$repo_first_time,
  stringsAsFactors = FALSE
)
write.csv(audit, file.path(out_dir, "first_time_contributors_by_pr.csv"),
          row.names = FALSE)

message(sprintf("\nWrote 4 CSVs to %s", out_dir))
