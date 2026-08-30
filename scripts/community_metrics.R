# community_metrics.R
# Community metrics for the R Weekly project (rweekly/rweekly.org).
#
# Metrics collected:
#   * Age of the R Weekly site (first commit -> today).
#   * Unique contributors of merged PRs that touch `draft.md`.
#   * Unique contributors of merged PRs overall.
#   * Total PRs (all / merged), issues opened, stars, forks, watchers.
#   * Top N PR contributors.
#
# Requires: gh (>= 1.4), gert or git2r optional. Uses a GitHub PAT from
# GITHUB_PAT / GITHUB_TOKEN.

suppressPackageStartupMessages({
  library(gh)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

owner <- "rweekly"
repo  <- "rweekly.org"
draft_file <- "draft.md"

message("=== R Weekly community metrics ===")

# ---------------------------------------------------------------------------
# 1. Age of the site
# ---------------------------------------------------------------------------
repo_meta <- gh("/repos/{owner}/{repo}", owner = owner, repo = repo)

created_at <- as.POSIXct(repo_meta$created_at, tz = "UTC",
                        format = "%Y-%m-%dT%H:%M:%SZ")

# Also grab the first commit date from the gh-pages branch as a sanity check.
commits_head <- gh("/repos/{owner}/{repo}/commits",
                   owner = owner, repo = repo, sha = "gh-pages",
                   per_page = 1)
last_page <- 1L
resp <- attr(commits_head, "response")
if (!is.null(resp) && !is.null(resp$link)) {
  m <- regmatches(resp$link,
                  regexec('<[^>]*[?&]page=(\\d+)[^>]*>;\\s*rel="last"', resp$link))[[1]]
  if (length(m) >= 2) last_page <- as.integer(m[2])
}
first_commit_date <- NA
if (last_page > 1L) {
  first <- gh("/repos/{owner}/{repo}/commits",
              owner = owner, repo = repo, sha = "gh-pages",
              per_page = 1, page = last_page)
  first_commit_date <- as.POSIXct(first[[1]]$commit$author$date, tz = "UTC",
                                  format = "%Y-%m-%dT%H:%M:%SZ")
}

now <- Sys.time()
age_days <- as.numeric(difftime(now, created_at, units = "days"))
age_years <- age_days / 365.25

message(sprintf("Repo created (GitHub):    %s", format(created_at, "%Y-%m-%d")))
if (!is.na(first_commit_date))
  message(sprintf("First commit (gh-pages):  %s", format(first_commit_date, "%Y-%m-%d")))
message(sprintf("Age:                      %.1f years  (%.0f days)",
                age_years, age_days))

# ---------------------------------------------------------------------------
# 2. All merged PRs (paginated)
# ---------------------------------------------------------------------------
message("\nFetching all pull requests (this can take a minute)...")

all_prs <- gh(
  "/repos/{owner}/{repo}/pulls",
  owner = owner, repo = repo,
  state = "closed",          # merged PRs are closed; we filter below
  per_page = 100,
  .limit = Inf
)

message(sprintf("Fetched %d closed PRs total.", length(all_prs)))

is_merged <- vapply(all_prs, function(pr) !is.null(pr$merged_at), logical(1))
merged_prs <- all_prs[is_merged]
message(sprintf("Merged PRs:               %d", length(merged_prs)))

# Also count open PRs
open_prs <- gh("/repos/{owner}/{repo}/pulls",
               owner = owner, repo = repo,
               state = "open", per_page = 100, .limit = Inf)
message(sprintf("Open PRs (currently):     %d", length(open_prs)))

# Unique authors of all merged PRs
authors_all <- vapply(merged_prs, function(pr) {
  if (is.null(pr$user$login)) NA_character_ else pr$user$login
}, character(1))
authors_all <- authors_all[!is.na(authors_all)]
unique_authors_all <- unique(authors_all)
message(sprintf("Unique merged-PR authors (repo-wide): %d",
                length(unique_authors_all)))

# ---------------------------------------------------------------------------
# 3. Unique contributors to draft.md via merged PRs
# ---------------------------------------------------------------------------
# For each merged PR, fetch the file list and check whether draft.md is touched.
message("\nChecking which merged PRs touched draft.md ...")

touched_draft <- logical(length(merged_prs))
authors_draft <- character(length(merged_prs))

pb_step <- max(1L, length(merged_prs) %/% 20L)
for (i in seq_along(merged_prs)) {
  pr <- merged_prs[[i]]
  files <- tryCatch(
    gh("/repos/{owner}/{repo}/pulls/{pull_number}/files",
       owner = owner, repo = repo, pull_number = pr$number,
       per_page = 100, .limit = Inf),
    error = function(e) NULL
  )
  if (!is.null(files) && length(files) > 0) {
    fnames <- vapply(files, function(f) f$filename %||% "", character(1))
    if (draft_file %in% fnames) {
      touched_draft[i] <- TRUE
      authors_draft[i] <- pr$user$login %||% NA_character_
    }
  }
    if (i %% pb_step == 0)
    message(sprintf("  ... %d / %d PRs inspected", i, length(merged_prs)))
}

draft_prs <- merged_prs[touched_draft]
draft_authors <- authors_draft[touched_draft]
draft_authors <- draft_authors[!is.na(draft_authors) & nzchar(draft_authors)]
unique_draft_authors <- unique(draft_authors)

message(sprintf("Merged PRs touching draft.md:              %d",
                length(draft_prs)))
message(sprintf("Unique contributors to draft.md (merged):  %d",
                length(unique_draft_authors)))

# ---------------------------------------------------------------------------
# 4. Extras: issues, stars, forks, top contributors
# ---------------------------------------------------------------------------
message("\n--- Extras ---")
message(sprintf("Stars:                    %d", repo_meta$stargazers_count))
message(sprintf("Forks:                    %d", repo_meta$forks_count))
message(sprintf("Watchers:                 %d", repo_meta$subscribers_count))
message(sprintf("Open issues (incl. PRs):  %d", repo_meta$open_issues_count))

# Total issues opened all time (excluding PRs) via search API
issues_search <- gh("/search/issues",
                    q = sprintf("repo:%s/%s is:issue", owner, repo))
message(sprintf("Total issues ever opened: %d", issues_search$total_count))

# Top PR contributors
tab <- sort(table(authors_all), decreasing = TRUE)
top_n <- head(tab, 15)
message("\nTop 15 merged-PR contributors:")
for (i in seq_along(top_n))
  message(sprintf("  %2d. %-25s %d", i, names(top_n)[i], top_n[i]))

# ---------------------------------------------------------------------------
# 5. Save results
# ---------------------------------------------------------------------------
out_dir <- Sys.getenv("RWEEKLY_METRICS_OUT",
                      unset = "/mnt/nvme2/r_projects/rweekly-positconf2026/data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

summary_df <- data.frame(
  metric = c("created_at", "age_years", "age_days",
             "merged_prs_total", "open_prs",
             "unique_merged_pr_authors",
             "merged_prs_touching_draft_md",
             "unique_draft_md_contributors",
             "stars", "forks", "watchers",
             "total_issues_opened"),
  value = c(format(created_at, "%Y-%m-%d"),
            sprintf("%.2f", age_years),
            sprintf("%.0f", age_days),
            length(merged_prs), length(open_prs),
            length(unique_authors_all),
            length(draft_prs),
            length(unique_draft_authors),
            repo_meta$stargazers_count,
            repo_meta$forks_count,
            repo_meta$subscribers_count,
            issues_search$total_count),
  stringsAsFactors = FALSE
)

write.csv(summary_df, file.path(out_dir, "rweekly_community_metrics.csv"),
          row.names = FALSE)
write.csv(data.frame(login = unique_draft_authors),
          file.path(out_dir, "draft_md_contributors.csv"),
          row.names = FALSE)
write.csv(data.frame(login = names(tab), merged_prs = as.integer(tab)),
          file.path(out_dir, "all_pr_contributors_ranked.csv"),
          row.names = FALSE)

message(sprintf("\nWrote CSVs to %s", normalizePath(out_dir)))
