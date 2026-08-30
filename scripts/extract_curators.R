# extract_curators.R
# Scan every markdown file under _posts/ in the local rweekly.org clone and
# extract the curator name(s) credited in the "curated by ..." byline.
#
# Handles:
#   - Single curator (linked):     "curated by [Name](url)"
#   - Co-curators (linked):        "curated by [Name1](url1) and [Name2](url2)"
#                                  "curated by [Name1](url1) & [Name2](url2)"
#   - Plain-text bylines:          "curated by First Last, with help from ..."
#   - Straight and curly apostrophes in "week's / week’s"
#
# Canonicalises variant display names (short forms, misspellings) to a single
# person by looking at their linked profile URL. Anyone who consistently
# points to the same GitHub handle / personal domain / Twitter / Mastodon /
# Bluesky account is folded into one canonical name.
#
# Output:
#   data/curators_by_issue.csv     one row per (post, curator) with raw display name + URL
#   data/curators_unique.csv       unique canonical curators with counts + span
#   data/curators_raw_variants.csv audit trail: raw display name variants seen per canonical

posts_dir <- "/mnt/nvme2/r_projects/rweekly.org/_posts"
out_dir   <- Sys.getenv("RWEEKLY_METRICS_OUT",
                        unset = "/mnt/nvme2/r_projects/rweekly-positconf2026/data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

files <- list.files(posts_dir, pattern = "\\.md$", full.names = TRUE)
message(sprintf("Scanning %d posts in %s", length(files), posts_dir))

# ---------------------------------------------------------------------------
# 1. Extract the "curated by ..." sentence and pull out every [Name](url) pair
# ---------------------------------------------------------------------------

# Isolate the byline sentence: locate the "curated by" line and take the
# clause up to ", with help from ...".

rows <- list()
for (f in files) {
  txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
  # Find the byline line: work line-by-line so URLs (which contain periods)
  # don't confuse a "up to next period" heuristic.
  byline <- NA_character_
  for (line in head(txt, 60)) {
    if (grepl("curated by ", line, perl = TRUE)) {
      byline <- line
      break
    }
  }
  if (is.na(byline)) next

  # Trim to the "curated by ..." clause and stop at the comma before
  # "with help from ...", which reliably delimits the co-curator list.
  sentence <- sub("^.*?curated by\\s*", "", byline, perl = TRUE)
  sentence <- sub(",?\\s*with help from.*$", "", sentence, perl = TRUE)

  base <- basename(f)
  dm <- regmatches(base, regexec("^(\\d{4})-(\\d{1,2})-(\\d{1,2})-", base))[[1]]
  issue_date <- if (length(dm) >= 4) {
    sprintf("%04d-%02d-%02d",
            as.integer(dm[2]), as.integer(dm[3]), as.integer(dm[4]))
  } else NA_character_

  # First: pull every markdown [Name](url) link inside the byline sentence
  link_pat <- "\\[([^\\]]+)\\]\\(([^)]+)\\)"
  links <- regmatches(sentence, gregexpr(link_pat, sentence, perl = TRUE))[[1]]

  if (length(links) > 0) {
    for (lk in links) {
      parts <- regmatches(lk, regexec(link_pat, lk, perl = TRUE))[[1]]
      if (length(parts) >= 3) {
        rows[[length(rows) + 1L]] <- data.frame(
          file = base, date = issue_date,
          display_name = trimws(parts[2]),
          url = trimws(parts[3]),
          stringsAsFactors = FALSE
        )
      }
    }
  } else {
    # Fallback: plain-text byline like "curated by Wolfram Qin"
    # Take up to two capitalised words as the name (allowing accented letters).
    # We also allow " and " / " & " to split co-curators.
    parts <- unlist(strsplit(sentence, "\\s+(?:and|&)\\s+", perl = TRUE))
    for (piece in parts) {
      piece <- trimws(piece)
      # Strip trailing punctuation
      piece <- sub("[[:punct:]]+$", "", piece)
      if (nzchar(piece) &&
          grepl("^[[:upper:]][[:alpha:]\u00C0-\u024F'-]*(\\s+[[:upper:]][[:alpha:]\u00C0-\u024F'-]*){0,3}$",
                piece, perl = TRUE)) {
        rows[[length(rows) + 1L]] <- data.frame(
          file = base, date = issue_date,
          display_name = piece,
          url = NA_character_,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

df <- do.call(rbind, rows)
df <- df[order(df$date, df$file), ]
row.names(df) <- NULL

# ---------------------------------------------------------------------------
# 2. Canonicalise: use URL "identity" tokens to group variants
# ---------------------------------------------------------------------------
# Extract a stable identity token from each URL. Priority:
#   github.com/<user>            -> github:<user>
#   twitter.com/<user>           -> twitter:<user>
#   *.mastodon-like /@user       -> masto:<user>
#   <user>.bsky.social path      -> bsky:<user>
#   personal domain              -> host:<hostname>
#
# Comparison is case-insensitive.
identity_of <- function(url) {
  if (is.na(url) || !nzchar(url)) return(NA_character_)
  u <- tolower(trimws(url))
  u <- sub("^https?://", "", u)
  u <- sub("^www\\.", "", u)

  # github.com/<user>[/...]
  if (grepl("^github\\.com/[^/]+", u)) {
    return(paste0("github:", sub("^github\\.com/([^/?#]+).*", "\\1", u)))
  }
  # twitter.com/<user>
  if (grepl("^twitter\\.com/[^/]+", u)) {
    return(paste0("twitter:", sub("^twitter\\.com/([^/?#]+).*", "\\1", u)))
  }
  # mastodon-style host/@user (fosstodon.org/@x, mstdn.social/@x, podcastindex.social/@x, ...)
  if (grepl("^[^/]+/@[^/]+", u)) {
    return(paste0("masto:", sub("^[^/]+/@([^/?#]+).*", "\\1", u)))
  }
  # bluesky: bsky.app/profile/<handle>
  if (grepl("^bsky\\.app/profile/[^/]+", u)) {
    handle <- sub("^bsky\\.app/profile/([^/?#]+).*", "\\1", u)
    # Strip .bsky.social suffix so it lines up with other bsky handles
    handle <- sub("\\.bsky\\.social$", "", handle)
    return(paste0("bsky:", handle))
  }
  # Fallback: personal-domain host, keep just the registrable hostname
  host <- sub("^([^/?#]+).*", "\\1", u)
  paste0("host:", host)
}

df$identity <- vapply(df$url, identity_of, character(1))
# For plain-text credits with no URL, use the normalised display name as a
# fallback identity token. That way "Wolfram Qin" from three link-less
# bylines groups together.
plain <- is.na(df$identity)
df$identity[plain] <- paste0("name:", tolower(trimws(df$display_name[plain])))

# Union-find style grouping: two identities refer to the same person if they
# ever appear with the same display_name (case-insensitive, whitespace-tidy)
# OR if a hand-curated alias table below says so. We use the alias table to
# glue together identities that never share an exact display string (e.g.
# "Colin" + colinfay.me   with   "Colin Fay" + github.com/ColinFay).
canonical_alias <- list(
  # canonical name -> character vector of identity tokens to merge
  "Colin Fay"        = c("github:colinfay", "host:colinfay.me", "twitter:_colinfay",
                         "twitter:colinfay"),
  "Ryo Nakagawara"   = c("github:ryo-n7", "twitter:r_by_ryo", "masto:r_by_ryo",
                         "bsky:rbyryo"),
  "Batool Almarzouq" = c("github:batoolmm", "host:batool-almarzouq.netlify.app",
                         "twitter:batoolmm"),
  "Tony ElHabr"      = c("twitter:tonyelhabr", "host:tonyelhabr.rbind.io",
                         "host:tonyelhabr.com"),
  "Eric Nantz"       = c("twitter:thercast", "masto:rpodcast",
                         "github:rpodcast", "host:r-podcast.org"),
  "Jonathan Carroll" = c("twitter:carroll_jono", "masto:jonocarroll",
                         "github:jonocarroll", "host:jcarroll.com.au",
                         "host:jcarroll.xyz"),
  "Jon Calder"       = c("twitter:jonmcalder", "github:jonmcalder",
                         "bsky:jonmcalder", "masto:jonmcalder"),
  "Jonathan Kitt"    = c("github:kittjonathan", "bsky:jonathankitt",
                         "masto:kittjonathan", "twitter:kittjonathan"),
  "Maëlle Salmon"    = c("twitter:ma_salmon", "github:maelle",
                         "masto:maelle", "host:masalmon.eu"),
  "Kelly Bodwin"     = c("twitter:kellybodwin", "github:kbodwin",
                         "host:kbodwin.github.io"),
  "Miles McBain"     = c("twitter:milesmcbain", "github:milesmcbain",
                         "masto:milesmcbain", "host:milesmcbain.xyz"),
  "Robert Hickman"   = c("twitter:robwhickman", "github:robwhickman",
                         "masto:robwhickman"),
  "Sam Parmar"       = c("github:parmsam", "twitter:parmsam_"),
  "Emily Robinson"   = c("twitter:emilyrobinson_a", "twitter:robinson_es",
                         "github:erobinson95"),
  "Wolfram Qin"      = c("name:wolfram qin", "name:wolfram king", "github:qinwf",
                         "twitter:wolfram_qin")
  # Wolfram Qin is the founder of R Weekly. Early on he curated issues under
  # both "Wolfram Qin" and "Wolfram King" plain-text bylines; both are merged
  # here.
)

# Build the identity -> canonical map. Start with every observed identity
# mapped to itself, then apply the alias table.
canon_map <- setNames(unique(df$identity), unique(df$identity))
for (canon in names(canonical_alias)) {
  for (id in canonical_alias[[canon]]) {
    canon_map[[id]] <- canon
  }
}
# For identities not covered by the alias table, keep the *most common*
# display name they were credited under.
uncovered <- setdiff(unique(df$identity), unlist(canonical_alias))
for (id in uncovered) {
  names_seen <- df$display_name[df$identity == id]
  best <- names(sort(table(names_seen), decreasing = TRUE))[1]
  canon_map[[id]] <- best
}

df$canonical <- unname(canon_map[df$identity])

# ---------------------------------------------------------------------------
# 3. Emit tables
# ---------------------------------------------------------------------------
write.csv(df, file.path(out_dir, "curators_by_issue.csv"), row.names = FALSE)

# Per canonical curator
by_canon <- split(df, df$canonical)
uniq <- do.call(rbind, lapply(by_canon, function(sub) {
  data.frame(
    canonical_name = sub$canonical[1],
    issues_curated = length(unique(sub$file)),
    first_issue    = min(sub$date, na.rm = TRUE),
    last_issue     = max(sub$date, na.rm = TRUE),
    display_variants = paste(sort(unique(sub$display_name)), collapse = " | "),
    identities     = paste(sort(unique(sub$identity)), collapse = " | "),
    stringsAsFactors = FALSE
  )
}))
uniq <- uniq[order(-uniq$issues_curated, uniq$canonical_name), ]
row.names(uniq) <- NULL
write.csv(uniq, file.path(out_dir, "curators_unique.csv"), row.names = FALSE)

# Raw display-name variants audit
variants <- aggregate(file ~ display_name + canonical + identity,
                      data = df, FUN = length)
names(variants)[names(variants) == "file"] <- "n_credits"
variants <- variants[order(variants$canonical, -variants$n_credits), ]
write.csv(variants, file.path(out_dir, "curators_raw_variants.csv"),
          row.names = FALSE)

# ---------------------------------------------------------------------------
# 4. Report
# ---------------------------------------------------------------------------
n_files <- length(files)
credited_files <- length(unique(df$file))
co_curated <- sum(table(df$file) > 1)

message(sprintf("\nPosts scanned:                     %d", n_files))
message(sprintf("Posts with a curator byline:       %d", credited_files))
message(sprintf("Posts without a curator byline:    %d  (mostly pre-2020)",
                n_files - credited_files))
message(sprintf("Co-curated issues:                 %d", co_curated))
message(sprintf("Unique canonical curators:         %d", nrow(uniq)))

message("\nAll unique curators (canonical name, issues curated, first → last):")
for (i in seq_len(nrow(uniq))) {
  message(sprintf("  %2d. %-22s %3d   (%s → %s)",
                  i, uniq$canonical_name[i], uniq$issues_curated[i],
                  uniq$first_issue[i], uniq$last_issue[i]))
}

# Diagnostic: display-name variants that got merged
message("\nDisplay-name variants merged into each canonical curator:")
for (canon in uniq$canonical_name) {
  vs <- unique(df$display_name[df$canonical == canon])
  if (length(vs) > 1) {
    message(sprintf("  %-22s  <-  %s",
                    canon, paste(vs, collapse = ", ")))
  }
}
