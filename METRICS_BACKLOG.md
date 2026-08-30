# R Weekly community metrics — ideas backlog

Tracker for community-aspect metrics we're gathering for the lightning talk
inspired by the R Weekly project (`rweekly/rweekly.org`).

Data source is the GitHub API via the R `gh` package unless noted otherwise.
Generated CSVs live in `data/`. The main collection script is
`scripts/community_metrics.R`.

## Legend

- [x] done — value captured (see notes / CSV column)
- [~] in progress
- [ ] todo — not yet fetched

## Metrics

- [x] **Age of the R Weekly site.**
  First commit + repo creation both on 2016-05-21 → 10.3 years / 3,754 days.
  (`data/rweekly_community_metrics.csv`)

- [x] **Unique contributors of merged PRs to `draft.md`.**
  368 unique authors across 1,639 merged PRs touching the file.
  (`data/draft_md_contributors.csv`)

- [x] **Unique contributors of merged PRs repo-wide.**
  406. (`data/rweekly_community_metrics.csv`)

- [x] **Repo-wide PR / issue / star / fork snapshot.**
  1,785 merged PRs, 3 open PRs, 112 issues ever opened, 818 stars, 426 forks,
  42 watchers. (`data/rweekly_community_metrics.csv`)

- [x] **Top merged-PR contributors (ranked).**
  Full list in `data/all_pr_contributors_ranked.csv`; top of the leaderboard:
  ivelasq (88), shikokuchuo (56), maelle (55), jonmcalder (54),
  HenrikBengtsson (53), jonocarroll (51), AlbertRapp (43).

- [x] **Curators over time.**
  Extracted curator byline ("This week's release was curated by ...") from
  every post under `_posts/`. 325 of 507 posts (2019+) carry a byline;
  pre-2019 issues predate the convention. Identified **15 canonical curators
  across 3 co-curated issues**, spanning 2019-04-15 → 2026-08-24. Rankings
  and per-issue records in `data/curators_unique.csv`,
  `data/curators_by_issue.csv`, `data/curators_raw_variants.csv`. Script:
  `scripts/extract_curators.R`. Cross-check with
  `scripts/rweekly_curators.csv` shows perfect alignment (0 missing, 0 extra).
  Notable resolutions from the scan: Emily Robinson surfaced as a 2023-W10
  co-curator (added to CSV), and "Wolfram King" / "Wolfram Qin" plain-text
  bylines were merged into one canonical curator (R Weekly's founder, who
  curated the early solo-run issues).

- [x] **Contributor geography.**
  Fetched `location` from `/users/{login}` for all 406 unique merged-PR
  contributors. 290 (71%) had a location set; all 290 mapped cleanly to
  **34 distinct countries**. Top: USA 55, France 25, Germany 22, UK 21,
  Canada 14, Australia 12. Full profiles in `data/contributor_profiles.csv`;
  country counts in `data/geography_summary.csv`. Script:
  `scripts/contributor_geography.R`.

- [x] **Contribution cadence over time.**
  Monthly and yearly merged-PR counts, unique authors, new-vs-returning
  split, cumulative distinct authors, and a compact text sparkline.
  Cadence has been remarkably stable: 150–250 merged PRs / year for the past
  8 years (2018 onward), average 13–21 merged PRs per month. Busiest month
  ever was 2018-02 (46 PRs). Data in `data/cadence_monthly.csv` and
  `data/cadence_yearly.csv`. Script: `scripts/contribution_cadence.R`.

- [x] **New vs returning contributors per year.**
  Covered by the cadence script (`new_authors` / `returning_authors` columns
  in `data/cadence_yearly.csv`). Every year 2019+ sees a stable base of
  30–50 new contributors, plus a growing pool of returning ones — the
  cumulative distinct-authors curve climbs steadily.

- [x] **RSS feeds tracked.**
  `rss_feeds.csv` on `gh-pages` contains **480 feeds total** (455 currently
  enabled, 25 disabled). 117 also list a Twitter handle. This is the
  passive-content firehose that complements the active-PR contribution track.

- [x] **Total weekly releases shipped.**
  **507 posts** under `_posts/` on `gh-pages`, of which **506 are regular
  weekly issues** (the outlier is a `user2016` conference bulletin from
  July 2016). That's roughly one shipped issue per week for a decade.

- [ ] **PR / issue comment volume.**
  Total comments left on PRs and issues (via `/issues/comments`) — proxy for
  editorial back-and-forth around suggestions.

- [x] **First-time contributor share.**
  Repo-local computation: for each merged PR, was it the author's first
  merged PR to the repo? **406 of 1,785 merged PRs (22.7%)** were an
  author's first — every unique contributor has exactly one first PR, so
  this is a direct restatement of "406 unique contributors". Yearly view
  in `data/first_time_contributors_by_year.csv` shows the "% of PRs from
  first-timers" running 17–46% across the years (never dips below ~15%),
  a strong "the door is always open" signal. GitHub's live
  FIRST_TIME_CONTRIBUTOR tag can't be reconstructed historically from the
  `/pulls` endpoint (it re-labels past PRs once a user becomes a
  contributor), so we omit it. Script: `scripts/first_time_contributors.R`.
  Overall table in `data/first_time_contributors_overall.csv`; per-PR audit
  in `data/first_time_contributors_by_pr.csv`; current author_association
  snapshot (MEMBER / CONTRIBUTOR / NONE = drive-by outsider) in
  `data/author_association_snapshot.csv`.

- [ ] **PR merge latency.**
  Median / mean days between PR opened and merged. Very fast median would
  reinforce the "low-friction contribution" narrative.

- [ ] **Curator credits per contributor (optional stretch).**
  If curators call out contributors by name in their release posts, parse
  `_posts/` for "curated by ... with help from ..." lines to build a
  co-contributor graph.
