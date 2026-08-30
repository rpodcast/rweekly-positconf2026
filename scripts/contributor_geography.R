# contributor_geography.R
# For every unique merged-PR contributor to rweekly/rweekly.org, fetch the
# GitHub user profile and extract `location`, `name`, `company`.
#
# Reads:  data/all_pr_contributors_ranked.csv (produced by community_metrics.R)
# Writes: data/contributor_profiles.csv       (one row per contributor)
#         data/geography_summary.csv          (top locations, coverage stats)
#
# ~5000 requests/hour authenticated is plenty for ~400 users.

suppressPackageStartupMessages({
  library(gh)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

out_dir <- Sys.getenv("RWEEKLY_METRICS_OUT",
                      unset = "/mnt/nvme2/r_projects/rweekly-positconf2026/data")
in_csv  <- file.path(out_dir, "all_pr_contributors_ranked.csv")
profile_csv <- file.path(out_dir, "contributor_profiles.csv")
summary_csv <- file.path(out_dir, "geography_summary.csv")

contribs <- read.csv(in_csv, stringsAsFactors = FALSE)
logins <- contribs$login
message(sprintf("Fetching profiles for %d unique contributors ...", length(logins)))

profiles <- vector("list", length(logins))
step <- max(1L, length(logins) %/% 20L)

for (i in seq_along(logins)) {
  u <- tryCatch(
    gh("/users/{login}", login = logins[i]),
    error = function(e) NULL
  )
  profiles[[i]] <- data.frame(
    login    = logins[i],
    name     = (u$name     %||% NA_character_)[1],
    company  = (u$company  %||% NA_character_)[1],
    location = (u$location %||% NA_character_)[1],
    bio      = (u$bio      %||% NA_character_)[1],
    followers = as.integer(u$followers %||% NA_integer_)[1],
    public_repos = as.integer(u$public_repos %||% NA_integer_)[1],
    created_at = (u$created_at %||% NA_character_)[1],
    stringsAsFactors = FALSE
  )
  if (i %% step == 0)
    message(sprintf("  ... %d / %d profiles fetched", i, length(logins)))
}

df <- do.call(rbind, profiles)
df <- merge(df, contribs, by = "login", all.x = TRUE)
df <- df[order(-df$merged_prs), ]
write.csv(df, profile_csv, row.names = FALSE)
message(sprintf("Wrote %s", profile_csv))

# ---------------------------------------------------------------------------
# Coverage + top locations
# ---------------------------------------------------------------------------
has_loc <- !is.na(df$location) & nzchar(trimws(df$location))
message(sprintf("\nContributors with a location set: %d / %d (%.0f%%)",
                sum(has_loc), nrow(df), 100 * mean(has_loc)))

# Very light normalisation — trim, collapse whitespace, unify a handful of
# obvious equivalents. We are NOT doing full geocoding here.
norm <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x
}

loc <- norm(df$location[has_loc])

# Map to country when we can spot an unambiguous signal. Anything else is
# reported verbatim.
country_of <- function(s) {
  s <- tolower(s)
  patterns <- list(
    "USA"        = "\\b(usa|u\\.s\\.a\\.|u\\.s\\.|united states|america|\\b(ny|nyc|boston|chicago|seattle|san francisco|sf|la|los angeles|washington dc|austin|philadelphia|denver|houston|dallas|atlanta|portland|minneapolis|pittsburgh|detroit|miami)\\b)",
    "UK"         = "\\b(uk|u\\.k\\.|united kingdom|england|scotland|wales|london|manchester|edinburgh|glasgow|oxford|cambridge|bristol|leeds)\\b",
    "Germany"    = "\\b(germany|deutschland|berlin|munich|münchen|hamburg|cologne|köln|frankfurt|stuttgart|leipzig)\\b",
    "France"     = "\\b(france|paris|lyon|marseille|toulouse|bordeaux|nantes|strasbourg)\\b",
    "Canada"     = "\\b(canada|toronto|montreal|montréal|vancouver|ottawa|calgary|edmonton|quebec|québec)\\b",
    "Australia"  = "\\b(australia|sydney|melbourne|brisbane|perth|canberra|adelaide)\\b",
    "Netherlands"= "\\b(netherlands|holland|amsterdam|rotterdam|utrecht|the hague|den haag)\\b",
    "Spain"      = "\\b(spain|españa|madrid|barcelona|valencia|seville|sevilla)\\b",
    "Italy"      = "\\b(italy|italia|rome|roma|milan|milano|turin|torino|bologna|florence|firenze)\\b",
    "Switzerland"= "\\b(switzerland|schweiz|suisse|zurich|zürich|geneva|basel|bern|lausanne)\\b",
    "Sweden"     = "\\b(sweden|stockholm|gothenburg|malmö|uppsala)\\b",
    "Norway"     = "\\b(norway|norge|oslo|bergen|trondheim)\\b",
    "Denmark"    = "\\b(denmark|copenhagen|københavn|aarhus)\\b",
    "Finland"    = "\\b(finland|helsinki|espoo|tampere)\\b",
    "Belgium"    = "\\b(belgium|belgique|brussels|bruxelles|antwerp|ghent|leuven)\\b",
    "Ireland"    = "\\b(ireland|dublin|cork|galway)\\b",
    "Poland"     = "\\b(poland|polska|warsaw|krakow|kraków|wrocław|poznań|gdansk)\\b",
    "Portugal"   = "\\b(portugal|lisbon|lisboa|porto)\\b",
    "Austria"    = "\\b(austria|österreich|vienna|wien|salzburg|graz|innsbruck)\\b",
    "Czechia"    = "\\b(czech|czechia|prague|praha|brno)\\b",
    "Brazil"     = "\\b(brazil|brasil|são paulo|sao paulo|rio de janeiro|brasília|brasilia)\\b",
    "Mexico"     = "\\b(mexico|méxico|cdmx|mexico city|guadalajara|monterrey)\\b",
    "Argentina"  = "\\b(argentina|buenos aires|córdoba|cordoba|rosario)\\b",
    "Chile"      = "\\b(chile|santiago|valparaíso|valparaiso)\\b",
    "Colombia"   = "\\b(colombia|bogotá|bogota|medellín|medellin)\\b",
    "China"      = "\\b(china|beijing|shanghai|shenzhen|guangzhou|hangzhou|chengdu)\\b",
    "Japan"      = "\\b(japan|tokyo|osaka|kyoto|nagoya|sapporo|fukuoka|yokohama)\\b",
    "South Korea"= "\\b(south korea|korea|seoul|busan|incheon)\\b",
    "India"      = "\\b(india|mumbai|delhi|new delhi|bangalore|bengaluru|hyderabad|chennai|kolkata|pune)\\b",
    "Singapore"  = "\\bsingapore\\b",
    "Hong Kong"  = "\\bhong kong\\b",
    "Taiwan"     = "\\b(taiwan|taipei)\\b",
    "Indonesia"  = "\\b(indonesia|jakarta|surabaya|bandung)\\b",
    "Malaysia"   = "\\b(malaysia|kuala lumpur|penang)\\b",
    "Philippines"= "\\b(philippines|manila|cebu)\\b",
    "Vietnam"    = "\\b(vietnam|hanoi|ho chi minh|saigon)\\b",
    "Thailand"   = "\\b(thailand|bangkok|chiang mai)\\b",
    "New Zealand"= "\\b(new zealand|auckland|wellington|christchurch)\\b",
    "South Africa"= "\\b(south africa|cape town|johannesburg|pretoria|durban)\\b",
    "Kenya"      = "\\b(kenya|nairobi|mombasa)\\b",
    "Nigeria"    = "\\b(nigeria|lagos|abuja|ibadan)\\b",
    "Egypt"      = "\\b(egypt|cairo|alexandria)\\b",
    "Israel"     = "\\b(israel|tel aviv|jerusalem|haifa)\\b",
    "Turkey"     = "\\b(turkey|türkiye|istanbul|ankara|izmir)\\b",
    "Iran"       = "\\b(iran|tehran|isfahan)\\b",
    "Russia"     = "\\b(russia|moscow|saint petersburg|st\\.? petersburg)\\b",
    "Ukraine"    = "\\b(ukraine|kyiv|kiev|lviv|odesa|odessa)\\b",
    "Greece"     = "\\b(greece|athens|thessaloniki)\\b",
    "Romania"    = "\\b(romania|bucharest|cluj)\\b",
    "Hungary"    = "\\b(hungary|budapest)\\b"
  )
  for (country in names(patterns)) {
    if (grepl(patterns[[country]], s, perl = TRUE)) return(country)
  }
  NA_character_
}

df$country <- NA_character_
df$country[has_loc] <- vapply(loc, country_of, character(1))

country_tab <- sort(table(df$country, useNA = "ifany"), decreasing = TRUE)
message("\nCountry distribution (mapped from free-text location):")
for (i in seq_along(country_tab)) {
  nm <- names(country_tab)[i]
  if (is.na(nm) || nm == "NA") nm <- "(unmapped / no location)"
  message(sprintf("  %-25s %d", nm, country_tab[i]))
}

# Also show the raw top locations (before mapping) for a sanity check
raw_tab <- sort(table(loc), decreasing = TRUE)
message("\nTop 20 raw location strings:")
for (i in seq_len(min(20, length(raw_tab))))
  message(sprintf("  %3d  %s", raw_tab[i], names(raw_tab)[i]))

# Save summary
summary_df <- data.frame(
  country = names(country_tab),
  contributors = as.integer(country_tab),
  stringsAsFactors = FALSE
)
summary_df$country[is.na(summary_df$country) | summary_df$country == "NA"] <-
  "(unmapped / no location)"
write.csv(summary_df, summary_csv, row.names = FALSE)
message(sprintf("\nWrote %s", summary_csv))

# Distinct countries actually identified
distinct_countries <- sum(!is.na(df$country))
n_countries <- length(unique(df$country[!is.na(df$country)]))
message(sprintf(
  "\nContributors mapped to a country: %d / %d (%d distinct countries).",
  distinct_countries, nrow(df), n_countries
))
