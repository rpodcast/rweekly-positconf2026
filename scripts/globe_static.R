# globe_static.R
# Render a "two-hemisphere globe" of R Weekly contributor geography as a
# PNG suitable for the dark-navy slide theme.
#
# Two side-by-side orthographic projections centred on the Americas and
# on Eurasia/Oceania respectively cover 100% of R Weekly's 34 contributor
# countries in a single image.
#
# Reads:  data/geography_summary.csv (produced by
#         scripts/contributor_geography.R)
# Writes: assets/img/contributor_globe.png

suppressPackageStartupMessages({
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(dplyr)
  library(ggplot2)
  library(magick)
})

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
project_dir <- "/mnt/media_drive2/r_projects/rweekly-positconf2026"
data_dir    <- file.path(project_dir, "data")
img_dir     <- file.path(project_dir, "assets", "img")
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)

csv_path <- file.path(data_dir, "geography_summary.csv")
out_path <- file.path(img_dir, "contributor_globe.png")

# Whether to draw the "Contributors" colourbar legend on the right side of
# the stitched image. Override at the command line or from an environment
# variable, e.g.
#
#   RWEEKLY_GLOBE_LEGEND=1 Rscript scripts/globe_static.R
#
# The slide-embedded version omits it (default FALSE); the standalone /
# blog version can enable it for reference.
show_legend_default <- FALSE
show_legend <- as.logical(Sys.getenv("RWEEKLY_GLOBE_LEGEND",
                                     unset = as.character(show_legend_default)))
if (is.na(show_legend)) show_legend <- show_legend_default
message(sprintf("Legend: %s", if (show_legend) "ON" else "OFF"))

# Slide-theme palette (matches styles.scss)
bg_navy      <- "#0F1D3D"     # slide background
bg_navy_deep <- "#0A1530"     # slightly darker for globe outline
land_fill    <- "#1E325D"     # subtle land colour on the globe
land_stroke  <- "#334C7F"     # continent outline
graticule    <- "#25406F"     # meridian / parallel lines
ocean_fill   <- "#0B1A38"     # slightly lighter than slide bg, hints "water"
star_teal    <- "#2CC5B0"
star_yellow  <- "#F2C438"
star_orange  <- "#F09030"
star_magenta <- "#B540B0"
text_light   <- "#F5F7FB"

# Orthographic projection centres for the two hemispheres. Chosen so that
# every one of R Weekly's 34 contributor countries falls in the visible
# hemisphere of exactly one panel, with minimal duplication near the seam.
#   Panel A ("Americas + EMEA"):  Atlantic-facing hemisphere -- shows North
#     & South America, Europe, Africa, Middle East.
#   Panel B ("Asia + Pacific"):   shows East/South Asia and Oceania.
# We tag each contributor country to exactly one panel; the assignment is
# by which projection centre it's closer to. Everything else can be visible
# in the other panel (as land only) for geographic context.
hemispheres <- list(
  list(label = "americas_emea", lon0 = -20, lat0 = 25),
  list(label = "asia_pacific",  lon0 = 125, lat0 = 15)
)

# ---------------------------------------------------------------------------
# 1. Load contributor counts and normalise country names to match Natural
#    Earth's `name` field.
# ---------------------------------------------------------------------------
csv <- read.csv(csv_path, stringsAsFactors = FALSE)
csv <- csv[csv$country != "(unmapped / no location)", ]

# Manual crosswalk for the handful of names that differ between our CSV
# and Natural Earth's canonical labels.
name_map <- c(
  "USA"         = "United States of America",
  "UK"          = "United Kingdom",
  "Czechia"     = "Czechia",
  "South Korea" = "South Korea"
)
csv$ne_name <- ifelse(csv$country %in% names(name_map),
                     name_map[csv$country], csv$country)

# ---------------------------------------------------------------------------
# 2. Load world outline
# ---------------------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")

# Sanity: any CSV country that doesn't join?
missing <- setdiff(csv$ne_name, world$name)
if (length(missing) > 0) {
  message("NOTE: could not match these countries to Natural Earth names:")
  message("  ", paste(missing, collapse = ", "))
  message("Consider adding them to `name_map` above.")
}

world <- world %>%
  left_join(csv, by = c("name" = "ne_name")) %>%
  mutate(contributors = ifelse(is.na(contributors), 0L, contributors))

# ---------------------------------------------------------------------------
# 3. Render a single hemisphere. Returns a ggplot object.
# ---------------------------------------------------------------------------
# Use s2 (spherical) mode so we can clip world polygons to a great-circle
# cap around the projection centre before projecting. This gives clean
# borders at the horizon without any GEOS invalid-geometry pain.
sf_use_s2(TRUE)

R_earth <- 6378137

render_hemisphere <- function(lon0, lat0, world, contribs_pts,
                              label = "", show_legend = TRUE) {
  ortho_crs <- sprintf(
    "+proj=ortho +lon_0=%f +lat_0=%f +ellps=WGS84 +no_defs", lon0, lat0
  )

  # Build a spherical cap centred on (lon0, lat0) with a radius just under
  # 90 degrees (~ 89 to avoid horizon artefacts).
  cap <- s2::s2_buffer_cells(
    s2::as_s2_geography(sprintf("POINT(%f %f)", lon0, lat0)),
    distance = 89 * (pi / 180) * 6371007.2   # radians -> metres on unit
  )
  cap_sf <- st_as_sfc(cap)
  st_crs(cap_sf) <- 4326

  # Clip world in spherical space, then project.
  world_clip <- suppressWarnings(st_intersection(world, cap_sf))
  world_ortho <- suppressWarnings(st_transform(world_clip, crs = ortho_crs))

  # Drop empty / degenerate rows.
  keep <- !vapply(st_geometry(world_ortho), st_is_empty, logical(1))
  world_ortho <- world_ortho[keep, ]

  contribs_sf <- world %>% filter(contributors > 0)

  disc <- st_sfc(st_buffer(st_point(c(0, 0)), R_earth), crs = ortho_crs)
  disc_outline <- disc

  # Project the caller-supplied contributor points and keep those visible
  # on this hemisphere.
  rad <- pi / 180
  cos_d <- sin(lat0 * rad) * sin(contribs_pts$lat * rad) +
           cos(lat0 * rad) * cos(contribs_pts$lat * rad) *
           cos((contribs_pts$lon - lon0) * rad)
  pts_visible <- contribs_pts[cos_d > 0.02, , drop = FALSE]
  n_total_pts <- nrow(contribs_pts)

  if (nrow(pts_visible) > 0) {
    pts_visible_sf <- st_as_sf(
      pts_visible, coords = c("lon", "lat"), crs = 4326
    ) %>% st_transform(crs = ortho_crs)
    pts_visible$X <- st_coordinates(pts_visible_sf)[, 1]
    pts_visible$Y <- st_coordinates(pts_visible_sf)[, 2]
  }

  # Build a graticule directly and project. We only need lines that will
  # partly fall inside the visible disc; the sf plotting will clip.
  graticule_lines <- tryCatch({
    g <- st_graticule(
      lat = seq(-60, 60, by = 30),
      lon = seq(-150, 150, by = 30),
      crs = 4326
    )
    g <- st_segmentize(g, units::set_units(1, "degree"))
    g <- suppressWarnings(st_transform(g, crs = ortho_crs))
    keep <- !vapply(st_geometry(g), st_is_empty, logical(1))
    g[keep, ]
  }, error = function(e) NULL)

  message(sprintf("  %-14s countries assigned here: %d visible / %d supplied",
                  label, nrow(pts_visible), n_total_pts))

  p <- ggplot() +
    geom_sf(data = disc_outline, fill = ocean_fill,
            colour = land_stroke, size = 0.6)
  if (!is.null(graticule_lines)) {
    p <- p + geom_sf(data = graticule_lines, colour = graticule,
                     size = 0.25, alpha = 0.55)
  }
  p <- p +
    geom_sf(data = world_ortho, fill = land_fill, colour = land_stroke,
            size = 0.25)

  if (nrow(pts_visible) > 0) {
    p <- p +
      geom_point(data = pts_visible,
                 aes(x = X, y = Y, size = contributors),
                 colour = star_yellow, alpha = 0.20, show.legend = FALSE) +
      geom_point(data = pts_visible,
                 aes(x = X, y = Y, size = contributors,
                     fill = contributors),
                 shape = 21, colour = "#FFFFFF", stroke = 0.4,
                 alpha = 0.95)
  }

  legend_guide <- if (show_legend) {
    guide_colourbar(
      barheight = unit(3.5, "cm"),
      barwidth  = unit(0.35, "cm"),
      title.theme = element_text(colour = text_light, size = 11),
      label.theme = element_text(colour = text_light, size = 10),
      frame.colour = NA, ticks.colour = NA
    )
  } else "none"

  p +
    scale_size_area(max_size = 20, guide = "none") +
    scale_fill_gradientn(
      colours = c(star_teal, star_yellow, star_orange, star_magenta),
      trans   = "sqrt",
      limits  = c(1, max(csv$contributors)),
      name    = "Contributors",
      guide   = legend_guide
    ) +
    coord_sf(crs = ortho_crs, expand = FALSE) +
    theme_void() +
    theme(
      plot.background   = element_rect(fill = bg_navy, colour = NA),
      panel.background  = element_rect(fill = bg_navy, colour = NA),
      legend.background = element_rect(fill = bg_navy, colour = NA),
      legend.key        = element_rect(fill = bg_navy, colour = NA),
      plot.margin       = margin(6, 6, 6, 6)
    )
}

# ---------------------------------------------------------------------------
# 5. Build the contributor point set once (with sensible centroid overrides
#    for countries whose Natural Earth `label_x/label_y` is off-centre).
# ---------------------------------------------------------------------------
contribs_sf <- world %>% filter(contributors > 0)

centroid_overrides <- data.frame(
  country = c("United States of America", "France", "Russia",
              "Netherlands", "United Kingdom", "Norway",
              "New Zealand", "Chile", "Canada", "Australia"),
  lon     = c(-98.35,   2.35,  55.75,   5.29,  -1.55, 10.75, 174.78, -71.55,
              -96.82, 133.78),
  lat     = c( 39.50,  48.86,  55.75,  52.13,  53.00, 60.47, -41.29, -35.68,
               58.13, -25.27),
  stringsAsFactors = FALSE
)

if (all(c("label_x", "label_y") %in% names(contribs_sf))) {
  pts_all <- data.frame(
    country      = contribs_sf$name,
    contributors = contribs_sf$contributors,
    lon = contribs_sf$label_x, lat = contribs_sf$label_y,
    stringsAsFactors = FALSE
  )
} else {
  cent <- suppressWarnings(st_centroid(contribs_sf))
  cc <- st_coordinates(cent)
  pts_all <- data.frame(
    country      = contribs_sf$name,
    contributors = contribs_sf$contributors,
    lon = cc[, 1], lat = cc[, 2], stringsAsFactors = FALSE
  )
}
ov_i <- match(centroid_overrides$country, pts_all$country)
ok   <- !is.na(ov_i)
pts_all$lon[ov_i[ok]] <- centroid_overrides$lon[ok]
pts_all$lat[ov_i[ok]] <- centroid_overrides$lat[ok]

# Assign each contributor country to whichever hemisphere centre is
# angularly closest, so it shows up in exactly one panel.
rad <- pi / 180
ang_dist <- function(lon0, lat0, lon, lat) {
  cos_d <- sin(lat0 * rad) * sin(lat * rad) +
           cos(lat0 * rad) * cos(lat * rad) * cos((lon - lon0) * rad)
  acos(pmin(pmax(cos_d, -1), 1))
}
d1 <- ang_dist(hemispheres[[1]]$lon0, hemispheres[[1]]$lat0,
               pts_all$lon, pts_all$lat)
d2 <- ang_dist(hemispheres[[2]]$lon0, hemispheres[[2]]$lat0,
               pts_all$lon, pts_all$lat)
pts_all$panel <- ifelse(d1 <= d2, hemispheres[[1]]$label, hemispheres[[2]]$label)

pts_by_panel <- split(pts_all, pts_all$panel)

# ---------------------------------------------------------------------------
# 6. Render both hemispheres and stitch side by side with magick.
# ---------------------------------------------------------------------------
message("Rendering hemispheres...")
tmp_files <- character()
for (i in seq_along(hemispheres)) {
  h   <- hemispheres[[i]]
  pts <- pts_by_panel[[h$label]]
  if (is.null(pts)) pts <- pts_all[0, ]
  # Only the last panel gets the legend (to avoid duplication), and only
  # when the caller has actually asked for one.
  panel_show_legend <- show_legend && (i == length(hemispheres))
  p <- render_hemisphere(h$lon0, h$lat0, world, contribs_pts = pts,
                         label = h$label,
                         show_legend = panel_show_legend)
  tf <- tempfile(fileext = ".png")
  ggsave(tf, plot = p, width = 6.5, height = 6.5, dpi = 220, bg = bg_navy)
  tmp_files <- c(tmp_files, tf)
}

# Stitch with a small navy strip in between so the two globes read as
# distinct spheres rather than merging.
imgs   <- lapply(tmp_files, image_read)
gutter <- image_blank(width = 40,
                     height = image_info(imgs[[1]])$height,
                     color = bg_navy)
stitched <- image_append(c(imgs[[1]], gutter, imgs[[2]]))
image_write(stitched, path = out_path, format = "png")

message(sprintf("Wrote %s  (%dx%d)", out_path,
                image_info(stitched)$width,
                image_info(stitched)$height))
invisible(file.remove(tmp_files))
