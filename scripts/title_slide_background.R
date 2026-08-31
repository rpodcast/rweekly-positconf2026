# title_slide_background.R
# Generate a title-slide background image for the R Weekly lightning talk.
#
# Concept: "Constellation of contributors"
#   - Dark navy "night sky" canvas at 1920x1080
#   - One star per unique R Weekly PR contributor (406 of them), scattered
#     across the canvas with varied sizes/brightness. Larger/brighter stars
#     correspond to more-prolific contributors.
#   - Star colours sampled from the R Weekly logo palette (teal / green /
#     yellow / orange / magenta / lime).
#   - The R Weekly logo sits large on the right side, with a soft glow.
#   - A subset of contributor stars is connected back to the logo with
#     very-low-opacity lines, evoking a constellation.
#   - Left third of the canvas is intentionally sparse so title/subtitle
#     text has room to breathe.
#
# Deterministic: fixed RNG seed so the layout is reproducible.
#
# Output: assets/img/title_slide_background.png

suppressPackageStartupMessages({
  library(magick)
})

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
project_dir <- "/mnt/nvme2/r_projects/rweekly-positconf2026"
img_dir     <- file.path(project_dir, "assets", "img")
data_dir    <- file.path(project_dir, "data")

logo_path   <- file.path(img_dir, "icon-512x512.png")
contrib_csv <- file.path(data_dir, "all_pr_contributors_ranked.csv")
out_path    <- file.path(img_dir, "title_slide_background.png")

W <- 1920L
H <- 1080L

# Logo placement (constants also reused by the SCSS text bounds)
logo_cx      <- 1560L   # 81% across of 1920
logo_cy      <- 540L
logo_size_px <- 420L    # slightly smaller so the title has room to breathe

set.seed(20260830)  # reproducible layout

# Colours pulled from the R Weekly logo's star palette
star_palette <- c(
  "#2CC5B0",  # teal
  "#3EDB9B",  # mint green
  "#F2C438",  # yellow
  "#F09030",  # orange
  "#B540B0",  # magenta
  "#7ED957",  # lime
  "#2E8B36"   # deep green
)

bg_top    <- "#0A1A3A"   # deep navy
bg_mid    <- "#0B1224"   # midnight
bg_bottom <- "#050813"   # near-black
line_col  <- "#4A6FA5"   # faint dusty blue for constellation lines

# ---------------------------------------------------------------------------
# Load contributors -> star weights
# ---------------------------------------------------------------------------
contribs <- read.csv(contrib_csv, stringsAsFactors = FALSE)
# Weight each contributor's star by PR count, but compress the dynamic range
# heavily so the top contributor doesn't visually dominate.
w <- contribs$merged_prs
star_size <- 6 + 10 * (log1p(w) / log1p(max(w)))^1.5   # px radius, roughly 6-16
star_alpha <- 0.55 + 0.45 * (log1p(w) / log1p(max(w)))  # 0.55 - 1.0
n_stars <- nrow(contribs)

message(sprintf("Building constellation of %d contributor stars.", n_stars))

# ---------------------------------------------------------------------------
# Star positions
# ---------------------------------------------------------------------------
# Bias positions to leave the title area (roughly the left half, upper 2/3)
# less dense. Do this by rejection sampling with a per-region acceptance rate.
title_area <- function(x, y) {
  # rectangle from (0, 100) to (1250, 800) is the "title area" - widened
  # to match the SCSS h2/h4 max-width so long titles have breathing room.
  x >= 0 & x <= 1250 & y >= 100 & y <= 800
}

# The right portion is where the logo will sit; keep it clear.
logo_area <- function(x, y) {
  ((x - logo_cx)^2 + (y - logo_cy)^2) < (240^2)
}

sample_positions <- function(n) {
  xs <- numeric(0); ys <- numeric(0)
  while (length(xs) < n) {
    need <- n - length(xs)
    # Oversample; accept/reject
    cx <- runif(need * 3, 0, W)
    cy <- runif(need * 3, 0, H)

    keep <- rep(TRUE, length(cx))
    # Down-weight title area (accept 30% of candidates that fall there)
    in_title <- title_area(cx, cy)
    keep[in_title] <- runif(sum(in_title)) < 0.30
    # Forbid the logo area entirely
    keep[logo_area(cx, cy)] <- FALSE

    cx <- cx[keep]; cy <- cy[keep]
    take <- min(length(cx), need)
    if (take > 0) {
      xs <- c(xs, cx[seq_len(take)])
      ys <- c(ys, cy[seq_len(take)])
    }
  }
  list(x = xs, y = ys)
}

pos <- sample_positions(n_stars)

star_colour <- sample(star_palette, n_stars, replace = TRUE)

# ---------------------------------------------------------------------------
# Build SVG for stars + constellation lines
# ---------------------------------------------------------------------------
# Five-pointed star polygon points, centred at (cx, cy) with outer radius r.
star_points <- function(cx, cy, r) {
  # Ratio between outer and inner points of a five-pointed star
  ir <- r * 0.42
  # Start at top
  angles <- seq(-pi/2, -pi/2 + 2*pi, length.out = 11)  # 10 points + close
  radii  <- rep(c(r, ir), length.out = 11)
  x <- cx + radii * cos(angles)
  y <- cy + radii * sin(angles)
  paste(sprintf("%.1f,%.1f", x, y), collapse = " ")
}

# Pick a subset of contributor stars to connect to the logo — enough to
# suggest a constellation without cluttering. Weight by PR count so the
# most-active contributors are more likely to be linked.
n_lines <- 55L
line_idx <- sample.int(n_stars, n_lines, prob = w / sum(w))
line_svg <- paste(sprintf(
  '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="1" stroke-opacity="0.15"/>',
  pos$x[line_idx], pos$y[line_idx], logo_cx, logo_cy, line_col
), collapse = "\n")

star_svg <- paste(vapply(seq_len(n_stars), function(i) {
  sprintf(
    '<polygon points="%s" fill="%s" fill-opacity="%.2f"/>',
    star_points(pos$x[i], pos$y[i], star_size[i]),
    star_colour[i], star_alpha[i]
  )
}, character(1)), collapse = "\n")

# Small extra "twinkle" halos on the very brightest stars for atmosphere
brightest <- order(-w)[seq_len(15)]
twinkle_svg <- paste(vapply(brightest, function(i) {
  sprintf(
    '<circle cx="%.1f" cy="%.1f" r="%.1f" fill="%s" fill-opacity="0.10"/>',
    pos$x[i], pos$y[i], star_size[i] * 2.4, star_colour[i]
  )
}, character(1)), collapse = "\n")

svg_doc <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">
  <defs>
    <radialGradient id="bg" cx="60%%" cy="45%%" r="80%%">
      <stop offset="0%%" stop-color="%s"/>
      <stop offset="55%%" stop-color="%s"/>
      <stop offset="100%%" stop-color="%s"/>
    </radialGradient>
    <radialGradient id="logoGlow" cx="50%%" cy="50%%" r="50%%">
      <stop offset="0%%"  stop-color="#FFFFFF" stop-opacity="0.18"/>
      <stop offset="60%%" stop-color="#FFFFFF" stop-opacity="0.04"/>
      <stop offset="100%%" stop-color="#FFFFFF" stop-opacity="0"/>
    </radialGradient>
  </defs>
  <rect width="100%%" height="100%%" fill="url(#bg)"/>
  %s
  %s
  %s
  <circle cx="%d" cy="%d" r="300" fill="url(#logoGlow)"/>
</svg>',
  W, H, W, H,
  bg_top, bg_mid, bg_bottom,
  line_svg,      # behind stars so stars sit on top
  twinkle_svg,
  star_svg,
  logo_cx, logo_cy
)

svg_path <- tempfile(fileext = ".svg")
writeLines(svg_doc, svg_path)

# ---------------------------------------------------------------------------
# Rasterise and composite the R Weekly logo on top
# ---------------------------------------------------------------------------
message("Rasterising SVG canvas ...")
# image_read() dispatches on file extension; magick's librsvg backend renders
# the SVG at the canvas dimensions we declared in the <svg> tag.
canvas <- image_read(svg_path)
# Force to expected pixel dimensions (defensive; SVG should already be W x H)
canvas <- image_resize(canvas, sprintf("%dx%d!", W, H))

logo <- image_read(logo_path)
logo <- image_scale(logo, sprintf("%dx%d", logo_size_px, logo_size_px))

# Composite the logo, centred on the same coordinates as the halo
logo_info <- image_info(logo)
offset_x <- logo_cx - as.integer(logo_info$width  / 2)
offset_y <- logo_cy - as.integer(logo_info$height / 2)

final <- image_composite(canvas, logo, offset = sprintf("+%d+%d", offset_x, offset_y))

image_write(final, path = out_path, format = "png")
message(sprintf("Wrote %s  (%dx%d)", out_path, W, H))

# Also record the parameters that produced this render, for reproducibility
params <- list(
  seed = 20260830,
  n_stars = n_stars,
  n_lines = n_lines,
  logo_center = c(logo_cx, logo_cy),
  logo_size_px = logo_size_px,
  palette = star_palette,
  bg = c(bg_top, bg_mid, bg_bottom)
)
saveRDS(params, file.path(data_dir, "title_slide_params.rds"))
