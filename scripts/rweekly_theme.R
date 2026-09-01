# rweekly_theme.R
# ------------------------------------------------------------------
# A ggplot2 theme + palette designed to sit on the deep-navy Reveal
# slide background used throughout this presentation (see styles.scss).
#
# Design goals:
#   * Plot panel is *slightly* lighter than the slide background so
#     the chart reads as a distinct object without a jarring bright
#     rectangle.
#   * Text and grid lines are muted enough to fade into the slide,
#     but bright enough to remain legible from the back of a room.
#   * Discrete + continuous colour scales pull from the R Weekly
#     accent palette defined in styles.scss.
#
# Usage:
#   source("scripts/rweekly_theme.R")
#   ggplot(df, aes(x, y)) + geom_col(fill = rweekly_colors$teal) +
#     theme_rweekly()
#
#   # Discrete colour / fill scales that match the palette:
#   + scale_color_rweekly()
#   + scale_fill_rweekly()
#
#   # To make ggsave() output blend into slides, save with a
#   # transparent background:
#   ggsave("plot.png", bg = "transparent")
# ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
})

# ---- Palette (mirrors styles.scss) --------------------------------
rweekly_colors <- list(
  navy       = "#0F1D3D",  # slide background
  navy_deep  = "#0A1530",  # slightly darker accent
  panel      = "#182A55",  # a touch lighter than navy for plot panel
  teal       = "#2CC5B0",
  mint       = "#3EDB9B",
  yellow     = "#F2C438",
  orange     = "#F09030",
  magenta    = "#B540B0",
  text       = "#F5F7FB",
  muted      = "#B8C2D9",
  grid       = "#2A3B66"   # low-contrast grid line on navy panel
)

# Ordered accent palette for discrete scales.
rweekly_palette <- c(
  rweekly_colors$teal,
  rweekly_colors$yellow,
  rweekly_colors$mint,
  rweekly_colors$orange,
  rweekly_colors$magenta,
  rweekly_colors$muted
)

# ---- Theme --------------------------------------------------------
#' R Weekly ggplot2 theme
#'
#' @param base_size Base font size in points. Defaults to 14 which
#'   reads well when a plot is rendered at slide size.
#' @param base_family Base font family. NULL uses the ggplot2 default.
#' @param transparent If TRUE (default), plot + panel backgrounds are
#'   fully transparent so the slide navy shows through. Set FALSE to
#'   render a self-contained image with the navy background baked in.
theme_rweekly <- function(base_size = 14,
                          base_family = NULL,
                          transparent = TRUE) {
  panel_fill <- if (transparent) NA else rweekly_colors$panel
  plot_fill  <- if (transparent) NA else rweekly_colors$navy

  theme_minimal(base_size = base_size, base_family = base_family %||% "") %+replace%
    theme(
      # Backgrounds
      plot.background  = element_rect(fill = plot_fill,  colour = NA),
      panel.background = element_rect(fill = panel_fill, colour = NA),
      legend.background = element_rect(fill = NA, colour = NA),
      legend.key        = element_rect(fill = NA, colour = NA),

      # Text: near-white for titles, muted for secondary
      text = element_text(colour = rweekly_colors$text),
      plot.title = element_text(
        colour = rweekly_colors$yellow,
        face   = "bold",
        size   = rel(1.15),
        hjust  = 0,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        colour = rweekly_colors$muted,
        size   = rel(0.9),
        hjust  = 0,
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        colour = rweekly_colors$muted,
        size   = rel(0.75),
        hjust  = 1
      ),

      axis.title  = element_text(colour = rweekly_colors$muted, size = rel(0.9)),
      axis.text   = element_text(colour = rweekly_colors$muted, size = rel(0.85)),
      axis.ticks  = element_line(colour = rweekly_colors$grid, linewidth = 0.3),
      axis.line   = element_blank(),

      # Grid: only major, very low contrast so it whispers on the slide
      panel.grid.major = element_line(colour = rweekly_colors$grid, linewidth = 0.3),
      panel.grid.minor = element_blank(),

      # Legend
      legend.title = element_text(colour = rweekly_colors$muted, size = rel(0.9)),
      legend.text  = element_text(colour = rweekly_colors$text,  size = rel(0.85)),
      legend.position = "top",
      legend.justification = "left",

      # Strip (facets)
      strip.background = element_rect(fill = rweekly_colors$navy_deep, colour = NA),
      strip.text = element_text(
        colour = rweekly_colors$text,
        face   = "bold",
        size   = rel(0.9),
        margin = margin(t = 4, b = 4)
      ),

      plot.margin = margin(t = 10, r = 12, b = 8, l = 8)
    )
}

# Small helper so theme_rweekly() works without importing rlang.
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Scales -------------------------------------------------------
.rweekly_palette_fn <- function(n) {
  if (n > length(rweekly_palette)) {
    warning("rweekly palette has ", length(rweekly_palette),
            " colors; ", n, " requested. Colors will be recycled.")
    rep_len(rweekly_palette, n)
  } else {
    rweekly_palette[seq_len(n)]
  }
}

#' Discrete colour scale using the R Weekly accent palette.
scale_color_rweekly <- function(...) {
  ggplot2::discrete_scale("colour", palette = .rweekly_palette_fn, ...)
}

#' Discrete fill scale using the R Weekly accent palette.
scale_fill_rweekly <- function(...) {
  ggplot2::discrete_scale("fill", palette = .rweekly_palette_fn, ...)
}

# UK-spelling aliases
scale_colour_rweekly <- scale_color_rweekly

# ---- Register as default (optional) -------------------------------
# Uncomment to make every ggplot in the session pick up the theme:
theme_set(theme_rweekly())
