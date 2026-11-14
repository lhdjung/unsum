# This script draws man/figures/logo.png; run from the package root. It needs a
# TeX distribution, for the font Latin Modern Math, and unsum itself.
#
# A sigma, the sign for a sum, broken into three pieces: un-summed. It is the
# \sum of LaTeX. Its middle piece has become the "s" in "unsum". The letters are
# one tube bent into shape: the "n" is the "u" turned upside down, and the "m"
# is two "n"s that share a stem.
#
# Behind them are samples that unsum finds from summary statistics, each drawn
# as a small histogram.

gold <- "#f2c94c"
border <- "#5D3FD3" # CLOSURE purple

# The background is a very dark shade of the border's hue, and the histograms
# are a slightly lighter one, so they read as texture behind the gold.
sky <- "#16122f"
bars <- "#2a2457"

pts <- function(x, y) paste(sprintf("%.1f,%.1f", x, y), collapse = " ")
polygon <- function(x, y) sprintf('<polygon points="%s"/>', pts(x, y))

# The word. Letters sit between y_top and y_bot and are w wide, measured along
# the middle of the tube.
sw <- 20
y_top <- 270
y_bot <- 330
w <- 48
r <- w / 2

u <- function(x) {
  sprintf(
    '<path d="M %g,%g V %g A %g,%g 0 0 0 %g,%g V %g"/>',
    x,
    y_top,
    y_bot - r,
    r,
    r,
    x + w,
    y_bot - r,
    y_top
  )
}

n <- function(x) {
  sprintf(
    '<g transform="rotate(180 %g %g)">%s</g>',
    x + r,
    (y_top + y_bot) / 2,
    u(x)
  )
}

# The s is two half-height bowls, joined by straight strokes.
s <- function(x) {
  q <- (y_bot - y_top) / 4
  sprintf(
    paste(
      '<path d="M %g,%g H %g A %g,%g 0 0 0 %g,%g H %g',
      'A %g,%g 0 0 1 %g,%g H %g"/>'
    ),
    x + w,
    y_top,
    x + q,
    q,
    q,
    x + q,
    y_top + 2 * q,
    x + w - q,
    q,
    q,
    x + w - q,
    y_bot,
    x
  )
}

# The letters are evenly spaced, and the word is centered. Each x is a letter's
# left edge, measured along the middle of the tube.
pitch <- w + sw + 12
x <- 260 - (4 * pitch + 2 * w) / 2 + pitch * 0:4

letters <- c(
  sprintf(
    paste(
      '<g fill="none" stroke="%s" stroke-width="%g"',
      'stroke-linecap="round" stroke-linejoin="round">'
    ),
    gold,
    sw
  ),
  u(x[1]),
  n(x[2]),
  s(x[3]),
  u(x[4]),
  n(x[5]),
  n(x[5] + w),
  "</g>"
)

# The sigma, 260 units tall and centered. Its middle is missing and the s takes
# its place.
font <- system2("kpsewhich", "latinmodern-math.otf", stdout = TRUE)
glyph <- systemfonts::glyph_info("∑", path = font)$index
outline <- systemfonts::glyph_outline(glyph, font, size = 260)
sigma <- polygon(outline$x + 123, 365 - outline$y)

# The other two pieces broke off along jagged cracks well above and below the
# word. They float askew, turned away from the word.
crack <- function(x, y) list(x = x + 10 * 0:4, y = y + c(0, 4, -3, 4, 0))

piece <- function(id, x, y, transform) {
  sprintf(
    paste(
      '<clipPath id="%s"><polygon points="%s"/></clipPath>',
      '<g transform="%s"><g clip-path="url(#%s)">%s</g></g>'
    ),
    id,
    pts(x, y),
    transform,
    id,
    sigma
  )
}

k1 <- crack(178, 244)
k2 <- crack(186, 356)

pieces <- c(
  sprintf('<g fill="%s">', gold),
  piece(
    "top",
    c(0, k1$x, 520, 520, 0),
    c(244, k1$y, 244, 0, 0),
    "translate(-10,-10) rotate(7 263 205)"
  ),
  piece(
    "bottom",
    c(0, k2$x, 520, 520, 0),
    c(356, k2$y, 356, 600, 600),
    "translate(-4,12) rotate(-5 268 400)"
  ),
  "</g>"
)

# Samples of 20 ratings from 1 to 5 with a mean of 3.0, found by CLOSURE for
# every SD from 0.2 to 2.0. (With an SD of 0.1, there are none.)
pool <- seq(0.2, 2, by = 0.1) |>
  sprintf(fmt = "%.1f") |>
  lapply(\(sd) unsum::closure_generate("3.0", sd, 20, 1, 5)@results) |>
  do.call(what = rbind) |>
  suppressMessages()

# They are shown as a grid of histograms. Each row has the samples whose horns
# index is closest to a target that rises from bottom to top. At the vertical
# center of the logo, the target is the horns index of a uniform distribution,
# so the histograms run from peaked at the bottom through flat in the middle to
# horned at the top.
rows <- 0:11
cols <- 0:8
row_mid <- 24 + 50 * rows
target <- unsum::horns_uniform(1, 5) + (300 - row_mid) * 0.44 / (300 - 24)

picks <- integer()
for (t in target) {
  nearest <- setdiff(order(abs(pool$horns - t)), picks)
  picks <- c(picks, nearest[seq_along(cols)])
}

grid <- expand.grid(col = cols, row = rows)
samples <- pool$sample[picks]

histogram <- function(i) {
  counts <- tabulate(samples[[i]], 5)
  sprintf(
    '<rect x="%.1f" y="%.1f" width="8" height="%.1f"/>',
    6 + 58 * grid$col[i] + 10 * 0:4,
    46 + 50 * grid$row[i] - 2.2 * counts,
    2.2 * counts
  )
}

histograms <- samples |>
  seq_along() |>
  lapply(histogram) |>
  unlist()

hexs <- pts(
  c(260, 511.1, 511.1, 260, 8.9, 8.9),
  c(10, 155, 445, 590, 445, 155)
)

svg <- c(
  paste(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 520 600"',
    'width="520" height="600">'
  ),
  sprintf('<clipPath id="hex"><polygon points="%s"/></clipPath>', hexs),
  '<g clip-path="url(#hex)">',
  sprintf('<rect width="520" height="600" fill="%s"/>', sky),
  sprintf('<g fill="%s">', bars),
  histograms,
  "</g>",
  "</g>",
  pieces,
  letters,
  sprintf(
    paste(
      '<polygon points="%s" fill="none" stroke="%s" stroke-width="16"',
      'stroke-linejoin="round"/>'
    ),
    hexs,
    border
  ),
  "</svg>"
)

# Write logo to disk
svg_path <- tempfile(fileext = ".svg")
writeLines(svg, svg_path)
rsvg::rsvg_png(svg_path, "man/figures/logo.png", width = 1040)
