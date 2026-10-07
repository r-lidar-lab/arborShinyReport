# ---- Brand palette (same as the html report) --------------------------------
brand_green   <- "#1C6B3C"
brand_green_d <- "#123F23"
accent_amber  <- "#C77B3B"
ink           <- "#2B2B28"
mid_gray      <- "#6B6963"

app_css <- "
.stat-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(150px,1fr));gap:12px;margin:12px 0}
.stat-card{border:1px solid #e3e3df;border-left:4px solid #1C6B3C;border-radius:6px;padding:10px 12px;background:#fff}
.stat-label{font-size:11px;text-transform:uppercase;letter-spacing:.04em;color:#6B6963;font-weight:600}
.stat-value{font-size:22px;font-weight:700;color:#123F23}
.stat-desc{font-size:11px;color:#6B6963}
.callout{border-left:4px solid #1C6B3C;background:#F1F6F2;padding:10px 14px;border-radius:4px;margin:10px 0}
.callout-warning{border-left:4px solid #C0392B;background:#FBEAEA;color:#7A1F1F;padding:12px 16px;border-radius:4px;margin-bottom:14px}
.viewer-panel{position:sticky;top:10px;border:1px solid #e3e3df;border-radius:6px;padding:10px;background:#fff}
.viewer-panel h4,.well h4{margin-top:0;color:#123F23}
.nav-tabs>li.active>a{color:#1C6B3C!important;font-weight:600}
"

`%||%` <- function(a, b) if (is.null(a)) b else a

stat_card <- function(label, value, desc = NULL) {
  div(class = "stat-card",
      div(class = "stat-label", label),
      div(class = "stat-value", value),
      if (!is.null(desc)) div(class = "stat-desc", desc))
}

# ---- Accurate NAD27 -> WGS84 with a safety net -------------------------------
qc_bbox_wgs84 <- c(xmin = -80, xmax = -55, ymin = 44, ymax = 63)

safe_transform_to_wgs84 <- function(x) {
  transform_and_check <- function(x) {
    xy <- sf::st_coordinates(sf::st_transform(x, 4326))
    ok <- xy[, 1] >= qc_bbox_wgs84["xmin"] & xy[, 1] <= qc_bbox_wgs84["xmax"] &
      xy[, 2] >= qc_bbox_wgs84["ymin"] & xy[, 2] <= qc_bbox_wgs84["ymax"]
    list(xy = xy, ok = all(ok))
  }
  tryCatch(sf::sf_proj_network(TRUE), error = function(e) invisible(NULL))
  res <- transform_and_check(x)
  if (!res$ok) {
    warning("Network datum grid produced points outside Quebec - falling back ",
            "to the local (less accurate) transformation.", call. = FALSE)
    sf::sf_proj_network(FALSE)
    res <- transform_and_check(x)
    if (!res$ok)
      warning("Points are still outside Quebec: the source X/Y are probably not ",
              "real EPSG:32098 coordinates.", call. = FALSE)
  }
  res$xy
}
