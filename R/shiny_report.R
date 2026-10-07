#' Interactive Forest Inventory Report
#'
#' Interactive Shiny version of the "Forest Inventory Report". Selecting a tree
#' anywhere (plots, leaflet tree map, table, search box) selects it everywhere
#' and renders it in the WebGL (rgl) viewer.
#'
#' The "Height vs Diameter" plot shows the allometric fit
#' \eqn{H = \exp(b_0) (DBH \times 100)^{b_1}} (see [fit_height_dbh()]) with its
#' R2 and RMSE. The fit is recomputed on the trees that pass the current
#' filters.
#'
#' @param qsf a `qsf` object (`arbor::qsf_read()`), or a path to a `.qsf` file
#'   or to a directory containing `.qsf` files.
#' @param las (optional) a `lidR::LAS` with a `treeID` attribute, or a path to
#'   one. It is never drawn as a whole: only the points of the selected tree
#'   are rendered in the 3D viewer.
#' @param target_epsg optional EPSG code to assign/transform the tree map to
#'   (`0` = keep).
#'
#' @return A `shiny.appobj`. Print it (or pass it to `shiny::runApp()`) to
#'   launch the report.
#' @examples
#' \dontrun{
#' las <- lidR::readLAS("trees.laz")
#' sf::st_crs(las) <- 26917
#' qsf <- arbor::qsf(las)
#' shiny_report(qsf, las)
#' }
#' @export
shiny_report <- function(qsf, las = NULL, target_epsg = 0) {

  # ---- 1. Read inputs --------------------------------------------------------
  if (is.character(qsf)) {
    if (dir.exists(qsf)) {
      f <- list.files(qsf, pattern = "qsf", recursive = TRUE, full.names = TRUE)
      if (length(f) == 0) stop("No .qsf file detected in this directory")
      qsf <- do.call(c, lapply(f, arbor::qsf_read))
      qsf <- arbor:::as_qsf(qsf)
    } else if (file.exists(qsf)) {
      qsf <- arbor::qsf_read(qsf)
    } else stop("Invalid file or directory")
  }

  if (is.character(las)) las <- lidR::readLAS(las)
  if (!is.null(las) && !"treeID" %in% names(las@data))
    stop("`las` must have a 'treeID' attribute")

  # ---- 2. Tree table ---------------------------------------------------------
  ans <- arbor::qsf_treemap(qsf)
  has_crs <- !is.na(sf::st_crs(ans))
  if (target_epsg > 0) {
    if (!has_crs) sf::st_crs(ans) <- target_epsg
    else ans <- sf::st_transform(ans, target_epsg)
  }
  has_crs <- !is.na(sf::st_crs(ans))
  ans <- dplyr::rename(ans, name = "Name", volume = "V", height = "H", dbh = "DBH")

  xy_local <- sf::st_coordinates(ans)
  raw <- sf::st_drop_geometry(ans)
  msg <- ifelse(is.na(raw$Message), "OK", as.character(raw$Message))

  status_levels <- c("OK", sort(setdiff(unique(msg), "OK")))
  status_colors <- stats::setNames(
    c(brand_green, scales::hue_pal()(max(length(status_levels) - 1, 0))),
    status_levels
  )

  df <- data.frame(
    name    = as.character(raw$name),
    volume  = raw$volume,
    height  = raw$height,
    dbh     = raw$dbh,                 # metres
    dbh_cm  = round(raw$dbh * 100, 1),
    Message = factor(msg, levels = status_levels),
    x = xy_local[, 1], y = xy_local[, 2],
    stringsAsFactors = FALSE
  )
  if (has_crs) {
    ll <- safe_transform_to_wgs84(ans)
    df$lon <- ll[, 1]
    df$lat <- ll[, 2]
  }

  area <- as.numeric(sf::st_area(sf::st_as_sfc(sf::st_bbox(ans))))
  if (round(area / 50) * 50 > 0) area <- round(area / 50) * 50
  Vtot <- sum(df$volume)

  # Data-quality banner (computed on the full, unfiltered data)
  novalid_pct <- 100 * sum(df$volume[df$Message == "No valid measure"]) / Vtot
  show_warning <- isTRUE(novalid_pct > 20)

  # Slider limits, rounded outward so no tree is lost to step rounding
  slider_lim <- function(x) {
    r <- range(x, na.rm = TRUE)
    if (diff(r) == 0) r <- r + c(-0.5, 0.5)
    step <- signif(diff(r) / 500, 2)
    list(lim = c(floor(r[1] / step) * step, ceiling(r[2] / step) * step), step = step)
  }
  vol_s <- slider_lim(df$volume)
  dbh_s <- slider_lim(df$dbh_cm)

  # Pseudo-pixel radius for the (CRS-less) fallback map
  px_r <- function(dbh) 3 + 12 * (dbh - min(df$dbh)) / max(diff(range(df$dbh)), 1e-9)

  hover_text <- function(d) sprintf(
    "Tree: %s<br>Status: %s<br>DBH: %.1f cm<br>Height: %.2f m<br>Volume: %.3f m\u00B3",
    d$name, as.character(d$Message), d$dbh_cm, d$height, d$volume)

  # 3D viewer size (was 4/12 columns and 460 px)
  viewer_height <- "650px"

  # ===========================================================================
  # UI
  # ===========================================================================
  ui <- fluidPage(
    tags$head(tags$style(HTML(app_css))),
    titlePanel(div(style = paste0("color:", brand_green_d),
                   "Forest Inventory Report",
                   tags$small(style = paste0("color:", mid_gray), " interactive"))),

    if (show_warning)
      div(class = "callout-warning",
          strong("\u26A0 Data quality warning \u2014 "),
          sprintf(paste0("%.1f%% of total stand volume comes from trees flagged ",
                         "\u201cNo valid measure\u201d by the QSM reconstruction ",
                         "(threshold: 20%%). Interpret volume, carbon and allometric ",
                         "results with caution."), novalid_pct)),

    sidebarLayout(
      # ---- Left: filters + selection -----------------------------------------
      sidebarPanel(
        width = 2,
        h4("Filters"),
        sliderInput("vol_range", "Tree volume (m\u00B3)",
                    min = vol_s$lim[1], max = vol_s$lim[2],
                    value = vol_s$lim, step = vol_s$step),
        sliderInput("dbh_range", "DBH (cm)",
                    min = dbh_s$lim[1], max = dbh_s$lim[2],
                    value = dbh_s$lim, step = dbh_s$step),
        checkboxGroupInput("status", "Status",
                           choices = status_levels, selected = status_levels),
        hr(),
        h4("Selected tree"),
        selectizeInput("tree_pick", NULL, choices = NULL,
                       options = list(placeholder = "Search a tree...")),
        uiOutput("selected_info"),
        actionButton("clear_sel", "Clear selection", class = "btn-sm"),
        hr(),
        h4("Export"),
        downloadButton("dl_csv", "CSV", class = "btn-sm"),
        downloadButton("dl_gpkg", "GeoPackage", class = "btn-sm"),
        tags$p(class = "help-block", "Exports respect the current filters.")
      ),

      # ---- Right: tabs + 3D viewer -------------------------------------------
      mainPanel(
        width = 10,
        fluidRow(
          column(7,
                 tabsetPanel(
                   id = "main_tabs",

                   tabPanel("Overview",
                            h4("Global statistics"),
                            uiOutput("stat_cards"),
                            h4("Location"),
                            if (has_crs) leafletOutput("world_map", height = 420)
                            else div(class = "callout",
                                     strong("Cannot display data location \u2014 "),
                                     "the data has no coordinate reference system (CRS).")
                   ),

                   tabPanel("Tree map",
                            p("Each tree's diameter footprint is drawn at 2x scale. ",
                              "Click a tree to select it."),
                            radioButtons("color_by", "Color by", inline = TRUE,
                                         choices = c("Volume", "Height", "Status")),
                            leafletOutput("treemap", height = 560)
                   ),

                   tabPanel("Allometrics",
                            p("Click a point (or a table row) to select that tree everywhere."),
                            tabsetPanel(
                              tabPanel("Volume vs Diameter", plotlyOutput("plot_vol", height = 460)),
                              tabPanel("Height vs Diameter",
                                       p(class = "help-block",
                                         "Fit: H = exp(b0) \u00D7 DBH^b1 only on trees with valid QSM"),
                                       plotlyOutput("plot_height", height = 460)),
                              tabPanel("Data table", br(), DTOutput("tbl"))
                            )
                   ),

                   tabPanel("Distributions",
                            tabsetPanel(
                              tabPanel("Diameter", plotlyOutput("hist_dbh", height = 420)),
                              tabPanel("Height",   plotlyOutput("hist_height", height = 420))
                            )
                   ),

                   tabPanel("Contributions",
                            uiOutput("contrib_text"),
                            p("Click a point to select the tree."),
                            plotlyOutput("plot_contrib", height = 460)
                   ),

                   tabPanel("Quality & carbon",
                            h4("QSM quality (share of volume by status)"),
                            fluidRow(
                              column(6, plotlyOutput("qsm_donut", height = 380)),
                              column(6, br(), tableOutput("qsm_table"))
                            ),
                            hr(),
                            h4("Carbon estimate"),
                            p("Conversion factor (Mg C per m\u00B3 of wood volume) depends on species, ",
                              "wood density and carbon fraction. Adjust it for your species/region."),
                            sliderInput("carbon_factor", "Conversion factor (Mg C / m\u00B3)",
                                        min = 0.05, max = 1, value = 0.5, step = 0.01, width = "100%"),
                            uiOutput("carbon_cards")
                   )
                 )
          ),
          column(5,
                 div(class = "viewer-panel",
                     h4("Tree 3D view"),
                     conditionalPanel("output.has_sel",
                                      rgl::rglwidgetOutput("view3d", width = "100%",
                                                           height = viewer_height)),
                     conditionalPanel("!output.has_sel",
                                      p(style = paste0("color:", mid_gray),
                                        "Select a tree in any plot, map or table to inspect it in 3D.",
                                        if (is.null(las)) " (No point cloud supplied: only the QSM is shown.)"))
                 )
          )
        )
      )
    )
  )

  # ===========================================================================
  # Server
  # ===========================================================================
  server <- function(input, output, session) {

    # ---- Shared selection state --------------------------------------------
    selected <- reactiveVal(NULL)
    select_tree <- function(id) {
      id <- as.character(id)[1]
      if (!is.na(id) && id %in% df$name) selected(id)
    }

    output$has_sel <- reactive(!is.null(selected()))
    outputOptions(output, "has_sel", suspendWhenHidden = FALSE)

    updateSelectizeInput(session, "tree_pick", choices = df$name, server = TRUE)
    observeEvent(selected(), {
      updateSelectizeInput(session, "tree_pick", choices = df$name,
                           selected = selected() %||% "", server = TRUE)
    }, ignoreNULL = FALSE)
    observeEvent(input$tree_pick, {
      if (nzchar(input$tree_pick)) select_tree(input$tree_pick)
    })
    observeEvent(input$clear_sel, selected(NULL))

    # ---- Filtered data -----------------------------------------------------
    trees <- reactive({
      req(input$vol_range, input$dbh_range)
      tol <- 1e-9
      keep <- df$volume >= input$vol_range[1] - tol & df$volume <= input$vol_range[2] + tol &
        df$dbh_cm >= input$dbh_range[1] - tol & df$dbh_cm <= input$dbh_range[2] + tol &
        df$Message %in% input$status
      df[which(keep), , drop = FALSE]
    })
    need_trees <- function(d) validate(need(nrow(d) > 0, "No trees match the current filters"))

    # ---- Selected-tree card -------------------------------------------------
    output$selected_info <- renderUI({
      id <- selected()
      if (is.null(id)) return(p(style = paste0("color:", mid_gray), "None"))
      r <- df[match(id, df$name), ]
      div(class = "callout", style = "font-size:12px;",
          strong(paste("Tree", r$name)), br(),
          sprintf("Volume: %.3f m\u00B3", r$volume), br(),
          sprintf("Height: %.2f m", r$height), br(),
          sprintf("DBH: %.1f cm", r$dbh_cm), br(),
          paste("Status:", as.character(r$Message)))
    })

    # ---- Global statistics --------------------------------------------------
    output$stat_cards <- renderUI({
      d <- trees(); n <- nrow(d)
      ba <- sum(pi * (d$dbh / 2)^2)
      div(class = "stat-grid",
          stat_card("Trees", n, "Count of individual trees"),
          stat_card("Area", paste(round(area, 1), "m\u00B2"), "Fixed surveyed footprint"),
          stat_card("Density", sprintf("%.2f / ha", n / area * 1e4), "Trees per hectare"),
          stat_card("Basal area", sprintf("%.2f m\u00B2", ba), "Cross-sectional area at breast height"),
          stat_card("Basal area / ha", sprintf("%.2f m\u00B2/ha", ba / area * 1e4), "Basal area per hectare"),
          stat_card("Total volume", sprintf("%.2f m\u00B3", sum(d$volume)), "Stem + branches"),
          stat_card("Avg. height", sprintf("%.2f m", if (n) mean(d$height, na.rm = TRUE) else 0), "Mean tree height"),
          stat_card("Avg. DBH", sprintf("%.1f cm", if (n) mean(d$dbh_cm, na.rm = TRUE) else 0), "Mean diameter at 1.3 m")
      )
    })

    # ---- Carbon -------------------------------------------------------------
    output$carbon_cards <- renderUI({
      v <- sum(trees()$volume)
      total <- v * input$carbon_factor
      div(class = "stat-grid",
          stat_card("Total carbon", sprintf("%.2f Mg", total), "For the trees currently in view"),
          stat_card("Carbon / ha", sprintf("%.2f Mg/ha", total / area * 1e4), "Uses the fixed plot area"),
          stat_card("Volume in view", sprintf("%.2f m\u00B3", v), "Trees passing the filters"))
    })

    # ---- Location map (static) ---------------------------------------------
    if (has_crs) output$world_map <- renderLeaflet({
      pts  <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
      hull <- sf::st_convex_hull(sf::st_union(pts))
      m <- leaflet() |>
        setView(mean(df$lon), mean(df$lat), zoom = 4) |>
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo (Esri)") |>
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (Esri)") |>
        addLayersControl(baseGroups = c("Topo (Esri)", "Satellite (Esri)"),
                         options = layersControlOptions(collapsed = TRUE)) |>
        addMarkers(lng = mean(df$lon), lat = mean(df$lat), popup = "Location") |>
        addScaleBar(position = "bottomleft")
      if (all(sf::st_geometry_type(hull) %in% c("POLYGON", "MULTIPOLYGON")))
        m <- addPolygons(m, data = hull, weight = 2, color = "#ffffff", fillOpacity = 0.6)
      m
    })

    # ---- Tree map (leaflet) ---------------------------------------------------
    add_sel <- function(map, id) {
      map <- clearGroup(map, "sel")
      i <- match(id %||% NA_character_, df$name)
      if (is.na(i)) return(map)
      r <- df[i, ]
      if (has_crs)
        addCircles(map, lng = r$lon, lat = r$lat, radius = max(r$dbh * 1.6, 0.5),
                   group = "sel", color = accent_amber, weight = 4, fill = FALSE,
                   options = pathOptions(interactive = FALSE))
      else
        addCircleMarkers(map, lng = r$x, lat = r$y, radius = px_r(r$dbh) + 4,
                         group = "sel", color = accent_amber, weight = 4, fill = FALSE,
                         options = pathOptions(interactive = FALSE))
    }

    output$treemap <- renderLeaflet({
      d    <- trees()
      mode <- input$color_by
      # keep the user's current view when filters/colors change
      ctr  <- isolate(input$treemap_center)
      zm   <- isolate(input$treemap_zoom)

      m <- if (has_crs) {
        leaflet(options = leafletOptions(maxZoom = 22)) |>
          addProviderTiles(providers$Esri.WorldImagery, group = "Satellite",
                           options = providerTileOptions(maxZoom = 22, maxNativeZoom = 19)) |>
          addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo",
                           options = providerTileOptions(maxZoom = 22, maxNativeZoom = 19)) |>
          addLayersControl(baseGroups = c("Satellite", "Topo"),
                           options = layersControlOptions(collapsed = TRUE)) |>
          addScaleBar(position = "bottomleft")
      } else {
        leaflet(options = leafletOptions(crs = leafletCRS("L.CRS.Simple"), minZoom = -10))
      }

      cx <- if (has_crs) c("lon", "lat") else c("x", "y")
      pad <- if (has_crs) 1e-4 else 1
      if (!is.null(ctr) && !is.null(zm)) {
        m <- setView(m, ctr$lng, ctr$lat, zm)
      } else {
        m <- fitBounds(m, min(df[[cx[1]]]) - pad, min(df[[cx[2]]]) - pad,
                       max(df[[cx[1]]]) + pad, max(df[[cx[2]]]) + pad)
      }

      if (nrow(d) > 0) {
        if (mode == "Status") {
          cols <- unname(status_colors[as.character(d$Message)])
        } else {
          col  <- if (mode == "Volume") "volume" else "height"
          pal  <- colorNumeric("viridis", domain = range(df[[col]], na.rm = TRUE))
          cols <- pal(d[[col]])
        }
        lab <- lapply(hover_text(d), HTML)

        if (has_crs) {
          m <- addCircles(m, lng = d$lon, lat = d$lat, radius = d$dbh,   # 2x the DBH radius
                          layerId = d$name, group = "trees",
                          color = "white", weight = 1,
                          fillColor = cols, fillOpacity = 0.85, label = lab)
        } else {
          m <- addCircleMarkers(m, lng = d$x, lat = d$y, radius = px_r(d$dbh),
                                layerId = d$name, group = "trees",
                                color = "white", weight = 1,
                                fillColor = cols, fillOpacity = 0.85, label = lab)
        }

        m <- if (mode == "Status")
          addLegend(m, "bottomright", colors = unname(status_colors),
                    labels = status_levels, title = "Status")
        else
          addLegend(m, "bottomright", pal = pal, values = range(df[[col]], na.rm = TRUE),
                    title = if (mode == "Volume") "Volume (m\u00B3)" else "Height (m)")
      }
      add_sel(m, isolate(selected()))
    })

    # Highlight updates go through a proxy so the view is not reset. It also
    # depends on the active tab so a hidden map catches up when shown again.
    observe({
      input$main_tabs
      leafletProxy("treemap") |> add_sel(selected())
    })
    observeEvent(input$treemap_shape_click,  select_tree(input$treemap_shape_click$id))
    observeEvent(input$treemap_marker_click, select_tree(input$treemap_marker_click$id))

    # ---- Plotly helpers -------------------------------------------------------
    sel_overlay <- function(p, d, xcol, ycol, sel) {
      s <- d[d$name %in% sel, , drop = FALSE]
      if (nrow(s) == 0) return(p)
      add_markers(p, x = s[[xcol]], y = s[[ycol]], hoverinfo = "skip", showlegend = FALSE,
                  marker = list(color = accent_amber, size = 16,
                                line = list(color = "black", width = 2)))
    }

    # Trace order matters for click handling: the clickable markers are trace 0,
    # then the (optional) fit line, then the selection overlay.
    scatter_plot <- function(d, xcol, ycol, xlab, ylab, source, sel, fit = NULL) {
      need_trees(d)
      p <- plot_ly(source = source) |>
        add_markers(x = d[[xcol]], y = d[[ycol]], key = d$name,
                    text = hover_text(d), hoverinfo = "text", showlegend = FALSE,
                    marker = list(color = unname(status_colors[as.character(d$Message)]),
                                  size = 7, opacity = 0.75))

      annotations <- NULL
      if (!is.null(fit)) {
        xs <- seq(0, fit$range_cm[2], length.out = 100)   # cm
        p <- add_lines(p, x = xs, y = fit$fun(xs / 100), hoverinfo = "skip",
                       showlegend = FALSE,
                       line = list(color = brand_green_d, width = 1))
        annotations <- list(list(
          xref = "paper", yref = "paper", x = 0.02, y = 0.98,
          xanchor = "left", yanchor = "top", align = "left",
          showarrow = FALSE, bgcolor = "rgba(255,255,255,0.85)",
          bordercolor = "#e3e3df", borderwidth = 1, borderpad = 6,
          font = list(size = 12, color = ink),
          text = sprintf(paste0("<b>H = exp(%.3f) \u00D7 DBH<sup>%.3f</sup></b><br>",
                                "R\u00B2 = %.3f<br>RMSE = %.2f m<br>n = %d"),
                         fit$b0, fit$b1, fit$r2, fit$rmse, fit$n)))
      }

      p |>
        sel_overlay(d, xcol, ycol, sel) |>
        layout(xaxis = list(title = xlab), yaxis = list(title = ylab),
               uirevision = "keep", showlegend = FALSE,
               annotations = annotations) |>
        event_register("plotly_click")
    }

    # `source` and `curve` are forced: observeEvent() evaluates lazily, so
    # without force() every observer would see the last value of the loop.
    # `curve` is the index (0-based) of the clickable trace, used as a fallback
    # when plotly does not return the tree key.
    watch_click <- function(source, data_fn, curve) {
      force(source); force(data_fn); force(curve)
      get_ev <- function() suppressWarnings(
        event_data("plotly_click", source = source, priority = "event"))
      observeEvent(get_ev(), {
        ev <- get_ev()
        id <- NULL
        if (!is.null(ev$key)) {
          id <- unlist(ev$key)[1]
        } else if (!is.null(ev$pointNumber) && isTRUE(ev$curveNumber[1] == curve)) {
          id <- data_fn()$name[ev$pointNumber[1] + 1]
        }
        if (!is.null(id)) select_tree(id)
      })
    }
    # (observers are created after `contrib` is defined, see below)

    # Height ~ DBH allometry: H = exp(b0) * (dbh * 100)^b1, on the filtered trees
    height_fit <- reactive({
      d <- trees()
      d = d[d$Message == "OK",]
      fit_height_dbh(d$dbh, d$height)
    })

    output$plot_vol <- renderPlotly(
      scatter_plot(trees(), "dbh_cm", "volume", "Diameter (cm)", "Volume (m\u00B3)", "vol", selected()))
    output$plot_height <- renderPlotly(
      scatter_plot(trees(), "dbh_cm", "height", "Diameter (cm)", "Height (m)", "height", selected(),
                   fit = height_fit()))

    # ---- Distributions --------------------------------------------------------
    hist_plot <- function(x, xlab) {
      need_trees(trees())
      plot_ly(x = x, type = "histogram", nbinsx = 30,
              marker = list(color = brand_green, line = list(color = "white", width = 1))) |>
        layout(xaxis = list(title = xlab), yaxis = list(title = "Count"), bargap = 0.02)
    }
    output$hist_dbh    <- renderPlotly(hist_plot(trees()$dbh_cm, "Diameter (cm)"))
    output$hist_height <- renderPlotly(hist_plot(trees()$height, "Height (m)"))

    # ---- Contributions --------------------------------------------------------
    contrib <- reactive({
      d <- trees(); need_trees(d)
      d <- d[order(-d$volume), , drop = FALSE]
      d$rank <- seq_len(nrow(d))
      d$Vp <- cumsum(d$volume) / sum(d$volume) * 100
      d
    })

    watch_click("vol",     trees,   0)
    watch_click("height",  trees,   0)
    watch_click("contrib", contrib, 1)

    output$contrib_text <- renderUI({
      d <- contrib(); n <- nrow(d); h <- which(d$Vp >= 50)[1]
      div(class = "callout", HTML(sprintf(
        "The <b>%d</b> (%d%%) biggest trees represent 50%% of the volume. The <b>%d</b> (%d%%) smallest represent the other 50%%.",
        h, round(h / n * 100), n - h, round((n - h) / n * 100))))
    })

    output$plot_contrib <- renderPlotly({
      d <- contrib(); h <- which(d$Vp >= 50)[1]
      dash <- list(color = mid_gray, dash = "dash")
      plot_ly(source = "contrib") |>
        add_lines(x = d$rank, y = d$Vp, line = list(color = brand_green, width = 2),
                  hoverinfo = "skip", showlegend = FALSE) |>
        add_markers(x = d$rank, y = d$Vp, key = d$name, hoverinfo = "text",
                    text = sprintf("Tree: %s<br>rank %d \u2014 %.1f%% cumulative", d$name, d$rank, d$Vp),
                    marker = list(color = brand_green, size = 6), showlegend = FALSE) |>
        sel_overlay(d, "rank", "Vp", selected()) |>
        layout(xaxis = list(title = "Tree rank (largest first)"),
               yaxis = list(title = "Cumulative volume (%)"),
               uirevision = "keep",
               shapes = list(
                 list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 50, y1 = 50, line = dash),
                 list(type = "line", yref = "paper", y0 = 0, y1 = 1, x0 = h, x1 = h, line = dash))) |>
        event_register("plotly_click")
    })

    # Render (and thus register click events for) these plots even while their
    # tab is hidden, otherwise plotly warns that the event is not registered.
    for (o in c("plot_vol", "plot_height", "plot_contrib"))
      outputOptions(output, o, suspendWhenHidden = FALSE)

    # ---- QSM quality ------------------------------------------------------------
    qual <- reactive({
      d <- trees(); need_trees(d)
      q <- data.frame(
        Status = status_levels,
        Trees  = vapply(status_levels, function(l) sum(d$Message == l), 0L),
        Volume = vapply(status_levels, function(l) sum(d$volume[d$Message == l]), 0),
        stringsAsFactors = FALSE)
      q <- q[q$Trees > 0, ]
      q$Pct <- q$Volume / sum(q$Volume) * 100
      q[order(-q$Volume), ]
    })

    output$qsm_donut <- renderPlotly({
      q <- qual()
      plot_ly(labels = q$Status, values = q$Volume, type = "pie", hole = 0.55,
              textinfo = "label+percent", customdata = q$Trees,
              hovertemplate = "<b>%{label}</b><br>%{value:.2f} m\u00B3 (%{percent})<br>%{customdata} trees<extra></extra>",
              marker = list(colors = unname(status_colors[q$Status]),
                            line = list(color = "white", width = 1.5))) |>
        layout(showlegend = FALSE,
               title = list(text = "Share of volume by status", font = list(size = 13)),
               annotations = list(list(text = sprintf("%.1f m\u00B3", sum(q$Volume)),
                                       x = 0.5, y = 0.5, showarrow = FALSE,
                                       font = list(size = 14, color = ink))))
    })

    output$qsm_table <- renderTable({
      q <- qual()
      out <- data.frame(Status = q$Status,
                        Trees  = q$Trees,
                        Volume = sprintf("%.2f", q$Volume),
                        Pct    = sprintf("%.1f", q$Pct),
                        stringsAsFactors = FALSE)
      names(out) <- c("Status", "Trees", "Volume (m\u00B3)", "% of volume")
      out
    }, striped = TRUE, width = "100%")

    # ---- Data table (linked) ------------------------------------------------------
    output$tbl <- renderDT({
      d <- trees()
      idx <- match(isolate(selected()) %||% NA_character_, d$name)
      datatable(
        d[, c("name", "Message", "volume", "height", "dbh_cm")],
        rownames = FALSE, filter = "top", extensions = "Buttons",
        selection = list(mode = "single", selected = if (is.na(idx)) NULL else idx),
        colnames = c("Tree", "Status", "Volume (m\u00B3)", "Height (m)", "DBH (cm)"),
        options = list(pageLength = 15, dom = "Bftip", buttons = c("csv", "excel")),
        class = "display compact stripe")
    })
    observeEvent(input$tbl_rows_selected, {
      select_tree(trees()$name[input$tbl_rows_selected])
    })
    observeEvent(selected(), {
      idx <- match(selected() %||% NA_character_, trees()$name)
      rows <- if (is.na(idx)) NULL else idx
      selectRows(dataTableProxy("tbl"), rows)
    }, ignoreNULL = FALSE)

    # ---- 3D viewer (rgl / WebGL) ------------------------------------------------------
    output$view3d <- rgl::renderRglwidget({
      id <- req(selected())
      old <- options(rgl.useNULL = TRUE)      # offscreen: no native window
      on.exit(options(old), add = TRUE)
      try(rgl::close3d(), silent = TRUE)

      tid <- suppressWarnings(as.numeric(id))
      if (is.na(tid)) tid <- id

      res <- tryCatch({
        qsm <- qsf[[as.character(tid)]]
        if (is.null(qsm)) stop("no QSM found for this tree")
        withProgress(message = paste("Rendering tree", id), value = 0.5, {
          if (!is.null(las)) {
            tree <- lidR::filter_poi(las, treeID == tid)
            if (lidR::npoints(tree) == 0) {
              plot(qsm)                          # no points for this tree
            } else {
              x <- arbor::plot_semantic(tree)
              plot(qsm, add = x)
            }
          } else {
            plot(qsm)
          }
        })
        TRUE
      }, error = function(e) conditionMessage(e))
      validate(need(isTRUE(res), paste("Cannot render tree:", res)))

      w <- rgl::rglwidget()
      try(rgl::close3d(), silent = TRUE)
      w
    })

    # ---- Exports ---------------------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function() "tree_inventory.csv",
      content  = function(file) utils::write.csv(
        trees()[, c("name", "Message", "volume", "height", "dbh_cm")], file, row.names = FALSE))

    output$dl_gpkg <- downloadHandler(
      filename = function() "tree_inventory.gpkg",
      content  = function(file) sf::st_write(
        ans[ans$name %in% trees()$name, ], file, quiet = TRUE, append = FALSE))
  }

  shinyApp(ui, server)
}
