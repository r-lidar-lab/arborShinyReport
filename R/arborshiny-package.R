#' arborshiny: Interactive Forest Inventory Report
#'
#' An interactive shiny report for 'arbor' QSM inventories.
#'
#' @name arborshiny-package
#' @import shiny
#' @importFrom DT DTOutput renderDT datatable dataTableProxy selectRows
#' @importFrom plotly plot_ly add_markers add_lines layout event_register
#'   event_data plotlyOutput renderPlotly
#' @importFrom leaflet leaflet leafletOutput renderLeaflet leafletProxy
#'   leafletOptions leafletCRS setView fitBounds addProviderTiles providers
#'   providerTileOptions addLayersControl layersControlOptions addScaleBar
#'   addMarkers addPolygons addCircles addCircleMarkers clearGroup pathOptions
#'   colorNumeric addLegend
"_PACKAGE"

# `treeID` is used with non-standard evaluation in lidR::filter_poi()
utils::globalVariables("treeID")
