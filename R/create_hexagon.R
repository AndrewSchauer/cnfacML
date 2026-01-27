#' Create Hexagon Polygon Coordinates
#'
#' Generates the vertices for a hexagon centered at given coordinates.
#'
#' @param center_x Numeric. X-coordinate of hexagon center.
#' @param center_y Numeric. Y-coordinate of hexagon center.
#' @param radius Numeric. Distance from center to vertices. Default is 0.48.
#'
#' @return A data frame with x and y coordinates of the six hexagon vertices.
#'
#' @keywords internal
create_hexagon <- function(center_x, center_y, radius = 0.48) {
  angles <- seq(0, 2*pi, length.out = 7)
  data.frame(
    x = center_x + radius * cos(angles + pi/6),
    y = center_y + radius * sin(angles + pi/6)
  )
}
