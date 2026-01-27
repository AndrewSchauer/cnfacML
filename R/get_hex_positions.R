#' Get Hexagonal Grid Positions
#'
#' Calculates the x,y coordinates for nodes in a hexagonal SOM grid.
#'
#' @param xdim Integer. Width of the SOM grid.
#' @param ydim Integer. Height of the SOM grid.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{node}{Node number (1 to xdim*ydim)}
#'     \item{x}{X-coordinate of node center}
#'     \item{y}{Y-coordinate of node center}
#'   }
#'
#' @details
#' Hexagonal grids use offset rows where every other row is shifted by 0.5 units.
#' Vertical spacing is adjusted for hexagonal geometry (sqrt(3)/2 ≈ 0.866).
#' The grid is flipped vertically so node 1 appears at the top-left.
#'
#' @keywords internal
get_hex_positions <- function(xdim, ydim) {
  positions <- data.frame(
    node = integer(),
    x = numeric(),
    y = numeric()
  )
  
  node_counter <- 1
  
  for (row in 1:ydim) {
    for (col in 1:xdim) {
      # Hexagonal offset: every other row is shifted by 0.5
      x_pos <- col + ifelse(row %% 2 == 0, 0.5, 0)
      # Vertical spacing adjusted for hex geometry (sqrt(3)/2 ≈ 0.866)
      # Flip vertically by subtracting from max: start from top
      y_pos <- (ydim - row + 1) * 0.866
      
      positions <- rbind(positions, data.frame(
        node = node_counter,
        x = x_pos,
        y = y_pos
      ))
      node_counter <- node_counter + 1
    }
  }
  
  return(positions)
}
