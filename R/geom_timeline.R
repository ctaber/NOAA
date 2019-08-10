#' Create Timeline Geom for Earthquake Data
#'
#' This function uses ggplot to visualize a timeline of earthquakes.
#'
#' @param mapping defaults to NULL
#'
#' @param data defaults to NULL, unless data passed in
#'
#' @param stat defautls to 'identity'
#'
#' @param position defautls to 'identity'
#'
#' @param na.rm defaults to FALSE, do not remove NAs
#'
#' @param show.legend defaults to NA
#'
#' @param inherit.aes defaults to TRUE
#'
#' @param ... takes additional arguments
#'
#' @importFrom ggplot2 layer ggproto aes  draw_key_point Geom
#'
#' @importFrom grid pointsGrob gpar
#'
#' @details Create a new geom called geom_timeline that is used to plot a timline of earthquakes
#'
#' @return Returns a ggplot object
#'
#' @examples
#' \dontrun{data %>%
#' ggplot() +
#' geom_timeline(aes(x = DATE,
#' y = COUNTRY,
#' fill = TOTAL_DEATHS,
#' size = EQ_PRIMARY))
#' }
#'
#' @export

geom_timeline <- function(mapping = NULL, data = NULL, stat = 'identity',
                          position = 'identity', na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
      ggplot2::layer(
        geom = Geom_Timeline, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
      )
  }


  Geom_Timeline <- ggplot2::ggproto('Geom_Timeline',
                                    ggplot2::Geom,
                                    # ggproto() function is used to construct a new class corresponding to your new geom.
                                    # This new class specifies a number of attributes and functions that describe how data should be drawn on a plot.
                                    # a character vector of required aesthetics
                                    required_aes = c('x'),
                                    # default values for certain aesthetics
                                    default_aes = ggplot2::aes(y = 0,
                                                               shape = 21, # or 19 but doesnt work with FILL
                                                               color = 'darkgrey',
                                                               size = NA,
                                                               alpha = .75,
                                                               stroke=.5,
                                                               fill='darkgrey'
                                    ),
                                    # a function used to draw the key in the legend
                                    draw_key = ggplot2::draw_key_point,
                                    # Overriding draw_panel() is most appropriate if there is one graphic element per row.
                                    # In other cases, you want graphic element per group. For example, take polygons: each row gives one
                                    # vertex of a polygon. In this case, you should instead override draw_group()
                                    draw_group = function(data, panel_scales, coord) {
                                      #Function that returns a grid grob that will plotted (this is where the real work occurs)
                                      ## data: element is a data frame containing one column for each aesthetic specified
                                      ## panel_scales: is a list containing information about the x and y scales for the current panel
                                      # coord:is an object that describes the coordinate system of your plot.

                                      # transform
                                      coords <- coord$transform(data, panel_scales)

                                      #resize
                                      coords$size <-  coords$size / max(coords$size) * 1.5


                                      # gridgrob
                                      grid::pointsGrob(x = coords$x,
                                                       y = coords$y,
                                                       pch = coords$shape,
                                                       gp = grid::gpar(col = coords$color,
                                                                       alpha = coords$alpha,
                                                                       cex=coords$size,
                                                                       fill = coords$fill
                                                       )
                                      )


                                    }
  )


