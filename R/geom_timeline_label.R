#' Create Timeline Lablels
#'
#' This function creates a new geom that  can build on geom_timeline and add labels to the top n biggest earthqakes by magnitude
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
#' @importFrom ggplot2 layer ggproto aes draw_key_label Geom
#'
#' @importFrom grid textGrob segmentsGrob gTree gpar gList
#'
#' @importFrom dplyr group_by top_n %>%
#'
#' @details Create a new geom called geom_timeline_label that is used to plot relevant top_n labels on geom_timeline
#'
#' @return Returns a ggplot object
#'
#' @examples
#' \dontrun{data %>%
#'     ggplot() +
#'     geom_timeline(aes(
#'         x = DATE,
#'         y = COUNTRY,
#'         fill = TOTAL_DEATHS,
#'         size = EQ_PRIMARY)) +
#'     geom_timeline_label(aes(
#'         x = DATE,
#'         y = COUNTRY,
#'         magnitude = EQ_PRIMARY,
#'         label = LOCATION_NAME
#'         ,n_max=3 #number of lables to show))
#' }
#'
#' @export


 geom_timeline_label <- function(mapping = NULL, data = NULL, stat = 'identity',
                                  position = 'identity', na.rm = FALSE,
                                  show.legend = NA, inherit.aes = TRUE, ...) {
    ggplot2::layer(
      geom = Geom_Timeline_Label, mapping = mapping,
      data = data, stat = stat, position = position,
      show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
 }

 Geom_Timeline_Label <- ggplot2::ggproto('Geom_Timeline_Label',
                                         ggplot2::Geom,
                                         required_aes = c('x', 'label', 'magnitude'),
                                         # default values for certain aesthetics
                                         default_aes = ggplot2::aes(y=1,                                                                                                              shape = 19,
                                                                    color='gray',
                                                                    size=.25,
                                                                    linetype=1,
                                                                    angle=45,
                                                                    lineheight=2,
                                                                    hjust=0,
                                                                    vjust=0,
                                                                    n_max=5
                                         ),

                                         draw_key = ggplot2::draw_key_label,

                                         draw_panel = function(data, panel_scales, coord) {

                                           # filter to only n largest magnitude earthquakes
                                           n_max<- data$n_max[1]

                                           data <- data %>%
                                             dplyr::group_by(group)%>%
                                             dplyr::top_n(n_max, magnitude)


                                           coords <- coord$transform(data, panel_scales)

                                           label <- grid::textGrob(
                                             coords$label,
                                             x=coords$x,
                                             y=coords$y+.1,
                                             rot=coords$angle,
                                             default.units = "native",
                                             hjust = coords$hjust,
                                             vjust = coords$vjust,
                                             gp = grid::gpar(
                                               lineheight = coords$lineheight
                                             )
                                           )

                                           line <- grid::segmentsGrob(
                                             x0=coords$x, #start
                                             x1=coords$x, #stop
                                             y0=coords$y, #start
                                             y1=coords$y+.1, #stop
                                             default.units = "native",
                                             gp = grid::gpar(
                                               size=.4,
                                               color = 'black'
                                             )
                                           )

                                           # combine grobs into one
                                           timeline_label <- grid::gTree(children = grid::gList(line, label))

                                         }
 )

