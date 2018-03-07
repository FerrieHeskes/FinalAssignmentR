#' Geom of a timeline containing earthquakes
#'
#' This geom plots a timeline showing earthquakes with the time it occured, the scaled size, the country and the number of casualties momentfunction uses the data file with fatal injuries in motor vehicle traffic crashes in the USA to summarizes the total number
#'
#' @importFrom ggplot2 aes draw_key_point Geom layer
#' @importFrom grid gList gpar pointsGrob polylineGrob
#'
#' @examples
#' \dontrun{
#' data %>% eq_clean_data() %>%
#'    filter(COUNTRY %in% c("USA"), YEAR > 2000) %>%
#'    ggplot(aes(x = DATE, y = COUNTRY, colour = TOTAL_DEATHS, size = EQ_PRIMARY)) +
#'    geom_timeline() +
#'    theme_timeline() +
#'    labs(size = "Richter scale value", colour = "# deaths")
#' }
#'
#' @export
#'
GeomMyTimeline <- ggplot2::ggproto("GeomMyTimeline", ggplot2::Geom,
                       required_aes = c("x"),
                       default_aes = ggplot2::aes(colour = "grey", size = 1.5, alpha = 0.8, shape = 19, stroke = 1, fill = "grey"),
                       draw_key = ggplot2::draw_key_point,
                       draw_panel = function(data, panel_scales, coord) {
                         ## Transform the data first
                         coords <- coord$transform(data, panel_scales)

                         ## Construct a grid grob

                         # Points
                         points <- grid::pointsGrob(
                           x = coords$x,
                           y = coords$y,
                           pch = coords$shape,
                           size = unit(coords$size / 5, "char"), #Set the size
                           gp = grid::gpar(col = coords$colour, alpha = coords$alpha) #Set the colour and alpha
                         )

                         # Lines
                         y_lines <- unique(coords$y) #Only unique points/values

                         lines <- grid::polylineGrob(
                           x = unit(rep(c(0, 1), each = length(y_lines)), "npc"),
                           y = unit(c(y_lines, y_lines), "npc"),
                           id = rep(seq_along(y_lines), 2),
                           gp = grid::gpar(col = "lightgrey")
                         )

                         grid::gList(points, lines)
                       })

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomMyTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Geom to create labels for earthquakes on a timeline
#'
#' This geom plots labels with a certain number of largest (by size) earthquake on a timeline.
#' The timeline is created by \code{geom_timeline}
#'
#' @param n_max The number of the largest earthquakes to be labelled
#' @param mapping The mapping
#' @param data The data
#' @param stat The stat
#' @param position  The position
#' @param na.rm The na
#' @param show.legend The legend
#' @param inherit.aes The aesthetics
#' @param ... Et all
#'
#' @importFrom ggplot2 draw_key_blank Geom layer
#' @importFrom dplyr group_by top_n ungroup
#' @importFrom grid gList gpar polylineGrob textGrob
#'
#' @examples
#' \dontrun{
#' data %>% eq_clean_data() %>%
#'    filter(COUNTRY %in% c("CHINA", "USA"), YEAR > 2000) %>%
#'    ggplot(aes(x = DATE, y = COUNTRY, colour = TOTAL_DEATHS, size = EQ_PRIMARY)) +
#'    geom_timeline() +
#'    geom_timeline_label(aes(label = LOCATION_NAME), n_max = 5) +
#'    theme_timeline() +
#'    labs(size = "Richter scale value", colour = "# deaths")
#' }
#'
#' @export
#'
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE, n_max = NULL,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomMyTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max, ...)
  )
}

GeomMyTimelineLabel <- ggplot2::ggproto("GeomMyTimelineLabel", ggplot2::Geom,
                            required_aes = c("x", "label"),
                            draw_key = ggplot2::draw_key_blank,
                            setup_data = function(data, params) {
                              #Get the top 5 to attach a textlabel
                              data <- data %>%
                                dplyr::group_by("group") %>%
                                dplyr::top_n(params$n_max, size) %>%
                                dplyr::ungroup()
                            },
                            draw_panel = function(data, panel_scales, coord, n_max) {
                              coords <- coord$transform(data, panel_scales)
                              #Set the offset per group (i.e. line) for the line and corresponding textlabel
                              #to draw them properly
                              n_group <- length(unique(data$group))
                              offset_x <- 0.1 / ( 10 * n_group )
                              offset_y <- 0.2 / n_group

                              #Line
                              line <- grid::polylineGrob(
                                x = unit(c(coords$x, coords$x), "npc"),
                                y = unit(c(coords$y, coords$y + offset_y), "npc"),
                                id = rep(1:dim(coords)[1], 2),
                                gp = grid::gpar(col = "grey"))

                              #Textlabel
                              label <- grid::textGrob(
                                label = coords$label,
                                x = unit(coords$x + offset_x, "npc"),
                                y = unit(coords$y + offset_y, "npc"),
                                just = c("left", "bottom"),
                                rot = 45
                              )

                              grid::gList(line, label)
                            })

#' theme_timeline
#'
#' This is a function that makes some changes to visualize the eartquakes on the timeline
#'
#' @importFrom ggplot2 element_blank element_line theme
#'
#' @examples
#' \dontrun{
#' data %>% eq_clean_data() %>%
#' filter(COUNTRY %in% c("USA"), YEAR > 2000) %>%
#'    ggplot(aes(x = DATE, y = COUNTRY, colour = TOTAL_DEATHS, size = EQ_PRIMARY)) +
#'    geom_timeline() +
#'    theme_timeline()
#' }
#'
#' @export
#'
theme_timeline <- function() {
  ggplot2::theme(
    axis.line.x = ggplot2::element_line(size = 1.5),
    axis.title.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.position = "bottom",
    panel.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_blank()
  )
}
