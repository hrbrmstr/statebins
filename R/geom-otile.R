geom_otile <- function(mapping = NULL, data = NULL, ...,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {

  stat <- "otile"
  position <- "identity"

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomOtile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

StatOtile <- ggplot2::ggproto("StatOtile", Stat,

  default_aes = ggplot2::aes(
    colour = NA, size = 0.1, linetype = 1, alpha = NA, l_fill="blue", r_fill="red"
  ),

  required_aes = c("x", "y"),

  compute_panel = function(data, scales) {
    data
  }

)

stat_otile <- function(mapping = NULL, data = NULL, geom = "otile",
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE, ...) {

  layer(
    stat = StatOtile, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}

GeomOtile <- ggplot2::ggproto("GeomOtile", GeomOscar,

  default_aes = ggplot2::aes(
    l_fill = "blue", r_fill = "red", colour = NA, size = 0.1, linetype = 1, alpha = NA
  ),

  setup_data = function(data, params) {
    data$width <- data$width %||% params$width %||% ggplot2::resolution(data$x, FALSE)
    data$height <- data$height %||% params$height %||% ggplot2::resolution(data$y, FALSE)

    transform(data,
      xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  draw_key = ggplot2::draw_key_polygon

)

