
geom_oscar <- function(mapping = NULL, data = NULL,
                      ...,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {

  stat = "oscar"
  position = "identity"

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomOscar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

StatOscar<- ggplot2::ggproto("StatOscar", Stat,

  required_aes = c("xmin", "xmax", "ymin", "ymax"),

  default_aes = ggplot2::aes(
    l_fill = "blue", r_fill = "red", colour = NA, size = 0.1, linetype = 1, alpha = NA
  ),

  required_aes = c("x", "y"),

  compute_panel = function(data, scales) {
    data
  }

)

stat_oscar <- function(mapping = NULL, data = NULL, geom = "oscar",
                       position = "identity", na.rm = FALSE, n = 500, revolutions = NULL,
                       show.legend = NA, inherit.aes = TRUE, ...) {

  layer(
    stat = StatOscar, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}

GeomOscar <- ggplot2::ggproto("GeomOscar", ggplot2::Geom,

  default_aes = ggplot2::aes(
    l_fill = "blue", r_fill = "red", colour = NA, size = 0.1, linetype = 1, alpha = NA
  ),

  draw_panel = function(self, data, panel_params, coord) {

    coords <- coord$transform(data, panel_params)

    lapply(1:length(coords$xmin), function(i) {

      oscarGrob(
        name = as.character(i),
        coords$xmin[i], coords$ymax[i],
        width = (coords$xmax[i] - coords$xmin[i]),
        height = (coords$ymax[i] - coords$ymin)[i],
        default.units = "native",
        gpbl = grid::gpar(
          col = coords$colour[i],
          fill = alpha(coords$l_fill[i], coords$alpha[i]),
          lwd = coords$size[i] * .pt,
          lty = coords$linetype[i],
          lineend = "butt"
        ),
        gptr = grid::gpar(
          col = coords$colour[i],
          fill = alpha(coords$r_fill[i], coords$alpha[i]),
          lwd = coords$size[i] * .pt,
          lty = coords$linetype[i],
          lineend = "butt"
        )

      )

    }) -> gl

    grobs <- do.call(grid::gList, gl)

    ggname("geom_oscar", grid::grobTree(children = grobs))

  },

  draw_key = ggplot2::draw_key_polygon

)
