geom_rrect <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

GeomRrect <- ggplot2::ggproto("GeomRrect", ggplot2::Geom,
  default_aes = aes(colour = NA, fill = "grey35", size = 0.5, linetype = 1,
    alpha = NA),

  required_aes = c("xmin", "xmax", "ymin", "ymax"),

  draw_panel = function(self, data, panel_params, coord) {
      coords <- coord$transform(data, panel_params)
      lapply(1:length(coords$xmin), function(i) {
        ggname("geom_rrect", grid::roundrectGrob(
          coords$xmin[i], coords$ymax[i],
          width = (coords$xmax[i] - coords$xmin[i]),
          height = (coords$ymax[i] - coords$ymin)[i],
          r=unit(0.3, "snpc"),
          default.units = "native",
          just = c("left", "top"),
          gp = grid::gpar(
            col = coords$colour[i],
            fill = alpha(coords$fill[i], coords$alpha[i]),
            lwd = coords$size[i] * .pt,
            lty = coords$linetype[i],
            lineend = "butt"
          )
        ))
      }) -> gl

      do.call(grid::gList, gl)
  },

  draw_key = ggplot2::draw_key_polygon
)