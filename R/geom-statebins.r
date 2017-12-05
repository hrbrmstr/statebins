#' A statebins Geom (WIP!)
#'
#' WIP!!!!!!!!!!! Full geom documentation coming soon. See Details and the Examples for now.
#'
#' For now, you pass in:
#'
#' - `border_col` (border color of the state squares, default "`white`")
#' - `border_size` (thickness of the square state borders)
#' - `lbl_size` (the relative font size of the state label text, default `3`)
#' - `dark_lbl` (the color for the state label text when it shld be dark text on light background)
#' - `light_lbl` (the color for the state label text when it shld be light text on dark background)
#' - `radius` (corner radius --- `0` for a "square")
#'
#' You also need to (ok, _should_) pass in two `aes()` mappings:
#'
#' - `state` (so the geom knows which column to map the state names/abbrevs to)
#' - `fill` (which column you're mapping the filling for the squares with)
#'
#' @md
#' @export
#' @examples \dontrun{
#' library(statebins)
#' library(cdcfluview)
#' library(hrbrthemes)
#' library(tidyverse)
#'
#' flu <- ili_weekly_activity_indicators(2017)
#'
#' ggplot(flu, aes(state=statename, fill=activity_level)) +
#'   geom_statebins() +
#'   coord_equal() +
#'   viridis::scale_fill_viridis(
#'     name = "ILI Activity Level  ", limits=c(0,10), breaks=0:10, option = "magma", direction = -1
#'   ) +
#'   facet_wrap(~weekend) +
#'   labs(title="2017-18 Flu Season ILI Activity Level") +
#'   theme_statebins(base_family = font_ps) +
#'   theme(plot.title=element_text(size=16, hjust=0)) +
#'   theme(plot.margin = margin(30,30,30,30))
#' }
geom_statebins <- function(
  mapping = NULL, data = NULL,
  border_col = "white", border_size = 2,
  lbl_size = 3, dark_lbl = "black", light_lbl = "white",
  radius = grid::unit(6, "pt"),
  ...,
  na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomStatebins,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      border_col = border_col,
      border_size = border_size,
      lbl_size = lbl_size,
      dark_lbl = dark_lbl,
      light_lbl = light_lbl,
      radius = radius,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_statebins
#' @export
GeomStatebins <- ggplot2::ggproto("GeomStatebins", ggplot2::Geom,

  default_aes = ggplot2::aes(
    fill = "grey20", colour = NA, size = 0.1, linetype = 1,
    state = "state", label="abbrev", angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  extra_params = c("na.rm", "width", "height"),

  setup_data = function(data, params) {

    # message("setup_data()")
    # saveRDS(data, "/tmp/data.rds")

    state_data <- data.frame(data, stringsAsFactors=FALSE)

    if (max(nchar(state_data[,"state"])) <= 3) {
      merge.x <- "abbrev"
    } else {
      merge.x <- "state"
    }

    state_data <- validate_states(state_data, "state", merge.x, ignore_dups=TRUE)

    st.dat <- merge(b_state_coords, state_data,
                    by.x=merge.x, by.y="state", all.y=TRUE, sort=TRUE)

    st.dat$width <- st.dat$width %||% params$width %||% ggplot2::resolution(st.dat$x, FALSE)
    st.dat$height <- st.dat$height %||% params$height %||% ggplot2::resolution(st.dat$y, FALSE)

    transform(st.dat,
      xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    ) -> xdat

    saveRDS(xdat, "tmp/setupdata.rds")

    xdat

  },

  required_aes = c("state", "fill"),

  draw_panel = function(self, data, panel_params, coord,
                        border_col = "white", border_size = 2,
                        lbl_size = 3, dark_lbl = "black", light_lbl = "white",
                        radius = grid::unit(6, "pt")) {

    tile_data <- data
    tile_data$size <- 2
    tile_data$colour <- border_col
    tile_data$size <- border_size

    text_data <- data
    text_data$label <- data$abbrev
    text_data$fill <- NA
    text_data$size <-  lbl_size
    text_data$colour <- .sb_invert(data$fill, dark_lbl, light_lbl)

    coord <- coord_equal()

    grid::gList(
      GeomRtile$draw_panel(tile_data, panel_params, coord, radius),
      ggplot2::GeomText$draw_panel(text_data, panel_params, coord)
    ) -> grobs

    ggname("geom_statebins", grid::grobTree(children = grobs))

  },

  draw_key = ggplot2::draw_key_polygon

)
