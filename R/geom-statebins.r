#' A statebins Geom
#'
#' Pass in a data frame of states and values and let this do the work. It enables
#' easy faceting and makes it simpler to have a uniform legend across all the
#' plots.\cr
#' \cr
#' There are two special/critical `aes()` mappings:\cr
#' \cr
#' - `state` (so the geom knows which column to map the state names/abbrevs to)
#' - `fill` (which column you're mapping the filling for the squares with)
#'
#' @md
#' @param mapping Set of aesthetic mappings created by `aes()` or
#'   `aes_()`. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to `ggplot()`.
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    `fortify()` for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame.`, and
#'    will be used as the layer data.
#' @param border_col border color of the state squares, default "`white`"
#' @param border_size thickness of the square state borders
#' @param lbl_size font size (relative) of the label text
#' @param dark_lbl,light_lbl colrs to be uses when the label should be dark or light.
#'        The function automagically computes when this should be.
#' @param radius the corner radius
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... other arguments passed on to `layer()`. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. `borders()`.
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

    # saveRDS(xdat, "/tmp/setupdata.rds")

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
