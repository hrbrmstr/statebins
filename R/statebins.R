#' Create a new ggplot-based "statebin" chart for USA states/territories
#'
#' Pass in a data frame and get back a square choropleth.
#'
#' The `state_col` and `value_col` parameters default to `state` and `value`. That means
#' if you name the columns you want to plot with those names, you can forego passing them
#' in. Othersise, use `"strings"`.
#'
#' A _handy_ feature of this function is that you can specify a `dark_label` color
#' and a `light_label` color. What does that mean? Well, you also pass in the
#' color scale function you're going to use and `statebins` will apply it and use
#' that information to determine what the tile color is and --- if it's "dark" it will
#' use the `light_label` and if it's "light" it will use the `dark_label` color. That
#' means the labels will never blend in to the background (as long as you specify
#' decent label colors).
#'
#' You can customize the scale function you pass in by using name parameters. All named
#' parameters not used by `statebins()` itself get passed to the scale function.
#'
#' @md
#' @param state_data data frame of states and values to plot
#' @param state_col column name in \code{state_data} that has the states. no duplicates
#'        and can be names (e.g. "\code{Maine}") or abbreviatons (e.g. "\code{ME}")
#' @param value_col column name in \code{state_data} that holds the values to be plotted
#' @param dark_label,light_label,na_label dark/light/NA label colors. The specified color will be used
#'        when the algorithm determines labels should be inverted.
#' @param font_size font size (default = \code{3})
#' @param state_border_col default "\code{white}" - this creates the "spaces" between boxes
#' @param state_border_size border size
#' @param round rounded corners (default: `FALSE`)
#' @param radius if `round` is `TRUE` then use `grid::unit` to specify the corner radius.
#'        Default is `grid::unit(6, "pt")` if using rounded corners.
#' @param ggplot2_scale_function ggplot2 scale function to use. Defaults to `scale_fill_distiller`
#'        since you're likely passing in continuous data when you shouldn't be :-)
#' @param ... additional parameters to the scale function
#' @return ggplot2 object
#' @export
#' @examples
#' data(USArrests)
#'
#' USArrests$state <- rownames(USArrests)
#' statebins(USArrests, value_col="Assault", name = "Assault") +
#'   theme_statebins(legend_position="right")
statebins <- function(state_data,
                      state_col="state", value_col="value",
                      dark_label="black", light_label="white",
                      na_label="white", font_size=3,
                      state_border_col="white", state_border_size=2,
                      round=FALSE, radius=grid::unit(6, "pt"),
                      ggplot2_scale_function=ggplot2::scale_fill_distiller,
                      ...) {

  state_data <- data.frame(state_data, stringsAsFactors=FALSE)

  if (max(nchar(state_data[,state_col])) <= 3) {
    merge.x <- "abbrev"
  } else {
    merge.x <- "state"
  }

  state_data <- validate_states(state_data, state_col, merge.x)

  st.dat <- merge(state_coords, state_data, by.x=merge.x, by.y=state_col, all.y=TRUE,
                  sort=TRUE)

  gg <- ggplot()

  if (round) {
    gg <- gg + geom_rtile(data = st.dat, radius = radius,
                         aes_string(x = "col", y = "row", fill = value_col),
                         color = state_border_col, size = state_border_size)
  } else {
    gg <- gg + geom_tile(data = st.dat,
                         aes_string(x = "col", y = "row", fill = value_col),
                         color = state_border_col, size = state_border_size)
  }

  gg <- gg + scale_y_reverse()
  gg <- gg + ggplot2_scale_function(...)
  gg <- gg + coord_equal()
  gg <- gg + labs(x = NULL, y = NULL)

  gb <- ggplot2::ggplot_build(gg)

  gg <- gg + geom_text(data = st.dat,
                       aes_string(x = "col", y = "row", label = "abbrev"),
                       angle = 0,
                       color = .sb_invert(gb$data[[1]]$fill, dark_label, light_label, na_label),
                       size = font_size)

  gg

}
