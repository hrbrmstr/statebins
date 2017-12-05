context("basic functionality")
test_that("we can do something", {

  require(ggplot2)

  flu <- cdcfluview::ili_weekly_activity_indicators(2017)

  ggplot(flu, aes(state=statename, fill=activity_level)) +
    geom_statebins() +
    coord_equal() +
    ggplot2::facet_wrap(~weekend) +
    labs(title="2017-18 Flu Season ILI Activity Level") +
    theme_statebins() -> gg

  gb <- ggplot_build(gg)

  data(USArrests)

  USArrests$state <- rownames(USArrests)

  statebins(USArrests, value_col="Assault", name = "Assault") +
    theme_statebins(legend_position="right") -> gg

  gb <- ggplot_build(gg)

})
