context("basic functionality")
test_that("we can do something", {

  require(ggplot2)
  require(statebins)

  data(USArrests)

  USArrests$state <- rownames(USArrests)

  a1 <- USArrests
  a2 <- USArrests
  a3 <- USArrests

  a1$f <- 1
  a2$f <- 2
  a3$f <- 3

  a4 <- rbind.data.frame(rbind.data.frame(a1, a2), a3)

  ggplot(a4, aes(state=state, fill=Assault)) +
    geom_statebins() +
    coord_equal() +
    ggplot2::facet_wrap(~f) -> gg

  gb <- ggplot_build(gg)

  expect_equal(length(gb$plot$facet), 3)

  statebins(USArrests, value_col="Assault", name = "Assault") -> gg

  gb <- ggplot_build(gg)


})
