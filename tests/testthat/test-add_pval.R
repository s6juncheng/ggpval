library(ggplot2)
data("ToothGrowth")

plt <- ggplot(ToothGrowth, aes(supp, len)) +
  geom_boxplot()

plt_facet <- ggplot(ToothGrowth, aes(supp, len)) +
  geom_boxplot() +
  facet_wrap(~dose)

test_that("Simple plot", {
  add_pval_ggplot(plt, pairs = list(c(1, 2)))
})

test_that("With facets", {
  add_pval_ggplot(plt_facet, pairs = list(c(1, 2)))
})

test_that("Add annotation", {
  add_pval_ggplot(plt, pairs = list(c(1, 2)), annotation = "Awesome")
})

