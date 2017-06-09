# Unit test
# simulate data
require(data.table)
require(ggplot2)
A <- rnorm(200, 0, 3)
B <- rnorm(200, 2, 4)
G <- rep(c("G1", "G2"), each = 100)
dt <- data.table(A, B, G)
dt <- melt(dt, id.vars = "G")

plt <- ggplot(dt, aes(variable, value)) +
  geom_boxplot()

add_pval_ggplot(plt, pairs = list(c(1, 2)), heights = 11)

plt <- ggplot(dt, aes(variable, value)) +
  geom_boxplot() +
  facet_wrap(~G)

add_pval_ggplot(plt, pairs = list(c(1, 2)), pval_text_adj = 0)
