library(ggpval)
library(ggplot2)
library(data.table)
A <- rnorm(200, 0, 3)
B <- rnorm(200, 2, 4)
G <- rep(c("G1", "G2"), each = 100)
dt <- data.table(A, B, G)
dt <- melt(dt, id.vars = "G")
dt[, G := factor(G, levels = c("G2", "G1"))]
R <- sample(c('R1', 'R2'), 400, replace = TRUE)
dt[, R := factor(R, levels = c("R2", "R1"))]

plt <- ggplot(dt, aes(variable, value)) +
  geom_boxplot() +
  geom_jitter()

plt_facet_wrap <- ggplot(dt, aes(variable, value)) +
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(~G)

plt_facet_grid <- ggplot(dt, aes(variable, value)) +
  geom_boxplot() +
  geom_jitter() +
  facet_grid(R~G)

add_pval(plt, pairs = list(c(1, 2)))
add_pval(plt, pairs = list(c(1, 2)), pval_star = T)

add_pval(plt_facet_wrap, pairs = list(c(1, 2)))
add_pval(plt_facet_grid, pairs = list(c(1, 2)))
add_pval(plt_facet_grid, pairs = list(c(1, 2)), pval_star = T)

add_pval(plt, pairs = list(c(1, 2)), annotation = "Awesome")
add_pval(plt, pairs = list(c(1, 2)), annotation = "Awesome", pval_star = T)

add_pval(plt_facet_wrap, pairs = list(c(1, 2)), annotation = list("Awesome1", "Awesome2"))

# Bar plot
dt[, mu := mean(value),
   by = c("G", "variable")]

dt[, se := sd(value) / .N,
   by = c("G", "variable")]

plt_bar <- ggplot(dt, aes(x=variable, y=mu, fill = variable)) +
  geom_bar(stat = "identity", position = 'dodge') +
  geom_errorbar(aes(ymin=mu-se, ymax=mu+se),
                width = .2) +
  facet_wrap(~G)

add_pval(plt_bar, pairs = list(c(1, 2)), response = 'value')
add_pval(plt_bar, pairs = list(c(1, 2)), response = 'value', pval_star = T)
