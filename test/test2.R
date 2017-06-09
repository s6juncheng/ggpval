# test with real data

dt <- readRDS("data/dt.RDS")

g <- ggplot(dt, aes(cv, value)) + geom_boxplot() + theme_bw(base_size = 16) +
  facet_grid(. ~ Interval)

add_pval_ggplot(g, pairs = list(c(1,2)), heights = 0.15,
                size = 5, pval_text_adj = 0.075, log = T, barheight = 0.025)
