data(sleep)

g1 <- sleep$extra[1:10]
g2 <- sleep$extra[11:20]
difference <- g2 - g1


# 95% confidence interval, manually:
n <- 10
mu <- mean(difference)
sigma <- sd(difference)
mu + c(-1, 1) * qt(0.975, n-1) * sigma / sqrt(n)


# 95% confidence interval, using built-in t.test
t.test(difference)$conf.int
