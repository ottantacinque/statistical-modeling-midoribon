# データの読み込みなど
d <- read.csv("data3a.csv")
d$f <- as.factor(d$f)

# 4.2 統計モデルの当てはまりの悪さ：逸脱度
# xモデルのglmの結果表示
fit <- glm(y ~ x, data=d, family=poisson)
summary(fit)

# フルモデルの最大対数尤度
sum(log(dpois(d$y, lambda=d$y)))

# 一定モデルを使った推定
fit.null <- glm(formula=y~1, family=poisson, data=d)
summary(fit.null)

# 一定モデルの最大対数尤度
logLik(fit.null)





