# 7.1 例題；GLMでは説明出来ないカウントデータ
d <- read.csv("data7.csv")
summary(d)

# プロットの作成
plot(jitter(d$x,0.5), d$y)

# GLMを当てはめてみる。
fit <- glm(cbind(y, N - y) ~ x, data = d, family = binomial)
summary(fit)

# 結果をプロットしてみる
logistic <- function(x) 1 / (1 + exp(- (-2.1487 + 0.5104 * x)))
x <- seq(2 ,6, 0.1)
plot(jitter(d$x,0.5), d$y)
points(x,logistic(x)*8, type = "l")


# 7.2 過分散と個体差
d4 <- d[d$x==4,]

# 生存数がy_i子であった個体を数える。
table(d4$y)

# このデータの平均と分散を調べる
c(mean(d4$x), var(d4$y))

# x=4での種子の生存数をプロットしてみる
hist(d_4$y)


# 7.4 一般化線形混合モデルの最尤推定
# Rを使ってGLMMのパラメーターを推定する。
library(glmmML)
fit = glmmML(cbind(y, N-y)~x, data=d, family=binomial, cluster=id)
summary(fit)

# 結果をプロットしてみる
logistic_glmm <- function(x) 1 / (1 + exp(- (-4.190 + 1.005 * x)))
x <- seq(2 ,6, 0.1)
plot(jitter(d$x,0.5), d$y)
points(x,logistic_glmm(x)*8, type = "l")
