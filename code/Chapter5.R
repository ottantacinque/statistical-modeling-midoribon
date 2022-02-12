# データの読み込みなど
d <- read.csv("data3a.csv")
d$f <- as.factor(d$f)

# 一定モデル, xモデルを使った推定
fit1 <- glm(y ~ 1, data=d, family=poisson)
fit2 <- glm(y ~ x, data=d, family=poisson)


# 5.4 帰無仮説を棄却するための有意水準
# 残差逸脱度の確認
fit1$deviance
fit2$deviance

# 一定モデルとxモデルの逸脱度の差を求める
fit1$deviance - fit2$deviance

# 真のモデルから100個体分のデータを新しく生成する。
d$y.rnd <- rpois(100, lambda= mean(d$y))

# glm() を使って、一定モデルをxモデルに新データを当てはめてみる
fit1 <- glm(y.rnd ~ 1, data=d, family=poisson)
fit2 <- glm(y.rnd ~ x, data=d, family=poisson)
fit1$deviance - fit2$deviance

# pb.Rを呼び出して実行
source("pb.R")
dd12 <- pb(d, n.bootstrap = 1000)

# 結果の概要を調べる
summary(dd12)

# ヒスグラムの表示
hist(dd12, 100)
abline(v= 4.5, lty = 2)

# delta D12 >= 4.5 となる数
sum(dd12 >= 4.5)

# P=0.05 となる逸脱どの差(delta D1,2)を調べてみる
quantile(dd12, 0.95)

# カイ二乗分布を使った近似計算法
fit1 <- glm(y ~ 1, data=d, family=poisson)
fit2 <- glm(y ~ x, data=d, family=poisson)

anova(fit1, fit2, test="Chisq")
