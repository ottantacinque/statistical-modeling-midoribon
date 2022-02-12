# 3.2 観測されたデータの概要を調べる
#データの読み込み
d <- read.csv("data3a.csv")

# f列は因子なのでfactorに変換（本に記載なし）
d$f <- as.factor(d$f)

#データの確認
d

# dの列ごとにデータを表示
d$x
d$y
d$f

# データオブジェクトがどういうクラスに属しているかを調べる
class(d)
class(d$y)
class(d$x)
class(d$f)

# データフレームの概要を調べる
summary(d)


# 3.3 統計モデリングの前にデータを図示する
# データ全体を見る
plot(d$x, d$y, pch=c(21,19)[d$f])
legend("topleft", legend=c("C","T"), pch=c(21,19))


# 3.4 ポアソン回帰の統計モデル
# ポアソン回帰の推定量の導出
fit <- glm(y ~ x, data=d, family=poisson)
fit

# 詳細な結果の確認
summary(fit)

# 対数尤度の評価
logLik(fit)

# ポアソン回帰の推定結果を使って、回帰直線を図示
plot(d$x, d$y, pch=c(21,19)[d$f])
xx <- seq(min(d$x), max(d$x), length=100)
lines(xx, exp(1.29+0.0757*xx), lwd=2)

# predict()関数を使ったやり方
plot(d$x, d$y, pch=c(21,19)[d$f])
yy <- predict(fit, newdata=data.frame(x=xx), type="response")
lines(xx,yy,lwd=2)


# 3.5 説明変数が因子型の統計モデル
# 因子型の説明変数を使った推定
fit.f <- glm(y ~ f, data=d, family=poisson)
fit.f
summary(fit.f)

# 因子型の説明変数を使ったモデルの最大対数尤度
logLik(fit.f)


# 3.6 説明変数が数量型＋因子型の統計モデル
# 数量型, 因子型の両方を説明変数を使った推定
fit.all <- glm(y ~ x + f, data=d, family=poisson)
fit.all

# 最大対数尤度を調べる
logLik(fit.all)