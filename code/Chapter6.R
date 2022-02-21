# 6.2 例題；上限のあるカウントデータ
# load("d.RData")
# d.RDataがないのでcsvから読み込む
d <- read.csv("data4a.csv")
d$f <- as.factor(d$f)

# 各列のまとめを表示する
summary(d)

# 図示してみる
plot(d$x, d$y, pch=c(21,19)[d$f])


# 6.4 ロジスティク回帰とロジットリンク関数
#ロジスティク関数を作図してみる
logistic <- function(z) 1 / (1 + exp(-z))
z <- seq(-6 ,6, 0.1)
plot(z, logistic(z), type= "l")

# ロジスティック回帰のパラメーター推定
# c bind(y, N-y) のyは生存数, N-yは死んだ数
glm(cbind(y, N - y) ~ x + f, data = d, family = binomial)

# 結果を元に図示してみる
logistic <- function(x) 1 / (1 + exp(- (-19.5 + 1.95 * x)))
logistic2 <- function(x) 1 / (1 + exp(- (-19.5 + 1.95 * x + 2.022)))
x <- seq(7 ,12, 0.1)
plot(x, logistic(x), type = "l")
plot(x, logistic2(x), type = "l")

# Mass package のstepAIC()関数を使い、ネストしているモデルのAICを比較する
library(MASS) # stezzpAICを定義するMASS package読み込み
stepAIC(fit.xf) # オブジェクトエラーで実行できない


# 6.5 交互作用項の入った線形予測子
# 交互作用を入れた統計モデルのパラメーター推定
glm(cbind(y, N - y) ~ x * f, family = binomial, data = d)


# 6.6 割算値の統計モデリングはやめよう
d <- read.csv("data4b.csv")

# オフセット項を指定したモデルでのパラメーター推定
glm(y ~ x, offset = log(A), family = poisson, data = d)


# 6.7 正規分布とその尤度
# 正規位分布の密度関数を図示してみる
y <- seq(-5, 5, 0.1)
plot(y, dnorm(y, mean=0, sd=1), type="l")

# 1.2<=y<=1.8 の範囲の確率を計算する
pnorm(1.8, 0, 1) - pnorm(1.2, 0, 1)

# 長方形の面積として上記の確率を近似計算する
dnorm(1.5, 0, 1) * 0.6


# 6.8 ガンマ分布のGLM
d <- read.csv("d.csv")

# プロットを図示する
plot(d$x, d$y, type="p")

# ガンマ分布を用いてパラメータ推定する
glm(y ~ log(x), family=Gamma(link="log"), data=d)
