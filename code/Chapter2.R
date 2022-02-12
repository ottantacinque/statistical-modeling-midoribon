#データの読み込み
load("data.Rata")

data <- c(2, 4, 6, 4, 5, 2, 3, 1, 2, 0,
          4, 3, 3, 3, 3, 4, 2, 7, 2, 4,
          3, 3, 3, 4, 3, 7, 5, 3, 1, 7,
          6, 4, 6, 5, 2, 4, 7, 2, 2, 6,
          2, 4, 5, 4, 5, 1, 3, 2, 3)

data
length(data)

# dataの要約
summary(data)

# 度数分布を得る
table(data)

# ヒストグラム
hist(data, breaks = seq(-0.5, 9.5, 1))

# 標本分散
var(data)

# 標本標準偏差
sd(data)
sqrt(var(data))

# 平均3のポアソン分布を図示してみる。
y <- 0:9
prob <- dpois(y, lambda = 6)
plot(y, prob, type = "b", lty = 3)

# 対数尤度LogL(lambda)とlambdaの関係を調べる
logL <- function(m) sum(dpois(data, m, log=TRUE))
lambda <- seq(2, 5, 0.1)
plot(lambda, sapply(lambda, logL), type= "l")

