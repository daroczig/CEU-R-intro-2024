x <- seq(0, 20, by = 0.1)
plot(x, sin(x), type = "l")

curve(sin, from = 0, to = pi * 2)
curve(cos, from = 0, to = pi * 2, add = TRUE, col = "red")

## TODO brownian motion / random walk
x <- 0
## TODO 100 iterations
for (i in 1:100) {
  if (runif(1) < 0.5) {
    x <- x + 1
  } else {
    x <- x - 1
  }
}
x

## vectorize
set.seed(42)
cumsum(round(runif(100)) * 2 - 1)

h <- c(174, 170, 160)
w <- c(90, 80, 70)

min(w)
max(w)
range(w)
diff(range(w))
mean(w)
median(w)
sum(w)
summary(w)

str(summary(w))

cor(w, h)
lm(w ~ h)
## let's see predicted w for someone 165cm
-146.154 + 165 *  1.346

fit <- lm(w ~ h)
str(fit)
summary(fit)

predict(fit, newdata = list(h = 165))

predict(fit, newdata = list(h = 52))

plot(h, w)
abline(fit, col = "red")

df <- data.frame(weight = w, height = h)
df
str(df)
df$weight
df$weight[1]
df[1, 1]
df[2, 1]

nrow(df)
ncol(df)
dim(df)

plot(df)
cor(df)