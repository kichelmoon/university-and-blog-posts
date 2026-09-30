x <- seq(from=1, to=10, length.out = 500)

# Power law equation: y = x^a where a = 2
a <- 2
y <- x^(-a)

par(mfrow = c(1, 2))

plot(x, y, 
     type = "l", 
     col = "blue", 
     lwd = 2,
     main = "Default Scale", 
     xlab = "x", 
     ylab = "y")
grid()

# 2. Log-Log Plot
plot(x, y, 
     type = "l", 
     col = "red", 
     lwd = 2,
     log = "xy", 
     main = "Log-Log Scale", 
     xlab = "x (log scale)", 
     ylab = "y (log scale)")
grid()

# Reset layout
par(mfrow = c(1, 1))
