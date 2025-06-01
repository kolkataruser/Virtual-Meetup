# Set seed for reproducibility
lines=par(lwd = 3)
set.seed(123)

# Simulate data
n <- 2000
x <- sort(runif(n, -2, 2))
f_true <- function(x) {sin(pi * x)}
sigma2 <- 0.25  # Variance of irreducible error
y <- f_true(x) + rnorm(n, mean = 0, sd = sqrt(sigma2))
plot(x, y, pch = 19, xlab = "Predictors", ylab = "Observations", 
     cex.lab = 1.3, font.lab = 2, col = "blueviolet", main = "Dataset")
axis(1, font = 2); axis(2, font = 2)

# Split into training and test sets
train_idx <- sample(1:n, 70)
test_idx <- setdiff(1:n, train_idx)

x_train <- x[train_idx]
y_train <- y[train_idx]
x_test <- x[test_idx]
y_test <- y[test_idx]

# Fit polynomial models from degree 1 to 15
max_degree <- 13
train_mse <- numeric(max_degree)
test_mse <- numeric(max_degree)

for (d in 1:max_degree) {
  # Create polynomial regression model
  form <- as.formula(paste("y_train ~ poly(x_train, ", d, ", raw=TRUE)", sep = ""))
  model <- lm(form)
  
  # Predictions
  y_pred_train <- predict(model)
  y_pred_test <- predict(model, newdata = data.frame(x_train = x_test))
  
  # MSE
  train_mse[d] <- mean((y_pred_train - y_train)^2)
  test_mse[d] <- mean((y_pred_test - y_test)^2)
}

# Plot the training and test MSE
plot(1:max_degree, train_mse, type = "o", col = "darkgray", lwd = 3, pch = 19,
     ylim = c(0, max(test_mse) + 0.1),
     xlab = "Model Flexibility",
     ylab = "Mean Squared Error",
     main = "Training vs Test MSE", font.lab = 2, cex.lab = 1.5)
lines(1:max_degree, test_mse, type = "o", col = "red", lwd = 3, pch = 19)
axis(1, font = 2); axis(2, font = 2)

# Add irreducible error line (horizontal)
abline(h = sigma2, col = "blue", lty = 2, lwd = 3)

# Add legend
legend("bottomleft", legend = c("Training MSE", "Test MSE", "Irreducible Error"),
       col = c("darkgray", "red", "blue"), lwd = 3, 
       pch = c(16, 16, NA), lty = c(1, 1, 2), bty = "n", text.font = 2)

abline(v = 9, col = "mediumorchid1", lwd = 3, lty = 3)
abline(v = 5, col = "mediumorchid1", lwd = 3, lty = 3)

text(3, 0.72, "Underfit", cex = 1.8)
text(7, 0.5, "Goodfit", cex = 1.8)
text(11, 0.72, "Overfit", cex = 1.8)

####### Predicted figure ######

## Degree 2 
model1 <- lm(y_train ~ poly(x_train, 2, raw = TRUE))


plot(x_train, y_train, pch = 19, xlab = "x", ylab = "sin(x)", 
     cex.lab = 1.3, font.lab = 2, col = "blueviolet")
axis(1, font = 2); axis(2, font = 2)


x.pred = seq(-2, 2, length.out = length(x_train))
lines(x.pred, predict(model1), col = "black")

## Degree 8 
model2 <- lm(y_train ~ poly(x_train, 5, raw = TRUE))


x.pred = seq(-2, 2, length.out = length(x_train))
lines(x.pred, predict(model2), col = "green")

## Degree 100 
model3 <- lm(y_train ~ poly(x_train, 13, raw = TRUE))


x.pred = seq(-2, 2, length.out = length(x_train))
lines(x.pred, predict(model3), col = "red")
