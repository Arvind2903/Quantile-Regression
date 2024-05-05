# Load required library
library(quantreg)

# Load dataset
data(mtcars)

# Plot of mpg vs wt
plot(mpg ~ wt, data = mtcars, main = "mpg ~ wt")

# Linear regression vs median regression plot
plot(mpg ~ wt, data = mtcars, pch = 16, main = "mpg ~ wt")
lm_model <- lm(mpg ~ wt, data = mtcars)  # Fit linear regression model
rq_model <- rq(mpg ~ wt, data = mtcars)  # Fit quantile regression model
abline(lm_model, col = "red", lty = 2)   # Add linear regression line
abline(rq_model, col = "blue", lty = 2)  # Add quantile regression line
legend("topright", legend = c("lm", "rq"), col = c("red", "blue"), lty = 2)  # Add legend

# Summary of linear regression model
summary_lm <- summary(lm_model)  # Get summary of linear regression model
print(summary_lm)  # Print summary

# Summary of quantile regression model
summary_rq <- summary(rq_model)  # Get summary of quantile regression model
print(summary_rq)  # Print summary

# Plot with outliers
y <- c(mtcars$mpg, 42, 44)  # Add outliers to mpg
x <- c(mtcars$wt, 3, 4)     # Add outliers to wt
plot(y ~ x, pch = 16, main = "mpg ~ wt")  # Plot mpg vs wt with outliers
points(c(5, 4), c(40, 36), pch = 16, col = "dark orange")  # Add synthetic outliers
abline(lm_model, col = "red", lty = 2)   # Add linear regression line
abline(lm(y ~ x), col = "red")           # Add linear regression line with outliers
abline(rq_model, col = "blue", lty = 2)  # Add quantile regression line
abline(rq(y ~ x), col = "blue")          # Add quantile regression line with outliers

# Fitting Multiple Quantile Plots
plot(mpg ~ wt, data = mtcars, pch = 16, main = "mpg ~ wt")  # Plot mpg vs wt
multi_rqfit <- rq(mpg ~ wt, data = mtcars, tau = seq(0, 1, by = 0.1))  # Fit multiple quantile regression models
for (j in 1:ncol(multi_rqfit$coefficients)) {  # Loop over each coefficient
  abline(coef(multi_rqfit)[, j], col = colors[j])  # Add quantile regression lines
}
legend("topright", legend = seq(0, 1, by = 0.1), col = colors, lty = 1, pch = 16,  # Add legend
       title = "Tau", cex = 0.8, bg = "white", box.lwd = 1)
