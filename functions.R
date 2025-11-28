# Helper functions

generate_numeric_col <- function(n, distribution, params) {
  if (distribution == "Normal") {
    rnorm(n, params$mean, params$sd)
  } else if (distribution == "Uniform") {
    runif(n, params$min, params$max)
  } else if (distribution == "Exponential") {
    rexp(n, params$rate)
  } else if (distribution == "Poisson") {
    rpois(n, params$lambda)
  } else if (distribution == "Zero-inflated Poisson") {
    rZIP(n, params$mu, params$sigma)
  } else if (distribution == "Binomial") {
    rbinom(n, params$n, params$p)
  }
}