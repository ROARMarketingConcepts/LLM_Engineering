library(microbenchmark)

lcg <- function(seed, a=1664525, c=1013904223, m=2^32) {
  value <- as.numeric(seed)
  function() {
    value <<- (a * value + c) %% m
    value
  }
}

max_subarray_sum <- function(n, seed, min_val, max_val) {
  lcg_gen <- lcg(seed)
  random_numbers <- (vapply(1:n, function(x) lcg_gen(), numeric(1)) %% (max_val - min_val + 1)) + min_val
  
  max_sum <- -Inf
  current_sum <- 0
  for (num in random_numbers) {
    current_sum <- max(num, current_sum + num)
    max_sum <- max(max_sum, current_sum)
  }
  max_sum
}

total_max_subarray_sum <- function(n, initial_seed, min_val, max_val) {
  lcg_gen <- lcg(initial_seed)
  seeds <- vapply(1:20, function(x) lcg_gen(), numeric(1))
  sum(vapply(seeds, function(seed) max_subarray_sum(n, seed, min_val, max_val), numeric(1)))
}

# Parameters
n <- 10000        # Number of random numbers
initial_seed <- 42 # Initial seed for the LCG
min_val <- -10    # Minimum value of random numbers
max_val <- 10     # Maximum value of random numbers

# Timing the function
result <- microbenchmark(total_max_subarray_sum(n, initial_seed, min_val, max_val), times = 1)

cat("Total Maximum Subarray Sum (20 runs):", result$time / 1e9, "\n")
cat("Execution Time:", result$time / 1e9, "seconds\n")
