#include bit64

lcg <- function(seed, a=1664525, c=1013904223, m=2^32) {
  value <- as.integer64(seed)
  a <- as.integer64(a)
  c <- as.integer64(c)
  m <- as.integer64(m)
  function() {
    value <<- (a * value + c) %% m
    as.numeric(value)
  }
}

max_subarray_sum <- function(n, seed, min_val, max_val) {
  lcg_gen <- lcg(seed)
  random_numbers <- sapply(1:n, function(x) (lcg_gen() %% (max_val - min_val + 1)) + min_val)
  
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
  sum(sapply(1:20, function(x) max_subarray_sum(n, lcg_gen(), min_val, max_val)))
}

# Parameters
n <- 10000
initial_seed <- 42
min_val <- -10
max_val <- 10

# Timing the function
start_time <- Sys.time()
result <- total_max_subarray_sum(n, initial_seed, min_val, max_val)
end_time <- Sys.time()

cat("Total Maximum Subarray Sum (20 runs):", result, "\n")
cat("Execution Time:", format(end_time - start_time, digits=6), "seconds\n")
