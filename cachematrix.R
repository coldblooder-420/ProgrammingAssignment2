# Function to create a cacheable function
make_cacheable <- function(f) {
  cache <- NULL
  
  # Define the wrapper function
  function(...) {
    args <- list(...)
    args_key <- paste(names(args), args, sep = "=", collapse = "&")
    
    if (!is.null(cache) && identical(cache$args_key, args_key)) {
      message("Using cached result")
      return(cache$result)
    } else {
      message("Computing and caching result")
      result <- f(...)
      cache <<- list(args_key = args_key, result = result)
      return(result)
    }
  }
}

# Example of a time-consuming function to compute mean
time_consuming_mean <- function(x) {
  Sys.sleep(2)  # Simulate a time-consuming operation
  return(mean(x))
}

# Create a cacheable version of the function
cached_mean <- make_cacheable(time_consuming_mean)

# Now, let's call the function with a vector
print(cached_mean(1:100)) # This will compute and cache the result
print(cached_mean(1:100)) # This will use the cached result
