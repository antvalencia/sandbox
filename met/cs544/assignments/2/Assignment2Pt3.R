# Write your own R function, all.primes(n), that returns a vector of all the prime numbers up to n (inclusive).
allprimes <- function(n) {
    # i) Initialize the variable, source, to a vector sequence from 2 to n.
    source <- 2:n
    # ii) Initialize the variable, result, to the empty vector, or NULL.
    result <- NULL

    # In a loop, do the following as long as the there are numbers remaining in the variable, source.
    while (length(source) >= 1) {
      # iii) take the first element of source.
      n <- source[1]
      source <- source[-1]
      # iv) concatenate this element to result.
      result <- c(result, n)
      # v) Modify source by eliminating all numbers that are multiples of the element taken
      #    in step iii). This has to be done in a single statement without using any loop.
      source <- source[source%%n != 0]
    }
    # After the loop terminates, return the result.
    return(result)
}

allprimes(100)