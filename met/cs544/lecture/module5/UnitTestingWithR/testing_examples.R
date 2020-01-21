if (!is.element("testthat", installed.packages()[,"Package"]))
  install.packages("testthat", repos="http://cran.us.r-project.org", dependencies = TRUE)


library(testthat)


# Expectation, test, context

# Expectations

# equals()
# Passes
expect_that(10, equals(4+6))
expect_that(10, equals(as.integer(10)))
# Fails
expect_that(10, equals(5+6))

#
expect_equal(10, 4+6)
expect_equal(10, as.integer(10))
expect_equal(10, 5+6)


# is_identical_to uses identical() to check for exact equality
expect_that(10, is_identical_to(4+6))
# Fails
expect_that(10, is_identical_to(5+6))
expect_that(10, is_identical_to(as.integer(10)))

#
expect_identical(10, 4+6)
expect_identical(10, 5+6)
expect_identical(10, as.integer(10))


# is_equivalent_to() is a more relaxed version of equals() that ignores attributes
# Fails
expect_that(c("one" = 1, "two" = 2), equals(1:2))
# Passes
expect_that(c("one" = 1, "two" = 2), is_equivalent_to(1:2))

#
expect_equivalent(c("one" = 1, "two" = 2), 1:2)


# is_a() checks that an object inherit()s from a specified class
data <- table(rep(1:5, 1:5))
expect_that(data, is_a("table"))
# Fails
expect_that(data, is_a("data.frame"))

#
expect_is(data, "table")

# matches() matches a character vector against a regular expression. 
string <- "Testing is fun!"
# Passes
expect_that(string, matches("Testing"))
# Fails, match is case-sensitive
expect_that(string, matches("testing"))
# Passes, match can be a regular expression
expect_that(string, matches("T.+ting"))

#
expect_match(string, "Testing")
# Fails, match is case-sensitive
expect_match(string, "testing")
# Passes
expect_match(string, "testing", ignore.case = TRUE)


# prints_text() matches the printed output from an expression against a regular expression
a <- list(1:10, letters)
str(a)
# Passes
expect_that(str(a), prints_text("List of 2"))
# Passes
expect_output(str(a), "List of 2")
expect_output(str(a), "int [1:10]", fixed = TRUE) # match as it is

# gives_warning() expects that you get a warning
# Passes
log(-1)
expect_that(log(-1), gives_warning())
expect_that(log(-1), gives_warning("NaNs produced"))
# Fails
log(0)
expect_that(log(0), gives_warning())

#
expect_warning(log(-1))
# But always better to be explicit
expect_warning(log(-1), "NaNs produced")
# Failure to produce a warning or error when an error is expected
expect_warning(log(0))

# throws_error() verifies that the expression throws an error.
# Fails
expect_that(1 / 2, throws_error())
# Passes
expect_that(1 / "a", throws_error())
# But better to be explicit
expect_that(1 / "a", throws_error("non-numeric argument"))

#
expect_error(1 / "a")
expect_error(1 / "a", "non-numeric argument")

# is_true

expect_that( 2 == 2, is_true())
expect_true(2 == 2)

# is_false

expect_that( 2 == 3, is_false())
expect_false(2 == 3)

## Tests

setwd("~/Downloads/UnitTestingWithR)

test_file("./tests/testthat/test_strings.r", reporter = "tap")

test_dir("./tests/testthat/", reporter = "summary")
