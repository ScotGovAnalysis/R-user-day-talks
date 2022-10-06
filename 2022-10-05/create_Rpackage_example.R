
# Initial setup -----------------------------------------------------------

# Create a package structure in a directory - here called rpackagestest
usethis::create_package("C:/tests/rpackagetest")

# Note rules on naming a package (letters, numbers, . only, at least 2 characters, start with letter)


# Git and GitHub ----------------------------------------------------------
# (not covering this in talk)
usethis::use_git()
# Can use Git Bash / shell
# to change default branch to main
# git branch -m master main
# git remote add origin ssh_to_remote
# If using GitHub, probably also want a README:
usethis::use_readme_rmd()
# Might want to add badges to show the status : https://github.com/r-lib/devtools/
usethis::use_lifecycle_badge("experimental")


# Adding functions --------------------------------------------------------

# create a function
usethis::use_r("standard_scale_df")

# then we want to create a function
standard_scale <- function(x) {
  (x - mean(x)) / stats::sd(x)
}

standard_scale_df <- function(df) {
  df %>% dplyr::mutate_if(is.numeric, standard_scale)
}

# (talk about help functions and main function, which to add roxygen comment)
# Add dplyr to imports
usethis::use_package("dplyr")
usethis::use_package("stats")
usethis::use_package("magrittr")

# when want to use pipe, put this in function
#' @importFrom magrittr %>%
# Alternatively a pipe can be added with and can use in all functions
usethis::use_pipe()


# Document load and test --------------------------------------------------

# The roxygen comment should be added for the main function and then create documentation and namespace entry
devtools::document() # equivalent of roxygen2::roxygenize()

# Now let's test it!
devtools::load_all()

test_iris <- iris
test_iris <- standard_scale_df(test_iris)
head(test_iris, 10)
#Show that it's worked by printing original and scaled
head(iris, 10)

# (Note that scale_column function available, but not in global env because it's like loading from library()
# Bring up the help
?standard_scale_df


# Checking and building ---------------------------------------------------

# Good idea to check package periodically while developing
devtools::check()

# Final thing we will cover - building
devtools::build() # this will create a "source" tar.gz
devtools::build(binary = TRUE) # this will make a compiled R binary at the version of R you are using

# By default build will be created in parent dir of project, can then install and load
# Need to restart R as package is already loaded from devtools::load_all()
.rs.restartR()
install.packages("../rpackagetest_0.0.0.9000.zip", repos=NULL, type="win.binary")
library(rpackagetest)

# If want to change the build version - edit the description file or
usethis::use_version()
