#' @importFrom utils packageDescription
NULL

.onAttach <- function(lib, pkg, ...) {
  packageStartupMessage(d3heatmapWelcomeMessage())
}

#
#


d3heatmapWelcomeMessage <- function() {
  # library(utils)
  
  paste0(
    "\n",
    "======================\n",
    "Welcome to d3heatmap version ", utils::packageDescription("d3heatmap")$Version, "\n",
    # "\n",
    "\n",
    "Type citation('d3heatmap') for how to cite the package.\n",
    "Type ?d3heatmap for the main documentation.\n",
    "\n",
    "The github page is: https://github.com/talgalili/d3heatmap/\n",
    "Please submit your suggestions and bug-reports at: https://github.com/talgalili/d3heatmap/issues\n",
    "You may ask questions at stackoverflow, use the r and d3heatmap tags: \n\t https://stackoverflow.com/questions/tagged/d3heatmap\n",
    # "\n",
    # "\tTo suppress this message use:  ", "suppressPackageStartupMessages(library(d3heatmap))\n",
    "======================\n"
  )
}



# devtools::use_code_of_conduct()

# When all is done, run:
# require(devtools)
# check()
# devtools::check(args="--as-cran")
#                 Thanks to: http://stackoverflow.com/questions/10017702/r-cmd-check-options-for-more-rigorous-testing-2-15-0
# file.copy("NEWS", "NEWS.md", overwrite = TRUE)
# system('git log --graph --stat --date=short --pretty=format:"%ad(%an) %s |%h" > ChangeLog', intern = TRUE)
# pkgdown::build_site()
# devtools::check_win_devel()
# release(check = TRUE)