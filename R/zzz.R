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