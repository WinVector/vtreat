


#' Run vtreat tests.
#'
#'
#' For all files with names of the form "^test_.+\\.R$" in the package directory unit_tests
#' run all functions with names of the form "^test_.+$" as RUnit tests.
#' Attaches RUnit and pkg, requires RUnit.
#' Stops on error.
#'
#'
#' @param ... not used, force later arguments to bind by name.
#' @param verbose logical, if TRUE print more.
#' @param package_test_dirs directory names to look for in the installed package.
#' @param test_dirs paths to look for tests in.
#' @param stop_on_issue logical, if TRUE stop after errors or failures.
#' @param stop_if_no_tests logical, if TRUE stop if no tests were found.
#' @param require_RUnit_attached logical, if TRUE require RUnit be attached before testing.
#' @param require_pkg_attached logical, if TRUE require pkg be attached before testing.
#' @param rngKind pseudo-random number generator method name.
#' @param rngNormalKind pseudo-random normal generator method name.
#' @return RUnit test results (invisible).
#'
#' @export
#'
run_vtreat_tests <- function(...,
                             verbose = TRUE,
                             package_test_dirs = "unit_tests",
                             test_dirs = character(0),
                             stop_on_issue = TRUE,
                             stop_if_no_tests = TRUE,
                             require_RUnit_attached = FALSE,
                             require_pkg_attached = TRUE,
                             rngKind = "Mersenne-Twister",
                             rngNormalKind = "Inversion") {
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::run_vtreat_tests")
  pkg <- "vtreat"
  wrapr::run_package_tests(pkg = pkg,
                           verbose = verbose,
                           package_test_dirs = package_test_dirs,
                           test_dirs = test_dirs,
                           stop_on_issue = stop_on_issue,
                           stop_if_no_tests = stop_if_no_tests,
                           require_RUnit_attached = require_RUnit_attached,
                           require_pkg_attached = require_pkg_attached,
                           rngKind = rngKind,
                           rngNormalKind = rngNormalKind)
}


