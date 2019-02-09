


#' Run vtreat tests.
#'
#'
#' For all files with names of the form "^test_.+\\.R$" in the package directory unit_tests
#' run all functions with names of the form "^test_.+$" as RUnit tests.
#' Attaches RUnit and pkg, requires RUnit.
#' Stops on error.
#'
#' Based on \url{https://github.com/RcppCore/Rcpp/blob/master/tests/doRUnit.R}.  This
#' version is GPL-3, works derived from it must be distributed GPL-3.
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
  pkg <- "vtreat"
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::run_vtreat_tests")
  if(!requireNamespace("RUnit", quietly = TRUE)) {
    stop("run_packages_tests requires RUnit package")
  }
  if(!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("run_packages_tests requires", pkg, "package to test", pkg))
  }
  attached_packages <- .packages(all.available = FALSE)
  if(require_RUnit_attached) {
    if(!("RUnit" %in% attached_packages)) {
      stop("run_package_tests requires RUnit to already be attached via library('RUnit')")
    }
  }
  if(require_pkg_attached) {
    if(!(pkg %in% attached_packages)) {
      stop(paste0("run_package_tests requires ",
                  pkg,
                  " to already be attached via library('", pkg, "')"))
    }
  }
  for(ptd in package_test_dirs) {
    test_dirs <- c(test_dirs, system.file(ptd, package = pkg, mustWork = TRUE))
  }
  set.seed(2019)  # try to make things a bit more deterministic
  print(paste("RUnit testing package", pkg, "version", utils::packageVersion(pkg)))
  test_suite <- RUnit::defineTestSuite(name = paste(pkg, "unit tests"),
                                       dirs = test_dirs,
                                       testFileRegexp = "^test_.+\\.R$",
                                       testFuncRegexp = "^test_.+$")
  test_results <- RUnit::runTestSuite(test_suite,
                                      verbose = verbose,
                                      gcBeforeTest = FALSE)
  RUnit::printTextProtocol(test_results,
                           separateFailureList = TRUE,
                           showDetails = FALSE)
  test_errors <- RUnit::getErrors(test_results)
  if(stop_on_issue) {
    # stop if errors for R CMD CHECK
    if(test_errors$nDeactivated>0) {
      warning(paste("package", pkg, "has deactivated tests"))
    }
    if((test_errors$nFail>0) || (test_errors$nErr>0)) {
      stop(paste("package", pkg, "had test failures/errors"))
    }
  }
  if(stop_if_no_tests) {
    if(test_errors$nTestFunc<=0) { # catch packge test problem
      stop(paste("found no package", pkg, "RUnit tests"))
    }
  }
  invisible(test_results)
}


