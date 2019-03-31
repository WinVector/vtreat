
# pipes for vtreat

#' @export
apply_right.treatmentplan <- function(pipe_left_arg,
                                      pipe_right_arg,
                                      pipe_environment,
                                      left_arg_name,
                                      pipe_string,
                                      right_arg_name) {
  prepare(pipe_right_arg, pipe_left_arg)
}

#' @export
apply_right.simple_plan <- function(pipe_left_arg,
                                    pipe_right_arg,
                                    pipe_environment,
                                    left_arg_name,
                                    pipe_string,
                                    right_arg_name) {
  prepare(pipe_right_arg, pipe_left_arg)
}

#' @export
apply_right.multinomial_plan <- function(pipe_left_arg,
                                         pipe_right_arg,
                                         pipe_environment,
                                         left_arg_name,
                                         pipe_string,
                                         right_arg_name) {
  prepare(pipe_right_arg, pipe_left_arg)
}