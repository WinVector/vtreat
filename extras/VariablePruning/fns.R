

# return unscaled version of exp(x)
safe_exp <- function(x) {
  x <- as.numeric(x)
  x <- x - max(x)
  x <- exp(x)
  return(x)
}


# parallel many parameter generator driver
p_data_given_params <- function(
  ...,  # force arguments to bind by name
  params,
  data,
  fn_factory,
  sd_noise = 1,
  cl = NULL) {
  stop_if_dot_args(
    substitute(list(...)), 
    "p_data_given_params")
  f <- fn_factory(params, data, sd_noise)
  if(!is.null(cl)) {
    res <- parallel::parLapply( # or just lapply() without cl
      cl,
      seq_len(nrow(params)),
      f
    )
  } else {
    res <- lapply(
      seq_len(nrow(params)),
      f)
  }
  posterior_weight <- params$prior_weight * safe_exp(res)
  return(posterior_weight / sum(posterior_weight))
}


# plot our results
plot_parameter <- function(params, param_name, 
                           ...,
                           subtitle = NULL,
                           use_hist = FALSE,
                           bins = 30) {
  stop_if_dot_args(substitute(list(...)), "plot_parameter")
  dpost <- params[, c(param_name, 'posterior_weight')]
  dpost$what <- 'posterior estimate'
  names(dpost)[2] <- 'weight'
  dprior <- params[, c(param_name, 'prior_weight')]
  dprior$what <- 'prior assumption'
  names(dprior)[2] <- 'weight'
  dsub <- rbind(dpost, dprior)
  dsub$what <- factor(
    dsub$what,
    levels = c("prior assumption", "posterior estimate"))
  post_est <- sum(params$posterior_weight * params[[param_name]]) / 
    sum(params$posterior_weight)
  subtitle <- paste0(subtitle, '; posterior ', param_name,' est. = ', 
                     sprintf("%.2g",post_est))
  plt <- ggplot(data = dsub,
                aes_string(x = param_name, 
                           color = 'what', 
                           fill = 'what'))
  if(use_hist) {
    plt <- plt + geom_histogram(aes(weight = weight), 
                                alpha = 0.5, 
                                bins = bins) 
  } else{
    plt <- plt + geom_density(aes(weight = weight), 
                              alpha = 0.5)
  }
  plt <- plt +
    facet_wrap( ~ what, scales = 'free_y', ncol = 1) +
    scale_color_brewer(palette = 'Dark2') +
    scale_fill_brewer(palette = 'Dark2') +
    ggtitle(paste('prior and posterior estimates for', param_name),
            subtitle = subtitle)
  if(use_hist) {
    plt <- plt + ylab('probability')
  } else {
    plt <- plt + ylab('density')
  }
  return(plt)
}

