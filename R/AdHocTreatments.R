
#' @importFrom stats predict
NULL

#' Build a monotone transform transform in a chosen direction
#' 
#' Depends on stats
#'
#' @param x numeric vector input
#' @param y numeric vector (same lenght as x) target output
#' @param decreasing logical if TRUE build decreasing instead
#' @param k integer if not null approximate number of knots to prune to
#' @return monotone function mapping x to y
#' 
#' @examples
#' 
#' d <- data.frame(x=c(3,1,-12,NA,5,NaN))
#' d$y <- log(d$x)
#' m1 <- monotoneXformD(d$x,d$y)
#' m2 <- monotoneXformD(d$x,d$y,decreasing=TRUE)
#' d$pred1 <- m1(d$x)
#' d$pred2 <- m2(d$x)
#' 
#' @export
#'
monotoneXformD <- function(x, y, 
                           decreasing= FALSE, k= NULL) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if(decreasing) {
    x <- -x
  }
  good <- (!is.na(x)) & (!is.na(y))
  x <- x[good]
  y <- y[good]
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  m <- stats::isoreg(x,y)
  xvals <- m$x[m$iKnots]
  yvals <- m$yf[m$iKnots]
  if((!is.null(k)) && (k<length(xvals))) {
    # cut down to no more than 2 k + 2 points
    lookupsX <- c()
    if(xvals[[length(xvals)]]>xvals[[1]]) {
      probesX <- seq(xvals[[1]],xvals[[length(xvals)]],length.out=k)
      lookupsX <- pmin(length(xvals),pmax(1,findInterval(probesX,xvals)))
    }
    lookupsY <- c()
    if(yvals[[length(yvals)]]>yvals[[1]]) {
      probesY <- seq(yvals[[1]],yvals[[length(yvals)]],length.out=k)
      lookupsY <- pmin(length(yvals),pmax(1,findInterval(probesY,yvals)))
    }
    idxs <- sort(unique(c(1,length(xvals),lookupsX,lookupsY)))
    xvals <- xvals[idxs]
    yvals <- yvals[idxs]
  }
  rm(list=setdiff(ls(),c('xvals','yvals','decreasing')))
  function(x) {
    x <- as.numeric(x)
    if(decreasing) {
      x <- -x
    }
    nknot <- length(xvals)
    yhat <- rep(NA_real_,length(x))
    nonNA <- !is.na(x)
    if(length(xvals)<=1) {
      yhat[nonNA] = yvals[[1]]
    } else {
      ## step version
      #lookups <- pmin(length(xvals),pmax(1,findInterval(x,xvals)))
      #yhat[nonNA] <- yvals[lookups[nonNA]]
      ## linearly interpolated version
      yhat[nonNA] <- stats::approx(x=xvals,y=yvals,
                                   yleft=yvals[[1]],
                                   yright=yvals[[nknot]],
                                   xout=x[nonNA])$y
    }
    yhat
  }
}


#' Build a monotone transform
#' 
#' Depends on stats
#'
#' @param x numeric vector input
#' @param y numeric vector (same lenght as x) target output
#' @param k integer if not null approximate number of knots to prune to
#' @return monotone function mapping x to y
#' 
#' @examples
#' 
#' d <- data.frame(x=c(15,1,-12,NA,5,NaN))
#' d$y <- -log(d$x)
#' m <- monotoneXform(d$x,d$y)
#' d2 <- data.frame(x=-20:20)
#' d2$pred <- m(d2$x)
#' # ggplot() + geom_point(data=d,mapping=aes(x=x,y=y)) + 
#' #  geom_line(data=d2,mapping=aes(x=x,y=pred),color='blue')
#' 
#' @export
#'
monotoneXform <- function(x, y, k= NULL) {
  xi <- monotoneXformD(x,y,decreasing=FALSE,k=k)
  ei <- mean((y-xi(x))^2, na.rm= TRUE)
  if(ei<=0) {
    return(xi)
  }
  xd <- monotoneXformD(x,y,decreasing=TRUE,k=k)
  ed <- mean((y-xd(x))^2, na.rm= TRUE)
  if(ei<=ed) {
    return(xi)
  }
  return(xd)
}


#' Build a one variable GAM transform
#' 
#' Depends on mgcv and stats
#'
#' @param x numeric vector input
#' @param y numeric vector (same lenght as x) target output
#' @param k spline degree (leave null for automatic fit)
#' @param family mgcv/stats family object specifying the distribution and link to use in fitting 
#' @return increasing function mapping x to y
#' 
#' @examples
#' 
#' d <- data.frame(x=0.05*(1:100))
#' d$y <- sin(d$x)
#' m <- gamXform(d$x,d$y)
#' d$pred <- m(d$x)
#' # ggplot(data=d,mapping=aes(x=x)) + geom_point(aes(y=y)) +
#' #   geom_line(aes(y=pred),color='blue')
#' 
#' d$yCat <- d$y>0.1
#' mC <- gamXform(d$x, d$yC, family= stats::binomial(link='logit'))
#' d$predC <- mC(d$x)
#' # ggplot(data=d,mapping=aes(x=predC,color=yCat)) + geom_density()
#' 
#' @export
#'
gamXform <- function(x, y, 
                     k= NULL,
                     family= stats::gaussian()) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  good <- (!is.na(x)) & (!is.na(y))
  x <- x[good]
  y <- y[good]
  m <- NULL
  if(requireNamespace('mgcv', quietly = TRUE)) {
    tryCatch({
      form <- 'y~s(x)'
      if(!is.null(k)) {
        form <- paste0('y~s(x,k=',k,')')
      }
      m <- mgcv::gam(as.formula(form), data=data.frame(x=x,y=y), family=family)
      },
      error = function(e) { NULL })
  }
  if(is.null(m)) {
    m <- glm(y~x, data=data.frame(x=x,y=y), family=family)
  }
  rm(list=setdiff(ls(),c('m')))
  function(x) {
    x <- as.numeric(x)
    yhat <- rep(NA_real_,length(x))
    nonNA <- !is.na(x)
    yhat[nonNA] <- predict(m, newdata= data.frame(x=x[nonNA]), type='response')
    yhat
  }
}
