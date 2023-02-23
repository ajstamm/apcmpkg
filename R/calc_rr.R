#' Calculate risk ratios for model cross-bases
#' 
#' 

calc_rr <- function(data, model, cb, measure, defect, lags) {
  l <- names(d)[grepl(measure, names(d))][1:(lags + 1)] # most recent last
  
  if (grepl("pm_mean", measure)) {
    centre <- 17.80 # 20 # Q1
    atvals <- 1:37 
  } else if (grepl("pm_peak", measure)) { # peak
    centre <- 31.47 # 30 # Q1
    atvals <- 3:81
  } else if (grepl("o3_mean", measure)) { # O3
    centre <- 58.67 # 60 # Q1
    atvals <- 8:93
  } else { # O3 peak
    centre <- 78.98 # 80 # Q1
    atvals <- 15:126
  }
  plotvar <- centre + 10
  atvals <- c(atvals, centre, plotvar)
  
  cb1 <- cb
  m <- model
  
  p <- dlnm::crosspred(cb1, m, at = plotvar, cen = centre)
  # estimated effects at each lag for given measure value
  r <- data.frame(rr = t(p$matRRfit), cilow = t(p$matRRlow), 
                  cihigh = t(p$matRRhigh))
  names(r) <- c("rr", "cilow", "cihigh")
  r$lag <- rownames(r)
  # estimated cumulative effect of all lags
  r <- rbind(r, data.frame(rr = p$allRRfit, cilow = p$allRRlow, 
                           cihigh = p$allRRhigh, lag = "cum"))
  r$defect <- defect
  r$air <- measure
  r$threshold <- centre
  r$lags <- lags
  
  return(r)
}
