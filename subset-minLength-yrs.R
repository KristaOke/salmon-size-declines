
#' Subset Data for Minimum Time Series
#'
#' @param temp.dat data frame of size data
#' @param n.min.yrs value indicating the minimum number years for datasets to include
#'
#' @return data frame subsetted to remove LocationIDs with fewer than min.yrs of observations.
#' @export
#'
subset_minLength_yrs <- function(temp.dat, n.min.yrs=NULL) {
  if(!is.null(n.min.yrs)) {
    ids.all <- unique(temp.dat$LocationID)
    n.ids.all <- length(ids.all)
  
    n.yrs.all <- vector(length=n.ids.all)
  
    i <- 1
    for(i in 1:n.ids.all) {
      n.yrs.all[i] <- length(unique(temp.dat$sampleYear[temp.dat$LocationID==ids.all[i]]))
    }
  
    # length(which(n.yrs.all>15))
    #Limit to 
    output.dat <- temp.dat[temp.dat$LocationID %in% ids.all[which(n.yrs.all>=n.min.yrs)],]
  }else {
    output.dat <- temp.dat
  }
  return(output.dat)
}