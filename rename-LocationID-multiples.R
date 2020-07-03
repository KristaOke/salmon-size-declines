#' Rename LocationID for locations that appear in multiple regions
#'
#' @param temp.dat data frame of size data
#'
#' @return data frame with updated LocationIDs
#' @export
#'
rename_LocationID_multiples <- function(temp.dat) {
  
  #===============================================================================
  #Create the input matrix
  years <- min(temp.dat$sampleYear):max(temp.dat$sampleYear)
  n.years <- length(years)
  
  locs <- unique(temp.dat$LocationID)
  n.locs <- length(locs)
  
  #===============================================================================
  #Rename Location ID's that appear in more than one Region
  l <- 1
  for(l in 1:n.locs) {
    temp.regions <- unique(temp.dat$SASAP.Region[temp.dat$LocationID==locs[l]])
    n.temp.regions <- length(temp.regions)
    if(n.temp.regions > 1) {
      r <- 1
      for(r in 1:n.temp.regions) {
        temp.dat$LocationID[temp.dat$LocationID==locs[l] & temp.dat$SASAP.Region==temp.regions[r]] <- paste0(temp.regions[r],'_',locs[l])
      }#next r
    }#endif
  }#next l
  
  return(temp.dat)
}