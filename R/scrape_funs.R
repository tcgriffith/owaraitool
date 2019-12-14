#' scrape_fp
#'
#' Scrape first 5 uploads for all mid in mid.df
#' @param mid.df
#'
#' @return scraped posts, data.frame
#' @export
#'
scrape_fp = function(mid.df) {
  vlist.l = pbapply::pblapply(1:nrow(mid.df), function(i) {
    vlist = api_getuploads_fp(mid.df$mids[i], kw = mid.df$kw[i])
    vlist$zmz = mid.df$zmz[i]
    return(vlist)
  })

  vlist.all = do.call(rbind, vlist.l)
  return(vlist.all)
}




#' Title scrape_all uploads for all mid in mid.df
#'
#' @param mid.df
#'
#' @return scraped posts, data.frame
#' @export
#'
scrape_all = function(mid.df){
  vlist.l = pbapply::pblapply(1:nrow(mid.df), function(i) {
    vlist = api_getuploads(mid.df$mids[i], kw = mid.df$kw[i])
    vlist$zmz = mid.df$zmz[i]
    return(vlist)
  })

  vlist.all = do.call(rbind, vlist.l)
  return(vlist.all)
}
