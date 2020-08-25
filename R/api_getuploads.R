#' api_getuploads
#'
#' @param mid bilibili member id
#' @param kw keyword for filtering results
#'
#' @return a dataframe of all uploaded videos of mid
#' @export
#'
#' @examples
#' api_getuploads(5382023,kw="aaa")
#'
api_getuploads <- function(mid, kw = "") {
  fpurl = paste0(
    "https://api.bilibili.com/x/space/arc/search?mid=",
    mid,
    "&ps=50&tid=0&pn=1&keyword=",
    kw,
    "&order=pubdate&jsonp=jsonp"
  )

  test2 <- fromJSON_fix(fpurl)
  vlist.df <- test2$data$list$vlist
  # message(paste0("pages received: ",test2$data$pages))

  total_pages=ceiling(test2$data$page$count/test2$data$page$ps)

  if (total_pages > 1)
  {
    for (i in 2:total_pages) {
      Sys.sleep(1)
      aurl =

      paste0(
        "https://api.bilibili.com/x/space/arc/search?mid=",
        mid,
        "&ps=50&tid=0&pn=",
        i,
        "&keyword=",
        kw,
        "&order=pubdate&jsonp=jsonp"
      )

      ajson <- fromJSON_fix(aurl)
      avlist <-  ajson$data$list$vlist
      vlist.df <- rbind(vlist.df, avlist)
    }
  }
  return(vlist.df)
}




#' Title api_getuploads, only return first page.
#'
#' @param mid bilibili member id
#' @param kw keyword for search
#'
#' @return
#' @export
#'
api_getuploads_fp = function (mid, kw = "", ps=5)
{
  fpurl = paste0(
    "https://api.bilibili.com/x/space/arc/search?mid=",
    mid,
    "&ps=",ps,"&tid=0&pn=1&keyword=",
    kw,
    "&order=pubdate&jsonp=jsonp"
  )
  test2 = fromJSON_fix(fpurl)
  vlist.df = test2$data$list$vlist
  return(vlist.df)
}

