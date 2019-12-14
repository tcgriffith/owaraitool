#' Title annotate_vlist
#'
#' annotate necessary info to generate posts,
#'
#'
#' @param vlist.new
#'
#' @return vlist.new.anno
#' @importFrom dplyr %>%
#' @export
#'


annotate_vlist <- function(vlist.new, bangumi_map, imgur=TRUE) {

  if(nrow(vlist.new) ==0){
    message("# no post to annotate")
    return(NULL)
  }

  vlist.new.anno <- vlist.new
  class(vlist.new.anno$created) <- "POSIXct"

  vlist.new.anno <- vlist.new.anno %>%
    mutate(up = author) %>%
    mutate(bangumi = tolower(getbangumi2(title, bangumi_map))) %>%
    mutate(title_bk = title,
           title = gsub("【.*?】", "", title_bk)) %>%
    mutate(airdate = getyearsdf(title)) %>%
    mutate(slug = paste0(
      format(created, "%Y-%m-%d"),
      "-",
      format(airdate, "%y%m%d"),
      "-",
      aid
    )) %>%
    mutate(desc = description) %>%
    mutate(description = paste0(bangumi, "&#8226;", format(airdate, "%y%m%d"))) %>%
    #  mutate(categories=I(list()))
    mutate(date = Sys.time()) %>%
    mutate(publishdate = ifelse(
      is.na(airdate),
      format(
        lubridate::parse_date_time("20010103", orders = "Y-m-d"),
        "%Y-%m-%d"
      ),
      format(airdate, "%Y-%m-%d")
    )) %>%
    # mutate(weight = ifelse(is.na(airdate), 200000, 200000 - as.numeric(format(
    #   publishdate, "%y%m%d"
    # ))))
    mutate(weight = 200000 - as.numeric(format(date,"%y%m%d"))) %>%
    mutate(date=format(date,"%Y-%m-%d"))


  ## unable to vectorize, use for lists
  vlist.new.anno$cid = NA
  vlist.new.anno$imgur = NA
  vlist.new.anno$tags = NA


  ## have to use loop
  for (i in 1:nrow(vlist.new.anno)) {
    vlist.new.anno$cid[i] <- api_aid2cid(vlist.new.anno$aid[i])
    vlist.new.anno$tags[i] <-
      I(api_getbilitags(vlist.new.anno$aid[i]))
    vlist.new.anno$categories[i] <-
      I(list(c(
        vlist.new.anno$zmz[i], vlist.new.anno$author[i]
      )))
    vlist.new.anno$bangumis[i] <- I(list(vlist.new.anno$bangumi[i]))
    vlist.new.anno$author[i] = I(list(unique(
      c(vlist.new.anno$zmz[i], vlist.new.anno$author[i])
    )))
    Sys.sleep(1)
    #  vlist.new.anno$imgur[i] <-
  }
  message("\n...cid, tags, categories, bangumis finished, uploading front cover to imgur")

  if(imgur) {
    for (i in 1:nrow(vlist.new.anno)) {
      vlist.new.anno$imgur[i] <- api_upload_imgur(vlist.new.anno$pic[i])
    }
  }
  else {
    for (i in 1:nrow(vlist.new.anno)){
      vlist.new.anno$imgur[i] = api_upload_github(vlist.new.anno$pic[i], "~/GIT/owaraisite/static/tmpimg/")
    }
  }

  vlist.new.anno <- vlist.new.anno %>%
    mutate(
      text = paste0(
        "![](",imgur,")\n",
        "# 简介  \n",
        desc,"  \n",
        "\n[去B站观看](https://www.bilibili.com/video/av",aid,"/)\n",
        '<div class ="resp-container">',
          '<iframe class="testiframe" src="//player.bilibili.com/player.html?aid=', aid,'"", scrolling="no", allowfullscreen="true" > </iframe>',
        '</div>',
       " "
      )
    )
  return(vlist.new.anno)
}
