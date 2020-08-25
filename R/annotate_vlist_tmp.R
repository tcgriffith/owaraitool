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

annotate_vlist = function(vlist.new, bangumi_map){

  message("... Basic Info")
  vlist.new.anno=annotate_basic(vlist.new, bangumi_map, FALSE)

  message("... Thumbnails")
  vlist.new.anno.pic=download_thumbnails(vlist.new.anno)

  message("... Generate Text")
  vlist.new.anno.final= gen_text(vlist.new.anno.pic)

  message("... Tags")
  vlist.new.anno.final2=fetch_tags(vlist.new.anno.final)

  return(vlist.new.anno.final2)
}



annotate_basic=function(vlist.new, bangumi_map, imgur=TRUE) {

  if(nrow(vlist.new) ==0){
    message("# no post to annotate")
    return(NULL)
  }

  vlist.new.anno <- vlist.new
  class(vlist.new.anno$created) <- "POSIXct"

  vlist.new.anno <- vlist.new.anno %>%
    mutate(up = author) %>%
    mutate(bangumi = tolower(owaraitool:::getbangumi2(title, bangumi_map))) %>%
    mutate(title_bk = title,
           title = gsub("【.*?】", "", title_bk)) %>%
    mutate(airdate = owaraitool:::getyearsdf(title)) %>%
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
    mutate(date=format(date,"%Y-%m-%d")) %>%
    mutate(categories=zmz,
           bangumis=bangumi)


    ## unable to vectorize, use for lists
    vlist.new.anno$cid = NA
  vlist.new.anno$imgur = NA
  vlist.new.anno$tags = NA

  return(vlist.new.anno)

}

download_thumbnails = function(vlist.new.anno) {

  pbapply::pboptions(type="txt")
  vlist.new.anno$imgur=
    pbapply::pbsapply(vlist.new.anno$pic,function(url){
      path=owaraitool:::api_upload_github(url, "~/GIT/owaraisite/static/tmpimg/")
    })

  return(vlist.new.anno)
}

fetch_tags = function(vlist.new.anno) {


  pbapply::pboptions(type="txt")
  vlist.new.anno$tags=pbapply::pbsapply(vlist.new.anno$aid, function(aid){
    return(I(owaraitool::api_getbilitags(aid)))
  })

  return(vlist.new.anno)
}

gen_text = function(vlist.new.anno){
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
