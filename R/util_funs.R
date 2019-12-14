

#' Get years from vector
#'
#' @param vector_character character vector containing possible date
#' @param date_format date format
#'
#' @return vector of date
#' @export
#'
#'
getyearsdf <- function(vector_character,date_format="%y%m%d"){


  df <- data.frame(text = vector_character)

  df.tmp <- df %>%
    mutate(year1 = stringr::str_extract(text, "[0-9]{6,8}")) %>%
    mutate(year2 = stringr::str_extract(
      text,
      "[0-9]{2,4}[-\\.][0-9]{1,2}[-\\.][0-9]{1,2}"
    )) %>%
    mutate(year3 = ifelse(is.na(year1), year2, year1)) %>%
    mutate(year4 =
             lubridate::parse_date_time(
               x = year3,
               orders = c("ymd", "y-m-d", "y.m.d", "Ymd")
    )) %>%
    mutate(date=year4) %>%
    pull(date)

  return(df.tmp)
}


getbangumi2 = function(vector, bangumi_map) {
  vector = gsub(" +", "", vector)
  vector = tolower(vector)

  pattern = paste0("(", paste0(tolower(bangumi_map$V1), collapse = "|"), ")")
  test = stringr::str_match_all(vector, pattern)
  bangumis = sapply(test, function(mat){
    if (length(mat) == 0) return("其他")
    else{
      return(bangumi_map$V2[max(match(mat[, 1], bangumi_map$V1))])
    }
  })
  return(bangumis)
}


get_zmz= function(vector, mid.df){
  pattern = paste0("(", paste0(tolower(mid.df$zmz), collapse = "|"), ")")

  test = stringr::str_match_all(vector, pattern)
  zmz = sapply(test, function(mat) {
    return(mid.df$zmz[max(match(mat[, 1], mid.df$zmz))])
  })
  return(zmz)
}


get_bangumi_img=function(bangumi, bangumi_map2){
  return(bangumi_map2$V2[match(bangumi, bangumi_map2$V1)])
}

#' Title get_existing_aid
#'
#' @param ... list of formatted posts
#'
#' @return aids, character vector
#' @export
#'
get_existing_aid = function(...) {
  pathlist = list(...)

  post.l = lapply(pathlist, function(fpath) {
    return(list.files(fpath))
  })

  post.v = unlist(post.l)

  oldaid <- gsub('^.*-(.*).md$', '\\1', post.v)

  return(oldaid)
}

