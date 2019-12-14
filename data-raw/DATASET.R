## code to prepare `DATASET` dataset goes here

library(dplyr)
# mid.df
mid.df = jsonlite::fromJSON(here::here("inst/extdata/author_list.json"))


# bangumi_map
bangumi_map =
  data.table::fread(here::here("inst/extdata/bangumi_list.txt"), header =
                      FALSE)
bangumi_map$V1 = tolower(bangumi_map$V1)


# geinin_map


geinin_map =
  data.table::fread(fill =TRUE, sep2=" ",here::here("inst/extdata/geinin_list.txt"), header =
                      FALSE)
geinin_map =
  geinin_map %>%
  tidyr::separate_rows(V2, sep=",") %>%
  mutate(a=tolower(V2),
         b=V1)  %>%
  select(V1=a, V2=b)

# middf=mid.df

usethis::use_data(mid.df, overwrite=TRUE)
#
#
usethis::use_data(bangumi_map,overwrite=TRUE)
#
#
usethis::use_data(geinin_map, overwrite=TRUE)
