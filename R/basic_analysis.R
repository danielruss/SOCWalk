## this is really ugly: I get warning if these variable (columns from a data frame)
## are not declared as global variables.
utils::globalVariables(c("11-1011", "99-9999","soccer_results","soccer_codes",
                         "noc2011","sw_codes","soc2010","crosswalked_codes",
                         "Id","soccer_scores","score","sw_scores"))

#' returns the top n results from a nested data frame
#'
#' @param data  data
#' @param col the column
#' @param codes_to ccc
#' @param score_to ss
#' @param n n
#'
#' @return a mutated copy of the data
#' @importFrom rlang :=
#' @export
#'
top_n_results <- function(data,col,codes_to,score_to,n=3){
  data %>% dplyr::mutate(
    ## sort the data...
    {{ col }} := purrr::map( {{ col }}, dplyr::arrange,-score),
    ## get the top 3 soc2010 and scores
    {{ codes_to }} := purrr::map( {{ col }}, ~.x %>% dplyr::slice_head(n=3) %>% dplyr::pull(code) ),
    {{ score_to }} := purrr::map( {{ col }}, ~.x %>% dplyr::slice_head(n=3) %>% dplyr::pull(score) )
  )
}

#' wraps in function for use in pipe useful
#'
#' @param col1 col1
#' @param col2 col2
#' @param top if TRUE, return TRUE only if the top scoring code is in col2
#'
#' @return whether or not the value(s) from col1 is in col2
#' @export
#'
is_in <- function(col1,col2,top=FALSE){
  if (top){
    return(col1[1] %in% col2)
  }
  any(col1 %in% col2)
}

soc_agree <- function(n){
  function(col1,col2,top=FALSE){
    col1 = unique( stringr::str_pad( stringr::str_sub(col1,1,n),7,side="right","0") )
    col2 = unique( stringr::str_pad( stringr::str_sub(col2,1,n),7,side="right","0") )
    is_in(col1,col2,top)
  }
}


#' get the n-digit level of agreement...
#'
#' @param col1 String -- The column title of SOCWalk codes
#' @param col2 String -- The column of expert assigned codes
#' @param top  logical -- Should we only include the top SOCWalk code (default=FALSE)
#'
#' @return rv
#'
soc_agree_5d <- soc_agree(6)
#' @describeIn soc_agree_5d soc agree 3d
#' @export
soc_agree_3d <- soc_agree(4)
#' @describeIn soc_agree_5d soc agree 2d
#' @export
soc_agree_2d <- soc_agree(2)



#' Nest SOCwalk results
#'
#' @param data the data
#' @param cols the cols you want to nest
#' @param new_col_name the col name for the nested data
#'
#' @return a tibble with the data nested
#' @export
#' @importFrom rlang :=
nest_results <- function(data,cols,new_col_name){
  data %>%
    tidyr::pivot_longer({{cols}}, names_to = "code",values_to = "score") %>%
    dplyr::group_by(Id) %>%
    tidyr::nest({{ new_col_name}}:=c('code','score'))
}

#' Title
#'
#' @param xw The crosswalk
#' @param sw_input   the input for SOCWalk
#' @param sw_results  The socwalk results
#' @param n  the number of top responses from socwalk to maintain
#'
#' @return a tibble of socwalk results..
#' @export
#' @importFrom rlang :=
model_analysis <- function(xw,sw_input,sw_results,n=3){

  ######
  #  FROM INPUT: get soccer(top n)/xw results
  ######
  tmp <- nest_results(sw_input,`11-1011`:`99-9999`,soccer_results)
  tmp <- tmp %>% top_n_results(soccer_results,soccer_codes,soccer_scores) %>%
    dplyr::select(-soccer_results) %>%
    dplyr::mutate(crosswalked_codes=socR::crosswalk(noc2011,xw))

  ######
  #  FROM RESULTS: get sw results
  ######
  sw_results <- nest_results(sw_results,`11-1011`:`99-9999`,sw_results)
  sw_results <- sw_results %>%
    top_n_results(sw_results,sw_codes,sw_scores,n) %>%
    dplyr::select(-sw_results)
  ######
  #  JOIN
  ######
  tmp <- tmp %>% dplyr::inner_join(sw_results,by="Id")

  #####
  #  some boolean flags...
  #####
  tmp <- tmp %>% dplyr::ungroup() %>% dplyr::mutate(
    soccer_agree1= purrr::map2_lgl(soccer_codes,soc2010,is_in,TRUE),
    soccer_agree3= purrr::map2_lgl(soccer_codes,soc2010,is_in,FALSE),
    soccer_agree_5d_1= purrr::map2_lgl(soccer_codes,soc2010,soc_agree_5d,TRUE),
    soccer_agree_5d_3= purrr::map2_lgl(soccer_codes,soc2010,soc_agree_5d,FALSE),
    sw_agree1= purrr::map2_lgl(sw_codes,soc2010,is_in,TRUE),
    sw_agree3= purrr::map2_lgl(sw_codes,soc2010,is_in,FALSE),
    sw_agree_5d_1= purrr::map2_lgl(sw_codes,soc2010,soc_agree_5d,TRUE),
    sw_agree_5d_3= purrr::map2_lgl(sw_codes,soc2010,soc_agree_5d,FALSE),
    coder_overlaps_xw = purrr::map2_lgl(crosswalked_codes,soc2010,is_in,FALSE),
    sw_crosswalk_agree1 = purrr::map2_lgl(sw_codes,crosswalked_codes,is_in,TRUE),
    sw_crosswalk_agree3 = purrr::map2_lgl(sw_codes,crosswalked_codes,is_in,FALSE),
    n_crosswalk_job = purrr::map_int(crosswalked_codes,length),
    max_soccer_score = purrr::map_dbl(soccer_scores, ~ .x[1]),
    max_sw_score = purrr::map_dbl(sw_scores, ~ .x[1])
  )
  tmp
}
