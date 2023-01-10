#' Title Get Hall of Fame (HOF) Voting from Baseball Reference
#' @description acquire HOF voting results by year
#' @param year the year of the Hall of Fame Voting
#'
#' @return a tibble of the ballot, their stats, and how many votes they received
#' @import dplyr,rvest,janitor,stringr
#' @export
#'
#' @examples \donttest{
#' try(mlb_hof_voting_bref(2022))
#' }


mlb_hof_voting_bref <- function(year) {

url <- paste0("https://www.baseball-reference.com/awards/hof_",year,".shtml")

df <- url %>% xml2::read_html() %>% rvest::html_nodes('table') %>%
  rvest::html_table(trim=T) %>% .[[1]] %>% tibble::as_tibble()

df <- df %>% janitor::row_to_names(1) %>% janitor::clean_names() %>%
  dplyr::rename(year_on_ballet = yo_b,
                hof_monitor = ho_fm,
                hof_standard = ho_fs,
                games_played = g,
                years_pro = yrs,
                ops_plus = ops_2,
                era_plus = era_2,
                games_pitched = g_2,
                ha = h_2,
                hra = hr_2,
                bba = bb_2) %>%
  dplyr::mutate(removed_from_ballet = ifelse(stringr::str_starts(name,"X-"),1,0),
                elected_in_future = ifelse(stringr::str_detect(name,"(HOF)"),1,0),
                name = stringr::str_remove(name,"(X-)"),
                name = stringr::str_remove(name,"(HOF)"),
                name = stringr::str_squish(name),
                percent_vote = stringr::str_remove(percent_vote,"(%)"),
                percent_vote = as.numeric(stringr::str_squish(percent_vote)))

return(df)

}
