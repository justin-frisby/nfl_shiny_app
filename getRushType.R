
#' NFL Team Rushing Tendencies
#'
#' A wrapper function that groups rushing plays by offenses and rushing types
#'
#' @param season_s int: indicates the season of interest
#' @param run_Type str: indicates a column in nflfastR to group rushes on (ie. run_gap, quarter, run_location,etc)
#' @param start_week int: indicates the starting week of the selected season
#' @param end_week int: indicates the ending week of the selected season
#' @param graph boolean: indicates if you wish to graph output or return df, default is true
#' @return df: dataframe containing the team, ypa, run type, attempts and epa
#' @export
#' @examples
#' \dontrun{
#' get_rush_type(2020, run_gap, 1, 17, graph = TRUE)
#'}
#' @import magrittr
#' @import dplyr
#' @import nflfastR
#' @import ggplot2
#' @import tidyverse
#' @import purrr
#' @import rlang

get_rush_type <- function(season, run_type, start_week, end_week, graph = TRUE)
  {
  seasons = season
  df <-  purrr::map_df(seasons, function(x) {
    readRDS(
      url(
        glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
      )
    )
  })
  df_temp <- df %>%
    filter(play_type == "run"
           & week >= as.integer(start_week)
           & week <= as.integer(end_week)) %>%
    left_join(fast_scraper_roster(c(2020)), by = c("rusher_player_id" = "gsis_id")) %>%
    filter(position == "RB") %>%
    dplyr::group_by_at(c(run_type,'posteam')) %>%
    summarize(rush_yards = sum(yards_gained),
              attempts = n(),
              yards_per_attempt = round((rush_yards / attempts),1),
              mean_epa = round(sum(epa),2))%>%
    left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
    rename(fac_var = !!run_type)  %>%
    mutate(fac_var = if_else(is.na(fac_var),"middle",fac_var)) %>%
    ungroup() %>%
    select(team_logo_espn, team_nick, fac_var,attempts,yards_per_attempt, mean_epa)
  if (graph == TRUE){
    graph_temp <- df_temp %>%
      ggplot(mapping = aes(x = mean_epa, y = attempts))+
      geom_image(aes(image = team_logo_espn), size = 0.087)+
      facet_wrap(~ fac_var, nrow = 2) +
      labs(title = "EPA and Rushing Attempts by Rushing Type", x = "Estimated Points Added (sum)", y ="Rushing Attempts")}
  else{
    return (df_temp)
  }
}


