#' Get a dataframe of matchups data
#'
#' @param conn a conn object created by `ff_connect()`
#'
#' @examples
#' \donttest{
#' try({ # try only shown here because sometimes CRAN checks are weird
#'   yahoo_conn <- ff_connect(platform = "yahoo", league_id = "77275", token = NULL)
#'   ff_schedule(yahoo_conn)
#' }) # end try
#' }
#' @describeIn ff_schedule Yahoo: Returns Schedule data.
#' @export
ff_schedule.yahoo_conn <- function(conn) {
  # get a list of team_keys from franchises
  franchises <- ff_franchises(conn)
  team_keys <- paste(conn$league_key, ".t.", franchises$franchise_id, sep = "")
  team_keys_string <- paste(team_keys, collapse = ",")

  response <- glue::glue("teams;team_keys={team_keys_string}/matchups") %>%
    yahoo_getendpoint(conn)

  matchup_nodes <- response$xml_doc %>%
    xml2::xml_find_all("//matchup")
  week <- matchup_nodes %>%
    xml2::xml_find_all("./week") %>%
    xml2::xml_integer()

  franchise_nodes <- matchup_nodes %>%
    xml2::xml_find_first(".//team")
  opponent_nodes <- matchup_nodes %>%
    purrr::map(~ xml2::xml_find_all(.x, ".//team")[2])

  franchise_id <- franchise_nodes %>%
    xml2::xml_find_first("./team_id") %>%
    xml2::xml_integer()
  opponent_id <- opponent_nodes %>%
    purrr::map(~ xml2::xml_find_first(.x, "./team_id")) %>%
    purrr::map(~ xml2::xml_integer(.x)) %>%
    unlist()

  franchise_score <- franchise_nodes %>%
    xml2::xml_find_first("./team_points/total") %>%
    xml2::xml_double()

  opponent_score <- opponent_nodes %>%
    purrr::map(~ xml2::xml_find_first(.x, "./team_points/total")) %>%
    purrr::map(~ xml2::xml_double(.x)) %>%
    unlist()

  df_matchups <- tibble::tibble(
    week = week,
    franchise_id = franchise_id,
    franchise_score = franchise_score,
    opponent_id = opponent_id,
    opponent_score = opponent_score
  )

  df_matchups <- df_matchups %>%
    dplyr::mutate(result = dplyr::case_when(
      .data$franchise_score > .data$opponent_score ~ "W",
      .data$franchise_score < .data$opponent_score ~ "L",
      .data$franchise_score == .data$opponent_score ~ "T",
      TRUE ~ NA_character_
    ))
  return(df_matchups)
}
