#' Title: Find Specific Codes in Set of Columns
#'
#' @param dat data object to search
#' @param cols columns to search
#' @param codes codes to search for
#'
#' @return a tibble with a column for the codes found and a column for whether any codes were found
#' @export
#'
#' @examples
#' dat <- tibble(
#'   id = 1:3,
#'   code1 = c("A", "B", "C"),
#'   code2 = c("B", "C", "D")
#' )
#' cd_find_ids(dat, c("code1", "code2"), c("A", "B"))
#' # A tibble: 3 x 2
#' # codes_found any_codes
#' # <list>      <lgl>
#' # 1 <chr [1]>   TRUE
#' # 2 <chr [1]>   TRUE
#' # 3 <chr [0]>   FALSE
cd_find_ids <- function(dat, cols, codes) {
  any_codes <- dat |>
    dplyr::as_tibble() |>
    dplyr::select(dplyr::matches(cols)) |>
    purrr::map(~ .x %in% codes) |>
    purrr::reduce(`|`)


  results <- dat |>
    dplyr::as_tibble() |>
    dplyr::select(dplyr::matches(cols)) |>
    dplyr::rowwise() |>
    dplyr::mutate(codes_found = list(
      dplyr::intersect(
        na.omit(dplyr::c_across(dplyr::everything())),
        codes
      )
    )) |>
    dplyr::ungroup() |>
    dplyr::select(codes_found) |>
    dplyr::mutate(any_codes = any_codes) |>
    dplyr::select(any_codes, codes_found)

  return(results)
}
