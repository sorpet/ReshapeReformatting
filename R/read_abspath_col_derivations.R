#' col_with_abspath_basename_sans_ext
#'
#' @param tibble_with_abspath_col a tibble
#'
#' @return a tibble
#' @export
#'
col_with_abspath_basename_sans_ext <- function(tibble_with_abspath_col) {
  tibble_with_abspath_col |>
    dplyr::mutate(basename_sans_ext =  tools::file_path_sans_ext(base::basename(abs_path)))
}

#' tibble_with_abspath_col
#'
#' @param tibble_with_abspath_col a tibble
#'
#' @return a tibble
#' @export
#'
col_with_abspath_first_directory_name <- function(tibble_with_abspath_col) {
  tibble_with_abspath_col |>
    dplyr::mutate(first_directory_name =  base::basename(dirname(abs_path)))
}

#' create_cols_with_abspath_basename_and_first_dir
#'
#' @param tibble_with_abspath_col a tibble
#'
#' @return a tibble
#' @export
#'
create_cols_with_abspath_basename_and_first_dir <- function(tibble_with_abspath_col) {
  tibble_with_abspath_col |>
    col_with_abspath_first_directory_name() |>
    col_with_abspath_basename_sans_ext() |>
    identity()
}
