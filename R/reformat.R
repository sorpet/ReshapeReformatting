
#' pivot_longer_reshape_image_classification_format
#'
#' @param tibble_with_wide_reshape_format a tibble
#'
#' @return a tibble
#' @export
#'
pivot_longer_reshape_image_classification_format <- function(tibble_with_wide_reshape_format) {
  tibble_with_wide_reshape_format |>
    pivot_longer(!c("basename_sans_ext",
                    "first_directory_name",
                    "duration_minutes"),
                 names_to = "well",
                 values_to = "value") |>
    identity()
}

#' change_min_to_hour_in_reshape_image_classification_format
#'
#' @param tibble_with_min_reshape_format a tibble
#'
#' @return a tibble
#' @export
#'
change_min_to_hour_in_reshape_image_classification_format <- function(tibble_with_min_reshape_format) {
  tibble_with_min_reshape_format |>
    mutate(hour_candidate = duration_minutes/60) |>
    relocate(hour_candidate, .after = duration_minutes) |>
    select(-duration_minutes) |>
    identity()
}

#' cleanup_time_rename_and_relocate_reshape_format
#'
#' @param tibble_with_long_reshape_format_uncleaned a tibble
#' @param sheet_string a string
#'
#' @return  a tibble
#' @export
#'
cleanup_time_rename_and_relocate_reshape_format <- function(tibble_with_long_reshape_format_uncleaned, sheet_string = 'label') {
  tibble_with_long_reshape_format_uncleaned |>
    change_min_to_hour_in_reshape_image_classification_format() |>
    relocate(value, .after = well) |>
    rename(!!sheet_string := value,
           deck_position = basename_sans_ext,
           run_id = first_directory_name) |>
    identity()
}
