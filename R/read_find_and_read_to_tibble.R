#' find_xlsx_abspath_recursive_readxls_and_unnest
#'
#' @param path_dir a string
#' @param sheet_string a string
#'
#' @return a tibble
#' @export
#'
find_xlsx_abspath_recursive_readxls_and_unnest <- function(path_dir, sheet_string = 'label') {
  path_dir |>
    find_xlsxs_abspaths_recursive_and_put_in_column_in_tibble() |>
    create_cols_with_abspath_basename_and_first_dir() |>
    abspath_read_and_unnest(sheet_string)
}

#' abspath_read_and_unnest
#'
#' @param tibble_with_abspath_col a tibble
#' @param sheet_string a string
#'
#' @return a tibble
#' @export
#'
abspath_read_and_unnest <- function(tibble_with_abspath_col, sheet_string = 'label') {
  tibble_with_abspath_col |>
    mutate(xls_read = purrr::map(abs_path, readxl::read_excel, sheet = sheet_string)) |>
    tidyr::unnest(xls_read) |>
    select(-abs_path) |>
    identity()
}


#' read_in_reshape_image_classification_format
#'
#' @param path_reshape_scores a string
#' @param sheet_string a string
#'
#' @return a tibble
#' @export
#'
read_in_reshape_image_classification_format <- function(path_reshape_scores, sheet_string = 'label') {
  path_reshape_scores |>
    find_xlsx_abspath_recursive_readxls_and_unnest(sheet_string = sheet_string) |>
    # slice_day_6_in_reshape_format() |>
    pivot_longer_reshape_image_classification_format() |>
    cleanup_time_rename_and_relocate_reshape_format(sheet_string = sheet_string) |>
    identity()
}


#' read_in_and_merge_reshape_labels_and_scores
#'
#' @param path_reshape_scores a string
#'
#' @return a tibble
#' @export
#'
read_in_and_merge_reshape_labels_and_scores <- function(path_reshape_scores) {
  labels <-  path_reshape_scores |>
    read_in_reshape_image_classification_format()

  scores <- path_reshape_scores |>
    read_in_reshape_image_classification_format(sheet_string = 'score')

  labels |>
    dplyr::left_join(scores, by = c("run_id", "deck_position", "hour_candidate", "well"))
}

#' reformat_reshape_classifications_according_to_plate_assay_role
#'
#' @param reshape_classification a tibble
#' @param tibble_reshape_runs_with_preinc_id_and_plate_assay_role a tibble
#'
#' @return a tibble
#' @export
#'
reformat_reshape_classifications_according_to_plate_assay_role <- function(reshape_classification,
                                                                           tibble_reshape_runs_with_preinc_id_and_plate_assay_role) {
  reshape_classification |>
    join_with_reshape_classification_and_remove_plate_level_details(tibble_reshape_runs_with_preinc_id_and_plate_assay_role) |>
    reformat_according_to_plate_assay_role()
}

#' join_with_reshape_classification_and_remove_plate_level_details
#'
#' @param tibble_reshape_runs_with_preinc_id_and_plate_assay_role a tibble
#' @param tibble_reshape_long_format a tibble
#'
#' @return a tibble
#' @export
#'
join_with_reshape_classification_and_remove_plate_level_details <- function(tibble_reshape_runs_with_preinc_id_and_plate_assay_role,
                                                                            tibble_reshape_long_format) {
  tibble_reshape_long_format |>
    inner_join(tibble_reshape_runs_with_preinc_id_and_plate_assay_role, by = c("run_id", "deck_position")) |>
    select(-run_id, -deck_position, -barcode) |>
    relocate(id, plate_assay_role)
}

#' reformat_according_to_plate_assay_role
#'
#' @param tibble_of_reshape_runs_with_preinc_id_and_plate_assay_role a tibble
#'
#' @return a tibble
#' @export
#'
reformat_according_to_plate_assay_role <- function(tibble_of_reshape_runs_with_preinc_id_and_plate_assay_role) {
  tibble_of_reshape_runs_with_preinc_id_and_plate_assay_role |>
    pivot_according_to_plate_assay_role() |>
    convert_relocate_and_arrange_columns()
}

#' pivot_according_to_plate_assay_role
#'
#' @param tibble_with_reshape_classification_preinc_joined_format a tibble
#'
#' @return a tibble
#' @export
#'
pivot_according_to_plate_assay_role <- function(tibble_with_reshape_classification_preinc_joined_format) {
  tibble_with_reshape_classification_preinc_joined_format |>
    pivot_wider(names_from = plate_assay_role, values_from = c(label, score)) |>
    pivot_longer(
      cols = !c("id", "hour_candidate", "well", "score_candidate_only", "label_candidate_only"),
      names_to = c(".value", "hour_pathogen_T_values"),
      names_pattern = "(.*)_(T.)",
      values_drop_na = TRUE,
    )
}

#' convert_relocate_and_arrange_columns
#'
#' @param tibble_pivotted_long_reshape_preinc_joined a tibble
#'
#' @return a tibble
#' @export
#'
convert_relocate_and_arrange_columns <- function(tibble_pivotted_long_reshape_preinc_joined) {
  tibble_pivotted_long_reshape_preinc_joined |>
    relocate(label_candidate_only, score_candidate_only,
             label_pathogen_only_r1, score_pathogen_only_r1,
             label_pathogen_only_r2,score_pathogen_only_r2,
             label_candidate_pathogen, score_candidate_pathogen,
             .after = well) |>
    convert_T_values_to_hour_and_relocate() |>
    convert_well_to_row_col_and_relocate() |>
    add_pathogen_spore_conc_column_and_relocate() |>
    arrange(hour_pathogen, row, col)
}

#' convert_T_values_to_hour_and_relocate
#'
#' @param tibble_with_T_values_column a tibble
#'
#' @return a tibble
#' @export
#'
convert_T_values_to_hour_and_relocate <- function(tibble_with_T_values_column) {
  tibble_with_T_values_column |>

    mutate(
      hour_pathogen = case_when(
        hour_pathogen_T_values == "T1" ~ 0 ,
        hour_pathogen_T_values == "T2" ~ 24,
        hour_pathogen_T_values == "T3" ~ 48
      )) |>
    relocate(hour_pathogen, .after = hour_candidate) |>
    select(-hour_pathogen_T_values)
}

#' convert_well_to_row_col_and_relocate
#'
#' @param tibble_with_well_column a tibble
#'
#' @return a tibble
#' @export
#'
convert_well_to_row_col_and_relocate <- function(tibble_with_well_column) {
  tibble_with_well_column |>
    format_well_column() |>
    relocate(row, col, .after = id)
}

#' add_pathogen_spore_conc_column_and_relocate
#'
#' @param tibble_without_pathogen_spore_conc_column a tibble
#'
#' @return a tibble
#' @export
#'
add_pathogen_spore_conc_column_and_relocate <- function(tibble_without_pathogen_spore_conc_column) {
  tibble_without_pathogen_spore_conc_column |>
    mutate(pathogen_spore_conc = 2000) |>
    relocate(pathogen_spore_conc, .after = hour_pathogen)
}


#' determine_rechecks
#'
#' @param tibble_preincubation_id_formatted a tibble
#'
#' @return a tibble
#' @export
#'
determine_rechecks <- function(tibble_preincubation_id_formatted) {
  tibble_preincubation_id_formatted |>
    mutate(recheck = "n",
           recheck = ifelse(label_candidate_only == 2 | score_candidate_only < 0.55, "y_ca", recheck),
           #recheck = ifelse(label_pathogen_only == 1 | score_pathogen_only < 0.55, "y_pa", recheck),
           recheck = ifelse(score_candidate_pathogen < 0.55, "y_capa", recheck),
    )
}
