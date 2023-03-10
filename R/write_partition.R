
#' partition_by_preincubation_id
#'
#' @param tibble_of_reshape_classifications_formatted_by_preinc_assay a tibble
#' @param path_folder_DTU_format_id_partion a string
#'
#' @return Nothing. Has file creation sideeffect
#' @export
#'
partition_by_preincubation_id <- function(tibble_of_reshape_classifications_formatted_by_preinc_assay,
                                          path_folder_DTU_format_id_partion) {
  preincubation_ids_classified <- tibble_of_reshape_classifications_formatted_by_preinc_assay |>
    pull(id) |>
    unique()

  preincubation_ids_classified |>
    purrr::walk(~ create_partion_by_preincubation_id(.x,
                                                     tibble_of_reshape_classifications_formatted_by_preinc_assay,
                                                     path_folder_DTU_format_id_partion))
}


#' create_partion_by_preincubation_id
#'
#' @param id_preinc a string
#' @param tibble_of_reshape_classifications_formatted_by_preinc_assay a tibble
#' @param path_folder_DTU_format_id_partion a string
#' @param extention_id_partion a string
#'
#' @return Nothing. Has file creation sideeffect
#' @export
#'
create_partion_by_preincubation_id <- function(id_preinc,
                                               tibble_of_reshape_classifications_formatted_by_preinc_assay,
                                               path_folder_DTU_format_id_partion,
                                               extention_id_partion = ".xlsx") {
  name_id_partion <- id_preinc |>
    paste0(extention_id_partion)
  path_DTU_format_id_partioned_file <- path_folder_DTU_format_id_partion |>
    path(name_id_partion)

  if (!file.exists(path_DTU_format_id_partioned_file)) {
    path_DTU_format_id_partioned_file |>
      paste0(" DOESN'T EXIST, writing") |>
      print()

    tibble_of_reshape_classifications_formatted_by_preinc_assay |>
      filter(id == !!id_preinc) |>
      select(-id) |>
      writexl::write_xlsx(path_DTU_format_id_partioned_file)
  } else {
    path_DTU_format_id_partioned_file |>
      paste0(" EXISTS not writing") |>
      print()
  }

}
