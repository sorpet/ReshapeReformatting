#' create_lookup_barcodes_per_reshape_run
#'
#' @param events_reshape a tibble
#'
#' @return a tibble
#' @export
#'
create_lookup_barcodes_per_reshape_run <- function(events_reshape) {
  events_reshape |>
    filter(experiment_ids == "SOP-223") |>  #only preincubation runs
    select(run_id = id, a1:e2) |>
    pivot_longer(!run_id, names_to = "deck_position", values_to = "barcode") |>
    identity()
}

#' create_lookup_barcodes_per_preincubation_id
#'
#' @param events_preincubation a tibble
#'
#' @return a tibble
#' @export
#'
create_lookup_barcodes_per_preincubation_id <- function(events_preincubation) {
  events_preincubation |>
    filter(
      !grepl("^chm",id), # chm runs doesn't have preincubation assay role's such as candidate_pathogen_T1
    ) |>
    select(id,
           candidate_pathogen_T3:pathogen_only_r2_T3,
           candidate_pathogen_T2:pathogen_only_r2_T2,
           candidate_pathogen_T1:pathogen_only_r2_T1,
           candidate_only) |>
    pivot_longer(!id, names_to = "plate_assay_role", values_to = "barcode")
}

#' find_correspondance_between_reshape_run_and_preincubation_assay
#'
#' @param lookup_barcodes_per_preincubation_id a tibble
#' @param lookup_barcodes_per_reshape_run a tibble
#'
#' @return a tibble
#' @export
#'
find_correspondance_between_reshape_run_and_preincubation_assay <- function(lookup_barcodes_per_preincubation_id,
                                                                            lookup_barcodes_per_reshape_run) {
  lookup_barcodes_per_preincubation_id |>
    left_join(lookup_barcodes_per_reshape_run, by = "barcode") |>
    relocate(id, plate_assay_role, run_id, deck_position, barcode)
}

#' create_reshape_runs_with_preinc_id_and_plate_assay_role
#'
#' @param events_reshape a tibble
#' @param events_preincubation a tibble
#'
#' @return a tibble
#' @export
#'
create_reshape_runs_with_preinc_id_and_plate_assay_role <- function(events_reshape,
                                                                    events_preincubation) {
  lookup_barcodes_per_reshape_run <- events_reshape |>
    create_lookup_barcodes_per_reshape_run()

  lookup_barcodes_per_preincubation_id <- events_preincubation |>
    create_lookup_barcodes_per_preincubation_id()

  reshape_runs_with_preinc_id_and_plate_assay_role <- lookup_barcodes_per_preincubation_id |>
    find_correspondance_between_reshape_run_and_preincubation_assay(lookup_barcodes_per_reshape_run)

}
