find_xlsxs_abspaths_recursive <- function(path_dir) {
  path_dir |>
    fs::dir_ls(glob = "*.xlsx", recurse = TRUE)
}
abspaths_vector_to_col_in_tibble <- function(paths_vector, col_name = "abs_path") {
  paths_vector |>
    (function(.) tibble::tibble(abs_path = .))()
}
