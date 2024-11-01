

sim_transformation_id_to_name <- function(db_con, id) {
  dplyr::tbl(db_con, "sim_transformations")
}
