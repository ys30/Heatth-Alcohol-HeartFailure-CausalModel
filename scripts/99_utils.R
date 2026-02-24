# 99_utils.R
# Purpose: small helper utilities used across scripts.

save_table <- function(df, filename) {
  out <- file.path("outputs", "tables", filename)
  readr::write_csv(df, out)
  message("Saved table: ", out)
  invisible(out)
}

save_plot <- function(p, filename, width = 8, height = 5, dpi = 150) {
  out <- file.path("outputs", "figures", filename)
  ggplot2::ggsave(out, p, width = width, height = height, dpi = dpi)
  message("Saved figure: ", out)
  invisible(out)
}
