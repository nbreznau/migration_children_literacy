# load_packages.R
# Attach a vector of packages, installing any that are missing first.
# Used at the top of every script so that a replicator needs no extra setup.

load_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) install.packages(missing)
  invisible(lapply(pkgs, library, character.only = TRUE))
}
