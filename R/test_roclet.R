register_tags(tests = parse.value)
tests_roclet <- function() {
  new_roclet(list, "tests")
}

roc_process.tests <- function(roclet, parsed, base_path,
                              options = list()) {
  have_tst <-  sapply(parsed[["blocks"]], function(x) "tests" %in% names(x))
  blocks <- mapply(function(prs, hastst) {
    if(hastst)
      prs
    else
      NULL
  }, SIMPLIFY=FALSE, prs = parsed[[2]], hastst = have_tst)

  blocks <- blocks[!sapply(blocks, is.null)]

  fils <-  sapply(blocks, function(x) x$srcref$filename)
  blocks <- lapply(blocks, function(x) x$tests)
  names(blocks) <- fils
  if(length(blocks) > 1)
    blocks <- tapply(blocks, fils, function(x) do.call(paste, c(sep="\n\n", x)))
  blocks
}

roc_output.tests <- function(roclet, results, base_path,
                             options = list(),
                             check = TRUE) {
  test_path <- testthat_path(base_path)
  uses_testthat <- length(test_path) > 0
  if (!uses_testthat) {
    test_path <- file.path(base_path, "tests")
  }
  if(!dir.exists(file.path(test_path)))
    dir.create(file.path(test_path))

  fil <- normalizePath(file.path(test_path, "tests_roxygen.R"), mustWork = FALSE)
  con <- file(fil, open = "w")
  on.exit(close(con))
  txt <- c(made_by("#"),
           mapply(paste, x = sapply(names(results),
                                    test_file_ref),
                  y = results,
                  sep="\n\n"))
  writeLines(txt,
             con = con,
             sep="\n")
}

test_file_ref<- function( fil ) {
  paste("\n## Tests generated from roxygen comments in file:", file.path("R", basename(fil)))
}

# same logic as devtools::uses_testthat()
testthat_path <- function(base_path) {
  paths <- c(
    file.path(base_path, "inst", "tests"),
    file.path(base_path, "tests", "testthat")
  )
  paths[dir.exists(paths)]
}
