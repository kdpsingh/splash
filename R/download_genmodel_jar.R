#' Download h2o-genmodel.jar file
#'
#' This function downloads a copy of `h2o-genmodel.jar` to a local package app
#' data folder, which is then automatically copied to each mojo file path when
#' `splash()` is called. This function is automatically run the first time the
#' package is used. This function can be called directly to internally update
#' the `h2o-genmodel.jar` file. The `h2o-genmodel.jar` file is accessed from the
#' local `h2o` package installation by launching a temporary instance of an
#' `h2o` server on port 426. If a more recent version of the `h2o-genmodel.jar`
#' file is required, please update your locally installed `h2o` package.
#'
#' @export
#'
#' @examples
download_genmodel_jar = function () {
  output_folder = rappdirs::user_data_dir(appname = 'splash')

  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  h2o::h2o.init(port = 426) # h2o in telephone numbers
  #Perform a safe (i.e. error-checked) HTTP GET request to an H2O cluster with genmodel.jar URL
  #and write to jar.path.
  writeBin(utils::getFromNamespace('.h2o.doSafeGET', 'h2o')(urlSuffix = 'h2o-genmodel.jar', binary = TRUE),
           file.path(output_folder, 'h2o-genmodel.jar'),
           useBytes = TRUE)

  h2o::h2o.shutdown(FALSE)
}
