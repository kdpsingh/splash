#' Splash
#'
#' When supplied with the path to an `h2o` mojo (`.zip`) file, this function
#' builds an interactive document (`.Rmd` file), provides an `h2o-genmodel.jar`
#' file, and launches the interactive document using `rmarkdown::render()`.
#'
#' @param mojo_file_path The file path to the h2o model-optimized Java object
#'   (MOJO) zip file.
#' @param output_file The name of the Rmd file to create.
#' @param overwrite Whether to overwrite the output_file. Defaults to `FALSE`.
#' @param title The YAML header `title`` for the Rmd output file.
#' @param author The YAML header `author`` for the Rmd output file.
#' @param date The YAML header `date`` for the Rmd output file.
#' @param rmd_output The YAML header `output`` for the of Rmd output file.
#' @param runtime The YAML runtime. Currently, only `shiny` is supported.
#' @param rmd_run Whether to run the Rmd output file after creating it. Defaults
#'   to `TRUE`.
#'
#' @export
splash = function(mojo_file_path,
                  output_file = 'index.Rmd',
                  overwrite = FALSE,
                  title = '',
                  author = '',
                  date = '',
                  rmd_output = 'html_document',
                  runtime = 'shiny',
                  rmd_run = TRUE) {

  if (!file.exists(mojo_file_path)) {
    stop('The mojo_file_path of ', mojo_file_path, ' does not exist.')
  }

  if (tools::file_ext(mojo_file_path) != 'zip') {
    stop('The mojo_file_path of ', mojo_file_path, ' does not point to an h2o MOJO object, which must be in the form of a zip file.')
  }

  mojo_file_name = basename(mojo_file_path)
  output_path = dirname(mojo_file_path)

  if (file.exists(file.path(output_path, output_file))) {
    if (!overwrite) {
      stop('The output_file could not be overwritten. To change this, set overwrite to TRUE.')
    }
  }

  if (!file.exists(file.path(rappdirs::user_data_dir(appname = 'splash'), 'h2o-genmodel.jar'))) {
    message('Downloading h2o-genmodel.jar. This will temporarily start an h2o session on port 426.')
    download_genmodel_jar()
    message('The h2o instance on port 426 was successfully shut down.')
  }

  # copy h2o-genmodel.jar to the output_path
  file.copy(from = file.path(rappdirs::user_data_dir(appname = 'splash'), 'h2o-genmodel.jar'),
            to = file.path(output_path, 'h2o-genmodel.jar'))

  # set up work dir, path & output file name

  # fn = output_fn
  # setwd(path)

  # Fill in libraries necessary for your h2o code:
  library_input = '' # "library(shiny)\nlibrary(h2o)\n"

  ### WRITE YAML TO FILE
  yaml_info = paste(
    "---",
    "\ntitle:", shQuote(title),
    "\nauthor:", shQuote(author),
    "\ndate:", shQuote(date),
    "\noutput:", rmd_output,
    "\nruntime:", runtime,
    "\n---")

  write(yaml_info,
        file = file.path(output_path, output_file))

  # Writing libraries
  write("```{r context='setup',include=FALSE, cache=FALSE}",
        file = file.path(output_path, output_file),
        append=TRUE)

  write(library_input,
        file = file.path(output_path, output_file),
        append=TRUE)

  write("```",
        file = file.path(output_path, output_file),
        append=TRUE)

  # Now is start of r code:
  write("\n```{r echo=FALSE}",
        file = file.path(output_path, output_file),
        append=TRUE)

  ### Header
  header =
    "wellPanel(style = \"background: #FFF;\",
    h4('Please enter the following information.'),
    flowLayout("

  write(header,
        file = file.path(output_path, output_file),
        append=TRUE)


  ### GET NAMES AND TYPES OF EACH H2O OBJ COLUMN
  # Extract the domain info out of model.ini

  unzip(mojo_file_path, files="model.ini", exdir = output_path)
  ini_file = ini::read.ini(file.path(output_path, 'model.ini'))

  num_columns = as.integer(ini_file$info$n_columns) - 1
  num_domains = as.integer(ini_file$info$n_domains)

  x = readLines(file.path(output_path, 'model.ini'))

  num = 1
  v = numeric(0)
  colname = c()

  while(!is.na(x[num])) {
    y = x[num]
    if (y == "[columns]") {
      num = num + 1
      for (i in seq_len(num_columns)) {
        colname = c(colname, x[num])
        num = num + 1
      }
      # minus the extra line here to avoid going one line too far
      # (look at num + 1 at the end of this entire loop)
      num = num - 1
    } else if (y == "[domains]") {
      num = num + 1
      for (i in seq_len(num_domains)) {
        line = x[num]
        colnum = as.numeric(strsplit(line, split=":")[[1]][1])
        v = c(v, colnum+1)
        num = num + 1
      }
      # minus the extra line here to avoid going one line too far
      # (look at num + 1 at the end of this entire loop)
      num = num - 1
    }
    num = num + 1
  }

  # create list of column types
  coltype = rep("Numeric", num_columns)
  for (i in v)
    coltype[i] = "Enum"

  # unzip the domain files
  dfl = c()
  en_collen = length(v)
  for (i in seq_len(en_collen)) {
    domain_file=sprintf("domains/d%03d.txt", i - 1)
    dfl=c(dfl, domain_file)
    unzip(mojo_file_path, files = domain_file, exdir = output_path)
  }

  # maps enum col num. to its domain file
  # maps col name to its type
  names(dfl) = v
  names(coltype) = colname

  # find a way to mark the last column so we don't
  # add  comma at the end of it
  collen = length(colname)

  ### MAIN LOOP
  idx = 1
  for (cn in colname) {
    # get the type of col
    ct = coltype[[cn]]

    # data input
    id = shQuote(cn)
    cn_quoted = shQuote(cn)

    if (ct == "Numeric") {
      # fill in string for numericInput
      data = paste0("\tnumericInput(", id, ", ", cn_quoted, ", value = NA)")
    } else if (ct == "Enum") {
      # stringfy Enum options
      x = enum2dom(idx, dfl, output_path)
      opt = eopt2str(x)

      # fill in string for selectInput
      data = paste0("\tselectInput(", id, ", ", cn_quoted, ", ",
                    "\n\t\tc(NA, ", opt, "), ",
                    "\n\t\tselected=NA", ")")
    }

    if (idx < collen)
      data = paste0(data, ", ")

    # write to file
    write(data,
          file = file.path(output_path, output_file),
          append = TRUE)
    idx = idx + 1
  }

  ### End
  data =
    "\t),
  \tbr(),
  \tactionButton('start','Calculate')
  )"

  write(data,
        file= file.path(output_path, output_file),
        append=TRUE)

  ### H2O OUTPUT
  data =
    "
  conditionalPanel('input.start>0',
                   wellPanel(id='results', style = \"background: #FFF;\",
                   h3('h2o output: '),
                   h5(dataTableOutput('pred_score'))
                   ))"
  write(data,
        file = file.path(output_path, output_file),
        append=TRUE)

  ### H2O CALCULATION
  data =
    "
  # Reactive function to calculate h2o score
  calculate_h2o = reactive({
  \n\t# assign variables"

  write(data,
        file = file.path(output_path, output_file),
        append=TRUE)

  # assign variables
  for (cn in colname) {
    data = paste0("\t", cn, " = ")
    ct = coltype[[cn]]
    if (ct == "Numeric") {
      data = paste0(data, "as.numeric(input$", cn, ")")
    } else if (ct == "Enum") {
      data = paste0(data, "input$", cn)
    }
    write(data,
          file=file.path(output_path, output_file),
          append=TRUE)
  }

  data =
    "
  \t# create data frame
  \tx <- data.frame("
  write(data,
        file = file.path(output_path, output_file),
        append=TRUE)

  num = 1
  for (cn in colname) {
    sz = length(colname)
    tabs = "\t\t\t"
    if (num < sz) {
      write(paste0(tabs, cn, ","),
            file = file.path(output_path, output_file),
            append=TRUE)
    } else {
      write(paste0(tabs, cn, ")"),
            file = file.path(output_path, output_file),
            append=TRUE)
    }
    num = num + 1
  }

  data = paste0(
    "
  \t# call mojo
  \tpred_rf <- h2o::h2o.mojo_predict_df(frame=x,
  \t                               mojo_zip_path=", shQuote(mojo_file_name), ",
  \t                               genmodel_jar_path='h2o-genmodel.jar',
  \t                               verbose = F)
  \tpred_rf")

  write(data,
        file = file.path(output_path, output_file),
        append=TRUE)

  write("})",
        file = file.path(output_path, output_file),
        append=TRUE)

  ### WRITE FUNCTION TO OUTPUT H2O RESULT
  data =
    "
  output$pred_score = renderDataTable(calculate_h2o(),
                                      options=list(paging=FALSE,
                                                   searching=FALSE,
                                                   info=FALSE,
                                                   scrollX=TRUE
                                      ))
  "

  write(data,
        file = file.path(output_path, output_file),
        append=TRUE)

  write("```",
        file = file.path(output_path, output_file),
        append=TRUE)




  # Remove files
  unlink(file.path(output_path, 'domains'), recursive = TRUE)
  unlink(file.path(output_path, 'model.ini'))


  # Run rmd file if rmd_run is set to TRUE
  if (rmd_run) {
    rmarkdown::run(file.path(output_path, output_file))
  }
}

# this function takes in a column num of an enum (categorical) column and returns
# a vector of its options
enum2dom = function(colnum, domain_files, output_path) {
  idx = toString(colnum)
  domFile = domain_files[[idx]]
  eopt = readLines(file.path(output_path, domFile))
  eopt
}

# this function takes all the possible inputs for 'Enum'
# and puts them into a string
eopt2str = function(eopt) {
  sz = length(eopt)
  num = 1
  str = ""

  for (i in eopt) {
    str = paste0(str, shQuote(i))
    # add comma if not end of vector
    if (num < sz) { str = paste0(str, ", ") }
    num = num + 1
  }

  str = trimws(str)
}

