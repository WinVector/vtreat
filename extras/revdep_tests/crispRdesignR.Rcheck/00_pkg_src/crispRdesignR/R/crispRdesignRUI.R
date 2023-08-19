#' @export

##crispRdesignRUI
crispRdesignRUI <- function(max_gtf_size = 150) {
  # Saves current shiny.maxRequestSize option and prepares to reset it when the function exits
  original_shinymaxrequest <- options("shiny.maxRequestSize")
  on.exit(options(original_shinymaxrequest))
  ## Increases the maximum file size that can be uploaded to Shiny to accomadate .gtf files
  options(shiny.maxRequestSize=max_gtf_size*1024^2)
  # Prepares and runs RunShiny.R script containing the crispRdesignR User Interface
  app_path <- system.file("apps", "RunShiny.R", package = "crispRdesignR")
  shiny::runApp(app_path)
}
