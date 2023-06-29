#' @name rave-server
#' @title Install and configure 'RAVE' server as background service using
#' shiny-server
#' @description Works on 'Linux' and 'Mac' only.
#' @param url 'URL' to shiny-server 'ZIP' file to download
#' @param ports integer vectors or character, indicating the port numbers to
#' host 'RAVE' instances a valid port must be within the range from 1024 to
#' 65535.
#' @param user user to run the service as; default is the login user
#' @param rave_version internally used; might be deprecated in the future
#' @returns nothing
#' @examples
#' \dontrun{
#'
#' # OS-specific. Please install R package `rpymat` first
#'
#' # Install rave-server
#' rave_server_install()
#'
#' # Let port 17283-17290 to host RAVE instance
#' rave_server_configure(ports = "17283-17290")
#'
#' }
#' @export
rave_server_install <- function(
  url = "https://github.com/rstudio/shiny-server/archive/refs/tags/v1.5.18.987.zip"
){

  if(!dipsaus::package_installed("rpymat")) {
    stop("Package `rpymat` must be installed first.")
  }
  catgl("Check if local environment has been set up", level = "TRACE")
  rpymat::ensure_rpymat(verbose = FALSE)

  catgl("Check nodejs & cmake", level = "TRACE")
  rpymat::add_packages(c("nodejs", "cmake"))

  # create a temporary directory to compile
  inst_path <- tempfile(pattern = "shiny-server-installer")
  if(file.exists(inst_path)) {
    unlink(inst_path, recursive = TRUE)
  }
  dir_create2(inst_path)

  catgl("Downloading shiny-server...", level = "TRACE")
  utils::download.file(url, destfile = file.path(inst_path, "sssrc.zip"))

  utils::unzip(file.path(inst_path, "sssrc.zip"), exdir = inst_path, overwrite = TRUE)

  dir_path <- list.dirs(inst_path, full.names = FALSE, recursive = FALSE)
  dir_path <- dir_path[grepl("^shiny-server-", dir_path)]

  if(!length(dir_path)) {
    stop("Cannot find folder starting with shiny-server after downloading & unzip.")
  }
  dir_path <- normalizePath(file.path(inst_path, dir_path[[1]]), mustWork = TRUE)
  build_path <- dir_create2(file.path(dir_path, "tmp"))


  old_wdir <- getwd()
  setwd(build_path)
  on.exit({ setwd(old_wdir) })

  catgl("Configuring NodeJS", level = "TRACE")
  rpymat::run_command("npm install ../")

  catgl("Configuring main service: shiny-server", level = "TRACE")
  rpymat::run_command("npm install ../")

  dest_dir <- dir_create2(
    file.path(R_user_dir('raveio', which = "config"), "rave-server")
  )
  rpymat::run_command('cmake -DCMAKE_INSTALL_PREFIX="{dest_dir}" ../', use_glue = TRUE)
  rpymat::run_command('make')
  rpymat::run_command('make install')

  catgl("RAVE-server should be installed...", level = "TRACE")
  message("Please run `rave_server_configure` to configure the server")


}

#' @rdname rave-server
#' @export
rave_server_configure <- function(
  ports = 17283,
  user = Sys.info()[["user"]],
  rave_version = c("1", "2")
){
  if(!dipsaus::package_installed("rpymat")) {
    stop("Please install R package `rpymat` first, then run `rave_server_install()` to install rave-server before generating configuration files.")
  }
  # shiny_server_path <- "/opt/rave-server/shiny-server"
  # node_path <- "/opt/homebrew/bin/node"
  catgl("Using rave ver-", rave_version, level = "TRACE")
  rave_version <- match.arg(rave_version)
  ports <- as.integer(dipsaus::parse_svec(ports))
  ports <- ports[!is.na(ports) & ports > 1023 & ports <= 65535]
  if(!length(ports)) {
    stop("Invalid ports found. Please enter valid port range to run RAVE instances")
  }

  # get shiny server path
  root_dir <- file.path(R_user_dir('raveio', which = "config"), "rave-server")
  shiny_server_path <- file.path(root_dir, "shiny-server")
  if(!dir.exists(shiny_server_path)) {
    stop("Please install the rave-server first.")
  }

  shiny_server_path <- normalizePath(shiny_server_path)
  catgl("Found shiny-server: ", shiny_server_path, level = "TRACE")

  os_type <- dipsaus::get_os()
  catgl("Initializing with OS: ", os_type, level = "TRACE")

  dir_create2(file.path(root_dir, "bookmarks"))
  dir_create2(file.path(root_dir, "conf"))
  dir_create2(file.path(root_dir, "logs"))
  dir_create2(file.path(root_dir, "bin"))
  dir_create2(file.path(root_dir, "app"))

  catgl("Generating configuration file for shiny-server with port(s): ",
        dipsaus::deparse_svec(ports), level = "TRACE")
  snippet_path <- system.file("rave-server", "shiny-server-snippet",
                              package = "raveio")
  snippet <- paste(readLines(snippet_path), collapse = "\n")

  conf_str <- paste(sapply(ports, function(port) {
    glue(snippet, .open = "${", .close = "}")
  }), collapse = "\n\n")

  conf_str <- paste(c(
    sprintf("run_as %s;", Sys.info()[['user']]),
    conf_str
  ), collapse = "\n")


  conf_path <- file.path(root_dir, "conf", "rave-server.conf")
  writeLines(conf_str, conf_path)

  # Create app
  catgl("Creating rave application... ", level = "TRACE")
  app_path <- file.path(root_dir, "app")
  if( rave_version == "1" ) {
    dir_create2(file.path(app_path, "rave-1.0", "main"))
    writeLines(
      "rave::start_rave()",
      file.path(app_path, "rave-1.0", "main", "app.R")
    )
    dir_create2(file.path(app_path, "rave-1.0", "preprocess"))
    writeLines(
      "rave::rave_preprocess()",
      file.path(app_path, "rave-1.0", "preprocess", "app.R")
    )
  } else {
    .NotYetImplemented()
  }

  # Create shell script to launch shiny-server with rave configuration
  catgl("Creating bash scheduler... ", level = "TRACE")
  cmd <- rpymat::run_command(sprintf(
    "exec node '%s/lib/main.js' %s",
    shiny_server_path,
    shQuote(normalizePath(conf_path), type = 'sh')
  ), workdir = "~", dry_run = TRUE, print_cmd = FALSE)

  s <- rpymat::cmd_build(cmd)

  # s <- c(
  #   '#!/bin/sh',
  #   sprintf(
  #     "exec %s '%s/lib/main.js' %s",
  #     shQuote(node_path, type = "sh"),
  #     shiny_server_path,
  #     shQuote(normalizePath(conf_path), type = 'sh')
  #   )
  # )
  bin_path <- file.path(root_dir, "bin", "rave-server")
  writeLines(
    s, bin_path, sep = "\n"
  )

  caveat <- ""

  if(os_type == 'darwin') {

    catgl("Creating OSX service: rave-server", level = "TRACE")

    # generate plist on OSX
    plist_template <- system.file("rave-server", "com.rave.server.plist",
                                  package = "raveio")
    plist_str <- paste(readLines(plist_template), collapse = "\n")

    rave_bin <- normalizePath(file.path(root_dir, "bin", fsep = "/"))
    PATH <- "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:"
    plist_path <- file.path(root_dir, "conf", "com.rave.server.plist")
    writeLines(
      glue(plist_str, .open = "${", .close = "}"),
      plist_path
    )
    # load the service
    try({
      system("launchctl stop rave-server",
             ignore.stderr = TRUE)
    }, silent = TRUE)
    try({
      system(sprintf("launchctl unload -w %s", shQuote(normalizePath(plist_path))),
             ignore.stderr = TRUE)
    }, silent = TRUE)
    cmd <- sprintf("launchctl load -w %s", shQuote(normalizePath(plist_path)))
    message("Registering service:\n  ", cmd)
    system(cmd)

    try({
      system("launchctl stop rave-server",
             ignore.stderr = TRUE)
    }, silent = TRUE)

    caveat <- paste0('Please run the following shell command to launch rave-server:\n\n\tlaunchctl start rave-server\n\nor to use R command:\n\n\tsystem("launchctl start rave-server")\n\nto start the service. Once rave-server is started, open your browser, go to any of the following URL:\n\t', paste(sprintf("http://127.0.0.1:%s", ports), collapse = "\n\t"), '\n\nTo stop the server, use shell command:\n\n\tlaunchctl stop rave-server\n\nor to use R command:\n\n\tsystem("launchctl stop rave-server")')
  }


  writeLines(caveat, file.path(root_dir, "README.txt"))
  catgl("Done configuration: rave-server. {caveat}", level = "INFO")
}

