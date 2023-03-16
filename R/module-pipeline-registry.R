#' @name module_registry
#' @title 'RAVE' module registry
#' @description Create, view, or reserve the module registry
#' @param title title of the registry, usually identical to the description
#' title in \code{'DESCRIPTION'} or \code{RAVE-CONFIG} file
#' @param repo 'Github' repository
#' @param modules characters of module ID, must only contain letters, digits,
#' underscore, dash; must not be duplicated with existing registered modules
#' @param authors a list of module authors; there must be one and only one
#' author with \code{'cre'} role (see \code{\link[utils]{person}}). This
#' author will be considered maintainer, who will be in charge if editing the
#' registry
#' @param url the web address of the repository
#' @param dry_run whether to generate and preview message content instead of
#' opening an email link
#' @param description path to \code{'DESCRIPTION'} or \code{RAVE-CONFIG} file
#' @param update whether to force updating the registry
#' @returns a registry object, or a list of registries
#' @details A 'RAVE' registry contains the following data entries: repository
#' title, name, 'URL', authors, and a list of module IDs. 'RAVE' requires
#' that each module must use a unique module ID. It will cause an issue if two
#' modules share the same ID. Therefore 'RAVE' maintains a public registry
#' list such that the module maintainers can register their own module ID
#' and prevent other people from using it.
#'
#' To register your own module ID, please use \code{add_module_registry} to
#' validate and send an email to the 'RAVE' development team.
#'
#' @examples
#'
#' library(raveio)
#'
#' # get current registries
#' get_modules_registries(FALSE)
#'
#' # create your own registry
#' module_registry(
#'   repo = "rave-ieeg/rave-pipelines",
#'   title = "A Collection of 'RAVE' Builtin Pipelines",
#'   authors = list(
#'     list("Zhengjia", "Wang", role = c("cre", "aut"),
#'          email = "dipterix@rave.wiki")
#'   ),
#'   modules = "brain_viewer"
#' )
#'
#' # If your repository is on Github and RAVE-CONFIG file exists
#' module_registry2("rave-ieeg/rave-pipelines")
#'
#' # send a request to add your registry
#' if(interactive()) {
#'
#' reg <- module_registry2("rave-ieeg/rave-pipelines")
#' add_module_registry(reg)
#'
#' }
#'
#'
NULL

#' @export
`print.rave-module-registry` <- function(x, ...) {
  s <- c(
    "[RAVE Registry]",
    sprintf("  Title: %s", x$title),
    sprintf("  Repository: %s", x$repo),
    sprintf("  URL: %s", x$url),
    sprintf("  Maintainer: %s", as.character(do.call(utils::person, x$maintainer))),
    sprintf("  Authors: "),
    paste("  -", sapply(x$authors, function(aut) {
      as.character(do.call(utils::person, aut))
    })),
    sprintf("  Modules: [%s]", paste(x$modules, collapse = ", "))
  )
  cat(s, sep = "\n")
  invisible(x)
}

#' @rdname module_registry
#' @export
module_registry <- function(
  title, repo, modules, authors,
  url = sprintf("https://github.com/%s", repo)
) {

  # check authors
  if(missing(authors)) {
    stop("`module_registry`: Authors must be specified in repository: ", repo)
  }
  maintainer <- NULL
  person <- function(
    given = NULL, family, middle = NULL, email,
    role = NULL, comment = NULL
  ) {
    list(given = given, family = family, middle = middle,
         email = email, role = role, comment = comment)
  }
  authors <- lapply(authors, function(aut) {

    aut <- do.call(person, aut)

    if(length(aut$email) != 1 || is.na(aut$email) ||
       !grepl("^[^@]+@[^@.]+\\.[^@]+$", aut$email)) {
      stop("`module_registry`: Each item in the author list must contains email - ",
           repo)
    }
    if(!length(aut$role)) {
      aut$role <- "aut"
    }
    if("cre" %in% aut$role) {
      if(!is.null(maintainer)) {
        stop("`module_registry`: A RAVE module/pipeline must have only one maintainer - ", repo)
      }
      maintainer <<- aut
    }
    aut
  })
  if(is.null(maintainer)) {
    stop("`module_registry`: A RAVE module/pipeline must have one maintainer. Please specify the role of each author. See ?person for details. Repository: ", repo)
  }
  valid_modules <- modules[!is.na(modules) & grepl("^[a-zA-Z0-9][a-zA-Z0-9_.-]*$", modules)]
  invalids <- modules[!is.na(modules) & !modules %in% valid_modules]
  if(!length(valid_modules)) {
    stop("`module_registry`: no valid module ID found. Repository: ", repo)
  }
  if(length(invalids)) {
    stop("`module_registry`: invalid module ID found in repository [", repo,
         "] - ", paste(invalids, collapse = ", "))
  }

  item <- structure(
    list(
      title = title,
      url = url,
      repo = repo,
      maintainer = maintainer,
      authors = authors,
      modules = valid_modules
    ),
    class = "rave-module-registry"
  )

  item
}

#' @rdname module_registry
#' @export
module_registry2 <- function(repo, description) {

  # repo <- "rave-ieeg/rave-pipelines"
  # description <-

  if(missing(description) || !length(description)) {
    description <- sprintf("https://raw.githubusercontent.com/%s/main/RAVE-CONFIG", repo)
  }

  if(is.character(description) && startsWith(description, "http")) {
    description <- url(description)
    on.exit({
      close(description)
    })
  }

  desc <- read.dcf(description, all = TRUE)
  desc <- as.data.frame(desc)
  modules <- trimws(c(
    unlist(strsplit(desc$InteractiveModules, ",")),
    unlist(strsplit(desc$SubPipelines, ","))
  ))
  modules <- sapply(strsplit(modules, "/"), function(x) {
    trimws(x[[length(x)]])
  })

  expr <- parse(text = desc$`Authors@R`)
  authors <- eval(expr, list(
    person = function(
      given = NULL, family = NULL, middle = NULL, email = NULL,
      role = NULL, comment = NULL
    ) {
      list(given = given, family = family, middle = middle,
           email = email, role = role, comment = comment)
    }
  ))
  if(!is.list(authors[[1]])) {
    authors <- list(authors)
  }

  urls <- unlist(strsplit(desc$URL, "[ ]{0,1}http"))
  urls <- trimws(urls)
  urls <- urls[!urls %in% ""]
  urls <- sprintf("http%s", urls)

  if(length(urls) > 1) {
    idx <- c(which(grepl("github\\.com", urls, ignore.case = TRUE)), 1)
    urls <- urls[[idx[[1]]]]
  } else {
    urls <- sprintf("https://github.com/%s", repo)
  }

  module_registry(
    title = desc$Title,
    repo = repo, modules = modules,
    authors = authors, url = urls
  )
}

validate_modules_registries <- function(registries, verbose = TRUE) {
  modules <- NULL
  registries <- lapply(registries, function(item) {
    if(verbose) {
      catgl("Loading registry: {item$repo}", level = "trace")
    }
    item <- module_registry(title = item$title, repo = item$repo, modules = item$modules, authors = item$authors, url = item$url)
    dups <- item$modules[item$modules %in% modules]
    if(length(dups)) {
      stop("Duplicated module ID found: \n  ", paste(dups, collapse = ", "),
           ". \n\nIf you are the maintainer, please modify your module ID.",
           call. = FALSE)
    }
    modules <<- item$modules
    item
  })
  repo_name <- sapply(registries, '[[', 'repo')
  names(registries) <- repo_name

  registries
}

#' @rdname module_registry
#' @export
get_modules_registries <- function(update = NA) {
  conf_root <- R_user_dir("raveio", which = "config")
  registry_path <- file.path(conf_root, "module-registry.yaml")
  updated <- TRUE
  if(!isFALSE(update)) {
    if(!is.na(update) && is.character(update)) {
      url <- update
    } else {
      url <- "https://raw.githubusercontent.com/beauchamplab/raveio/master/inst/module-registry.yaml"
    }
  } else if(file.exists(registry_path)){
    updated <- FALSE
    url <- registry_path
  } else {
    url <- system.file("module-registry.yaml", package = "raveio")
  }
  regs <- tryCatch({
    # try to download current registry file
    yaml::read_yaml(url)
  }, error = function(e) {
    updated <<- TRUE
    yaml::read_yaml(
      system.file("module-registry.yaml", package = "raveio")
    )
  })

  # validate
  regs <- validate_modules_registries(regs, verbose = FALSE)

  # save
  if( updated ) {
    dir_create2(conf_root)
    save_yaml(regs, file = registry_path)
  } else if(isTRUE(update)) {
    stop("Failed to update the RAVE module registry: Cannot access to \n  https://raw.githubusercontent.com/beauchamplab/raveio/master/inst/module-registry.yaml")
  }

  regs
}

#' @rdname module_registry
#' @export
add_module_registry <- function(title, repo, modules, authors, url,
                                dry_run = FALSE) {
  if(missing(repo)) {
    reg <- title
  } else {
    if(missing(url)) {
      url <- sprintf("https://github.com/%s", repo)
    }
    reg <- list(
      title = title,
      repo = repo,
      modules = modules,
      authors = authors,
      url = url
    )
  }
  if(!inherits(reg, "rave-module-registry")) {
    reg <- do.call(module_registry, reg)
  }
  # get current registries
  regs <- get_modules_registries(update = TRUE)
  reg_names <- names(regs)
  is_new <- !(reg$repo %in% reg_names)
  reg_old <- regs[[reg$repo]]
  regs[[reg$repo]] <- reg
  regs <- validate_modules_registries(regs)


  if(!is_new) {
    if(!identical(reg$maintainer$email, reg_old$maintainer$email)) {
      msg <- sprintf("You are trying to change the repository [%s] that is currently maintained by [%s]. We will hold your request until it is confirmed by the previous maintainer", reg$repo, reg_old$maintainer$email)
      warning(msg)
    }
  }

  # prepare the email
  from <- reg$maintainer$email
  rec <- "modules@rave.wiki"
  subj <- "Request to add/change a RAVE module registry"
  body <- paste0(
    "Dear RAVE Team,\n",
    sprintf("I am the maintainer of the module repository [%s]. Please consider %s the registry:\n\n", reg$repo, ifelse(is_new, "adding", "changing")),
    "```yml\n",
    paste(
      utils::capture.output({save_yaml(list(reg), stdout())}),
      collapse = "\n"
    ),
    "\n```\n",
    "\n",
    "This registry is generated from `raveio::add_module_registry` with no conflict/error/modification.\n\n",
    "Best,\n",
    sprintf(
      "%s, %s %s",
      paste(reg$maintainer$family, collapse = ""),
      paste(reg$maintainer$given, collapse = ""),
      paste(reg$maintainer$middle, collapse = "")
    ),
    "\n"
  )

  if(!dry_run && interactive()) {
    utils::browseURL(sprintf(
      "mailto:%s?subject=%s&body=%s",
      rec, subj, body
    ))
  }


  cat(
    sep = "\n",
    sprintf("From: %s", from),
    sprintf("To: %s", rec),
    sprintf("Title: %s", subj),
    "",
    body
  )

  message(
    "Please copy and send the above message to [", rec,
    "] with your **maintainer**'s email: ", from
  )

  invisible(reg)
}





