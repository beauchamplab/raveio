
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweek
  server_tools <- get_default_handlers(session = session)

  # Run analysis once the following input IDs are changed
  # This is used by auto-recalculation feature
  server_tools$run_analysis_onchange(
    component_container$get_input_ids(c(
      "electrode_text", "baseline_choices",
      "analysis_ranges", "condition_groups"
    ))
  )

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      # Invalidate previous results (stop them because they are no longer needed)
      if(!is.null(local_data$results)) {
        local_data$results$invalidate()
        ravedash::logger("Invalidating previous run", level = "trace")
      }


      # Collect input data
      settings <- component_container$collect_settings(ids = c(
        "electrode_text", "baseline_choices", "condition_groups", "analysis_ranges"
      ))

      pipeline$set_settings(.list = settings)

      #' Run pipeline without blocking the main session
      #' The trick to speed up is to set
      #' `async=TRUE` will run the pipeline in the background
      #' `shortcut=TRUE` will ignore the dependencies and directly run `names`
      #' `names` are the target nodes to run
      #' `scheduler="none"` will try to avoid starting any schedulers and
      #' run targets sequentially. Combined with `callr_function=NULL`,
      #' scheduler's overhead can be removed.
      #' `type="smart"` will start `future` plan in the background, allowing
      #' multicore calculation
      results <- pipeline$run(
        as_promise = TRUE,
        scheduler = "none",
        type = "smart",
        callr_function = NULL,
        progress_title = "Calculating in progress",
        async = TRUE,
        check_interval = 0.1,
        shortcut = TRUE,
        names = c(
          "settings",
          names(settings),
          "requested_electrodes", "analysis_ranges_index", "cond_groups",
          "bl_power", "collapsed_data"
        )
      )


      local_data$results <- results
      ravedash::logger("Scheduled: ", pipeline$pipeline_name,
                       level = 'debug', reset_timer = TRUE)

      results$promise$then(
        onFulfilled = function(...){
          ravedash::logger("Fulfilled: ", pipeline$pipeline_name,
                           level = 'debug')
          shidashi::clear_notifications(class = "pipeline-error")
          local_reactives$update_outputs <- Sys.time()
          return(TRUE)
        },
        onRejected = function(e, ...){
          msg <- paste(e$message, collapse = "\n")
          if(inherits(e, "error")){
            ravedash::logger(msg, level = 'error')
            ravedash::logger(traceback(e), level = 'error', .sep = "\n")
            shidashi::show_notification(
              message = msg,
              title = "Error while running pipeline", type = "danger",
              autohide = FALSE, close = TRUE, class = "pipeline-error"
            )
          }
          return(msg)
        }
      )

      return()

    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  # (Optional) check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }
      new_repository <- pipeline$read("repository")
      if(!inherits(new_repository, "rave_prepare_power")){
        ravedash::logger("Repository read from the pipeline, but it is not an instance of `rave_prepare_power`. Abort initialization", level = "warning")
        return()
      }
      ravedash::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

      # check if the repository has the same subject as current one
      old_repository <- component_container$data$repository
      if(inherits(old_repository, "rave_prepare_power")){

        if( !attr(loaded_flag, "force") &&
            identical(old_repository$signature, new_repository$signature) ){
          ravedash::logger("The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization", level = "debug", use_glue = TRUE)
          return()
        }
      }

      # TODO: reset UIs to default

      # Reset preset UI & data
      component_container$reset_data()
      component_container$data$repository <- new_repository
      component_container$initialize_with_new_data()

      # Reset outputs
      shidashi::reset_output("collapse_over_trial")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )





  # Register outputs
  output$collapse_over_trial <- shiny::renderPlot({
    shiny::validate(
      shiny::need(
        length(local_reactives$update_outputs) &&
          !isFALSE(local_reactives$update_outputs),
        message = "Please run the module first"
      )
    )
    shiny::validate(
      shiny::need(
          isTRUE(local_data$results$valid),
        message = "One or more errors while executing pipeline. Please check the notification."
      )
    )

    collapsed_data <- pipeline$read(var_names = "collapsed_data")
    repository <- pipeline$read(var_names = "repository")

    time_points <- repository$time_points
    frequencies <- repository$frequency

    data <- collapsed_data[[1]]$collasped$range_1
    image(t(data$freq_time), x = time_points[data$cube_index$Time], y = frequencies[data$cube_index$Frequency])

  })




}
