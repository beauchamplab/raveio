

module_html <- function(){

  shiny::fluidPage(
    shiny::fluidRow(

      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,

            electrode_selector$ui_func(),

            comp_condition_groups$ui_func(),

            baseline_choices$ui_func(),

            comp_analysis_ranges$ui_func()

          )
        )
      ),

      shiny::column(
        width = 9L,
        shiny::div(
          class = "row screen-height overflow-y-scroll output-wrapper",
          shiny::column(
            width = 12L,
            ravedash::output_card(
              'Collapsed over frequency',
              class_body = "no-padding fill-width height-450 min-height-450 resize-vertical",
              shiny::div(
                class = 'position-relative fill',
                shiny::plotOutput(ns("collapse_over_trial"), width = '100%', height = "100%")
              )
            )
          )
        )
      )

    )
  )
}
