library(testthat)
test_that("RAVEVariable", {
  var <- raveio:::RAVEVariable$new(name = "x")

  env <- new.env()
  env$bound <- letters

  var$use_constraints(raveio::new_constraints(
    type = "string",
    assertions = list(
      "bound" = quote({
        if(isTRUE(.x %in% bound)) { return(TRUE) }
        "Nah"
      })
    )
  ))

  expect_error(var$validate())
  var$set_value("a", validate = FALSE)
  var$get_value()

  expect_true(var$validate(env = env))
  expect_error(var$validate())

  x <- var$store()
  expect_true(inherits(x, "raveio_store.RAVEVariable"))
  var2 <- raveio:::RAVEVariable$new()$restore(x)

  expect_true(var2$validate(env = env))
  expect_error(var2$validate())

  expect_error(var$set_value("b"))
  expect_equal(var$get_value(), "a")

  expect_no_error(var$set_value("b", env = env))
  expect_equal(var$get_value(), "b")

  expect_equal(var2$get_value(), "a")
})

test_that("RAVEVariableCollection", {

  env <- new.env()
  env$bound <- letters

  collection <- raveio::new_variable_collection(name = "coll")

  var <- raveio:::RAVEVariable$new(name = "var")
  var$use_constraints(raveio::new_constraints(
    type = "string",
    assertions = list(
      "bound" = quote({
        if(isTRUE(.x %in% bound)) { return(TRUE) }
        "Nah"
      })
    )
  ))
  collection$add_variable("var", var)

  expect_error(collection$validate())
  expect_error({ collection[["var"]] <- "a" })
  collection["var", env = env] <- list(var = "a")

  expect_true(collection$validate(env = env))
  expect_error(collection$validate())

  expect_equal(collection$get_value("var"), "a")
  expect_equal(collection["var"], list(var = "a"))

  x <- collection$store()
  coll2 <- raveio:::RAVEVariableCollection$new()$restore(x)

  expect_true(coll2$validate(env = env))
  expect_error(coll2$validate())

  expect_equal(coll2$get_value("var"), "a")
  expect_equal(coll2["var"], list(var = "a"))

  coll2 <- collection$clone(deep = TRUE)
  coll2$set_value(id = "var", value = "b", env = env)
  expect_equal(collection$get_value("var"), "a")
  expect_equal(coll2$get_value("var"), "b")

  coll2$add_variable("var2", var = "a")
  coll2$use_constraints(quote({
    var <- .x$var
    var2 <- .x$var2
    if( !is.unsorted(c(var, var2)) ) {
      return("var < var2 is not TRUE")
    }
    return(TRUE)
  }))

  expect_true(coll2$validate(env = env))

  coll2$add_variable("var2", var = "c")
  expect_error(coll2$validate(env = env))
})

test_that("Nested RAVEVariableCollection", {

  # c1 > list(c2 -> list(list(c4)), list(v1 -> list(v2, list(c5 -> v3)), c3))
  c1 <- raveio::new_variable_collection(name = "c1", explicit = FALSE)
  c2 <- raveio::new_variable_collection(name = "c2", explicit = FALSE)
  c3 <- raveio::new_variable_collection(name = "c3", explicit = FALSE)
  c4 <- raveio::new_variable_collection(name = "c4", explicit = FALSE)
  c5 <- raveio::new_variable_collection(name = "c5", explicit = FALSE)

  v1 <- raveio::new_constrained_variable(name = "v1", initial_value = "v1")
  v2 <- raveio::new_constrained_variable(name = "v2", initial_value = "v2")
  v3 <- raveio::new_constrained_variable(name = "v3", initial_value = "v3")

  c1$add_variable("data", list(
    c2 = c2$add_variable(
      "data", list(
        list(c4),
        list(v1$set_value(
          list(v2 = v2, c5 = list(c5 = c5$add_variable("data", v3)), c3 = c3)
        ))
      )
    )
  ))

  self <- c1
  private <- c1$.__enclos_env__$private

  x <- c1$store()
  xunlist <- unlist(x)
  for(nm in names(xunlist)) {
    testthat::expect_false(R6::is.R6(xunlist[[nm]]), label = sprintf("xunlist[['%s']]", nm))
  }


  c10 <- raveio::new_variable_collection()$restore(x)
  # c1
  testthat::expect_true(inherits(c10, "RAVEVariableCollectionWrapper"))
  testthat::expect_equal(c10$name, c1$name)
  testthat::expect_true(c10$has_variable("data"))

  c10_data <- c10$get_value("data")
  c1_data <- c1$get_value("data")

  testthat::expect_identical(.subset2(c1_data$c2, "impl"), .subset2(c2, "impl"))
  testthat::expect_equal(names(c1_data), "c2")
  testthat::expect_equal(names(c10_data), "c2")


  c20 <- c10_data$c2
  # c2
  testthat::expect_true(inherits(c20, "RAVEVariableCollectionWrapper"))
  testthat::expect_equal(c20$name, c2$name)
  testthat::expect_true(c20$has_variable("data"))

  c20_data <- c20$get_value("data")
  c2_data <- c2$get_value("data")

  testthat::expect_length(object = c20_data, length(c2_data))

  c40 <- c20_data[[1]][[1]]
  v10 <- c20_data[[2]][[1]]

  # c4
  testthat::expect_true(inherits(c40, "RAVEVariableCollectionWrapper"))
  testthat::expect_equal(c40$name, c4$name)
  testthat::expect_equal(c40$varnames, c4$varnames)
  testthat::expect_length(c40$varnames, 0)

  # v10
  l1 <- v1$get_value()
  l10 <- v10$get_value()

  testthat::expect_true(inherits(v10, "RAVEVariable"))
  testthat::expect_equal(v10$name, v1$name)
  testthat::expect_equal(names(l1), names(l10))

  v20 <- l10$v2
  c30 <- l10$c3
  c50 <- l10$c5$c5

  # v2
  testthat::expect_true(inherits(v20, "RAVEVariable"))
  testthat::expect_equal(v20$name, v2$name)
  testthat::expect_equal(v20[], v2[])

  # c3
  testthat::expect_true(inherits(c30, "RAVEVariableCollectionWrapper"))
  testthat::expect_equal(c30$name, c3$name)
  testthat::expect_equal(c30$varnames, c3$varnames)
  testthat::expect_length(c30$varnames, 0)

  # c5
  testthat::expect_true(inherits(c50, "RAVEVariableCollectionWrapper"))
  testthat::expect_equal(c50$name, c5$name)
  testthat::expect_equal(c50$varnames, c5$varnames)

  v30 <- c50$get_value("data", get_definition = TRUE)

  # v3
  testthat::expect_true(inherits(v30, "RAVEVariable"))
  testthat::expect_equal(v30$name, v3$name)
  testthat::expect_equal(v30[], v3[])
  # c1 > list(c2 -> list(list(c4)), list(v1 -> list(v2, list(c5 -> v3)), c3))

})


test_that("RAVEVariableBinding", {
  v <- raveio::new_constrained_binding(
    name = "v1", quoted = TRUE,
    expr = quote({
      b + 1
    }),
    constraints = "number",
    lower = 1,
    na.ok = FALSE,
    null.ok = FALSE
  )

  # if(exists("b")) { rm("b") }
  expect_error(v$get_value())
  b <- NA
  expect_true(is.na(v$get_value()))
  expect_error(v$validate())

  env <- new.env()
  env$b <- 1
  rm(b)

  expect_equal(v$get_value(env = env), env$b + 1)
  expect_true(v$validate(env = env))

  env$b <- NA
  expect_error(v$validate(env = env))

  v2 <- raveio:::restore_list(v$store())
  expect_error(v2$validate(env = env))
  expect_error(v2$validate())

  b <- NA
  expect_true(is.na(v2$get_value()))
  expect_error(v2$validate())

  env$b <- 1
  rm(b)
  expect_equal(v2$get_value(env = env), env$b + 1)
  expect_true(v2$validate(env = env))


})
