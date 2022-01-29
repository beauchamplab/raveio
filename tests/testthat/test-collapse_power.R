test_that("collapse_power", {
  # Generate a 4-mode tensor array
  x <- filearray::filearray_create(
    tempfile(), dimension = c(16, 100, 20, 5),
    partition_size = 1
  )
  x[,,,1:4] <- rnorm(160000 / 5 * 4)
  dnames <- list(
    Frequency = 1:16,
    Time = seq(0, 1, length.out = 100),
    Trial = 1:20,
    Electrode = 1:5
  )
  dimnames(x) <- dnames

  on.exit({
    x$delete(force = TRUE)
  })
  analysis_index_cubes <- list(
    list(
      Frequency = 1:16,
      Time = 1:100,
      Trial = 1:20,
      Electrode = 1:4
    ),
    list(
      Trial = 1:20,
      Frequency = 1:16,
      Electrode = 1,
      Time = 1:100
    ),
    list(
      Trial = 1:10,
      Frequency = 1:4,
      Time = 1:20,
      Electrode = 1:4
    )
  )
  re <- collapse_power(x, analysis_index_cubes)

  for(jj in seq_along(analysis_index_cubes)){
    cube <- analysis_index_cubes[[jj]]
    cube_data <- x[cube$Frequency, cube$Time, cube$Trial, cube$Electrode, drop = FALSE]
    actual <- re[[jj]]
    nms <- names(actual)
    tmp <- cbind( grepl("freq", nms), grepl("time", nms), grepl("trial", nms), grepl("elec", nms) )
    for(ii in seq_along(nms)){
      nm <- nms[[ii]]
      sel <- tmp[ii,]
      if(any(sel)){
        # print(nm)
        expected <- apply(cube_data, which(sel), mean)
        act <- actual[[nm]]
        expect_equal(dim(act), dim(expected))
        expect_true(all(range(act / expected - 1) < 1e-6))
      }
    }

  }

  re2 <- collapse_power(x[], analysis_index_cubes)

  for(jj in seq_along(analysis_index_cubes)){
    cube <- analysis_index_cubes[[jj]]
    cube_data <- x[cube$Frequency, cube$Time, cube$Trial, cube$Electrode, drop = FALSE]
    actual2 <- re2[[jj]]
    nms <- names(actual2)
    tmp <- cbind( grepl("freq", nms), grepl("time", nms), grepl("trial", nms), grepl("elec", nms) )

    for(ii in seq_along(nms)){
      nm <- nms[[ii]]
      sel <- tmp[ii,]
      if(any(sel)){
        # print(nm)
        expected <- apply(cube_data, which(sel), mean)
        act2 <- actual2[[nm]]
        expect_equal(dim(act2), dim(expected))
        expect_true(all(range(act2 / expected - 1) < 1e-6))
      }
    }


  }
})
