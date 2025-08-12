# validate

rand_string <- function(length = 50){
  pid <- as.integer(Sys.getpid())
  now <- as.numeric(Sys.time() - as.POSIXlt(Sys.Date()), units = "secs")
  now <- sprintf("%.24f", now)
  now <- strsplit(now, "\\.")[[1]]
  now2 <- strsplit(now[[2]], "")[[1]]
  now <- as.integer(c(
    paste(now2[c(1,5,9,13,17,21) + 3], collapse = ""),
    paste(now2[c(1,5,9,13,17,21) + 2], collapse = ""),
    paste(now2[c(1,5,9,13,17,21) + 1], collapse = ""),
    paste(now2[c(1,5,9,13,17,21)], collapse = ""),
    now[[1]]
  ))
  now <- rev(as.integer(now))

  dict0 <- dipsaus::digest(paste(pid, now), algo = "xxhash32", seed = pid)
  dict1 <- dipsaus::digest(paste(pid, now, dict0), algo = "xxhash32", seed = now[[1]])
  dict2 <- dipsaus::digest(paste(pid, now, dict1), algo = "murmur32", seed = sum(now))
  dict3 <- dipsaus::digest(paste(pid, now, dict1, dict2), algo = "xxhash64",
                           seed = strtoi(sprintf("0x%s", substr(dict2, 1, 7))))

  dict <- strsplit(paste0(dict3, dict2, dict1), "")[[1]]
  # dict <- c(dict, letters, LETTERS, 0:9)

  paste(sample(dict, size = length, replace = TRUE), collapse = "")
  # c(dict1, dict2, dict3)
}

