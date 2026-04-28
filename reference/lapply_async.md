# Run [`lapply`](https://rdrr.io/r/base/lapply.html) in parallel

Uses
[`lapply_async2`](https://dipterix.org/dipsaus/reference/lapply_async2.html),
but allows better parallel scheduling via
[`with_future_parallel`](with_future_parallel.md). On 'Unix', the
function will fork processes. On 'Windows', the function uses strategies
specified by `on_failure`

## Usage

``` r
lapply_async(
  x,
  FUN,
  FUN.args = list(),
  callback = NULL,
  ncores = NULL,
  on_failure = "multisession",
  ...
)
```

## Arguments

- x:

  iterative elements

- FUN:

  function to apply to each element of `x`

- FUN.args:

  named list that will be passed to `FUN` as arguments

- callback:

  callback function or `NULL`. When passed as function, the function
  takes one argument (elements of `x`) as input, and it suppose to
  return one string character.

- ncores:

  number of cores to use, constraint by the `max_worker` option (see
  [`raveio_getopt`](http://dipterix.org/ravepipeline/reference/raveio-option.md));
  default is the maximum number of workers available

- on_failure:

  alternative strategy if fork process is disallowed (set by users or on
  'Windows')

- ...:

  passed to
  [`lapply_async2`](https://dipterix.org/dipsaus/reference/lapply_async2.html)

## Examples

``` r

if(!is_on_cran()) {
library(raveio)

# ---- Basic example ----------------------------
lapply_async(1:16, function(x) {
  # function that takes long to fun
  Sys.sleep(1)
  x
})

# With callback
lapply_async(1:16, function(x){
  Sys.sleep(1)
  x + 1
}, callback = function(x) {
  sprintf("Calculating|%s", x)
})

# With ncores
pids <- lapply_async(1:16, function(x){
  Sys.sleep(0.5)
  Sys.getpid()
}, ncores = 2)

# Unique number of PIDs (cores)
unique(unlist(pids))

# ---- With scheduler ----------------------------
# Scheduler pre-initialize parallel workers and temporary
# switches parallel context. The workers ramp-up
# time can be saved by reusing the workers.
#
with_future_parallel({

  # lapply_async block 1
  pids <- lapply_async(1:16, function(x){
    Sys.sleep(1)
    Sys.getpid()
  }, callback = function(x) {
    sprintf("lapply_async without ncores|%s", x)
  })
  print(unique(unlist(pids)))

  # lapply_async block 2
  pids <- lapply_async(1:16, function(x){
    Sys.sleep(1)
    Sys.getpid()
  }, callback = function(x) {
    sprintf("lapply_async with ncores|%s", x)
  }, ncores = 4)
  print(unique(unlist(pids)))

})


}
#> NOT_CRAN is TRUE/true (not on CRAN)
#> [1] 7448 7449
#> [1] 7527 7528
#> [1] 7527 7528

```
