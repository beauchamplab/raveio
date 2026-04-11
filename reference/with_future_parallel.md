# Enable parallel computing provided by 'future' package within the context

Enable parallel computing provided by 'future' package within the
context

## Usage

``` r
with_future_parallel(
  expr,
  env = parent.frame(),
  quoted = FALSE,
  on_failure = "multisession",
  max_workers = NA,
  ...
)
```

## Arguments

- expr:

  the expression to be evaluated

- env:

  environment of the `expr`

- quoted:

  whether `expr` has been quoted; default is false

- on_failure:

  alternative 'future' plan to use if forking a process is disallowed;
  this usually occurs on 'Windows' machines; see details.

- max_workers:

  maximum of workers; default is automatically set by
  `raveio_getopt("max_worker",1L)`

- ...:

  additional parameters passing into
  [`make_forked_clusters`](https://dipterix.org/dipsaus/reference/make_forked_clusters.html)

## Value

The evaluation results of `expr`

## Details

Some 'RAVE' functions such as [`prepare_subject_power`](rave-prepare.md)
support parallel computing to speed up. However, the parallel computing
is optional. You can enable it by wrapping the function calls within
`with_future_parallel` (see examples).

The default plan is to use 'forked' R sessions. This is a convenient,
fast, and relative simple way to create multiple R processes that share
the same memories. However, on some machines such as 'Windows' the
support has not yet been implemented. In such cases, the plan fall backs
to a back-up specified by `on_failure`. By default, `on_failure` is
`'multisession'`, a heavier implementation than forking the process, and
slightly longer ramp-up time. However, the difference should be marginal
for most of the functions.

When parallel computing is enabled, the number of parallel workers is
specified by the option `raveio_getopt("max_worker", 1L)`.

## Examples

``` r
library(raveio)

demo_subject <- as_rave_subject("demo/DemoSubject", strict = FALSE)

if(dir.exists(demo_subject$path)) {
  with_future_parallel({
    prepare_subject_power("demo/DemoSubject")
  })
}
```
