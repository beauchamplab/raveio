# Back up and rename the file or directory

Back up and rename the file or directory

## Usage

``` r
backup_file(path, remove = FALSE, quiet = FALSE)
```

## Arguments

- path:

  path to a file or a directory

- remove:

  whether to remove the original path; default is false

- quiet:

  whether not to verbose the messages; default is false

## Value

`FALSE` if nothing to back up, or the back-up path if `path` exists

## Examples

``` r
path <- tempfile()
file.create(path)
#> [1] TRUE

path2 <- backup_file(path, remove = TRUE)

file.exists(c(path, path2))
#> [1] FALSE  TRUE
unlink(path2)

```
