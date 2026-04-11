# Print colored messages

Print colored messages

## Usage

``` r
catgl(..., .envir = parent.frame(), level = "DEBUG", .pal, .capture = FALSE)
```

## Arguments

- ..., .envir:

  passed to [`glue`](https://glue.tidyverse.org/reference/glue.html)

- level:

  passed to [`cat2`](https://dipterix.org/dipsaus/reference/cat2.html)

- .pal:

  see `pal` in
  [`cat2`](https://dipterix.org/dipsaus/reference/cat2.html)

- .capture:

  logical, whether to capture message and return it without printing

## Value

The message as characters

## Details

The level has order that sorted from low to high: `"DEBUG"`,
`"DEFAULT"`, `"INFO"`, `"WARNING"`, `"ERROR"`, `"FATAL"`. Each different
level will display different colors and icons before the message. You
can suppress messages with certain levels by setting 'raveio' options
via `raveio_setopt('verbose_level', <level>)`. Messages with levels
lower than the threshold will be muffled. See examples.

## Examples

``` r
# ------------------ Basic Styles ---------------------

# Temporarily change verbose level for example
raveio_setopt('verbose_level', 'DEBUG', FALSE)

# debug
catgl('Debug message', level = 'DEBUG')
#> Debug message 

# default
catgl('Default message', level = 'DEFAULT')
#> Default message 

# info
catgl('Info message', level = 'INFO')
#> Info message 

# warning
catgl('Warning message', level = 'WARNING')
#> Warning message 

# error
catgl('Error message', level = 'ERROR')
#> Error message 

try({
  # fatal, will call stop and raise error
  catgl('Error message', level = 'FATAL')
}, silent = TRUE)
#> Error message 

# ------------------ Muffle messages ---------------------

# Temporarily change verbose level to 'WARNING'
raveio_setopt('verbose_level', 'WARNING', FALSE)

# default, muffled
catgl('Default message')

# message printed for level >= Warning
catgl('Default message', level = 'WARNING')
#> Default message 
catgl('Default message', level = 'ERROR')
#> Default message 


```
