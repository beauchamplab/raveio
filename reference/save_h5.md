# Save objects to 'HDF5' file without trivial checks

Save objects to 'HDF5' file without trivial checks

## Usage

``` r
save_h5(
  x,
  file,
  name,
  chunk = "auto",
  level = 4,
  replace = TRUE,
  new_file = FALSE,
  ctype = NULL,
  quiet = FALSE,
  ...
)
```

## Arguments

- x:

  an array, a matrix, or a vector

- file:

  path to 'HDF5' file

- name:

  path/name of the data; for example, `"group/data_name"`

- chunk:

  chunk size

- level:

  compress level from 0 - no compression to 10 - max compression

- replace:

  should data be replaced if exists

- new_file:

  should removing the file if old one exists

- ctype:

  data type such as "character", "integer", or "numeric". If set to
  `NULL` then automatically detect types. Note for complex data please
  store separately the real and imaginary parts.

- quiet:

  whether to suppress messages, default is false

- ...:

  passed to other `LazyH5$save`

## Value

Absolute path of the file saved

## See also

[`load_h5`](load_h5.md)

## Examples

``` r
file <- tempfile()
x <- array(1:120, dim = 2:5)

# save x to file with name /group/dataset/1
save_h5(x, file, '/group/dataset/1', chunk = dim(x))
#> /var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T//RtmpOZ6CXm/file196524d16376 => /group (Group Created)
#> /var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T//RtmpOZ6CXm/file196524d16376 => /group/dataset (Group Created)
#> /var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T//RtmpOZ6CXm/file196524d16376 => 1 (Dataset Created)
#> /var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T//RtmpOZ6CXm/file196524d16376 => 1 (Dataset Removed)
#> /var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T//RtmpOZ6CXm/file196524d16376 => 1 (Dataset Created)

# read data
y <- load_h5(file, '/group/dataset/1')
y[]
#> , , 1, 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
#> 
#> , , 2, 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    7    9   11
#> [2,]    8   10   12
#> 
#> , , 3, 1
#> 
#>      [,1] [,2] [,3]
#> [1,]   13   15   17
#> [2,]   14   16   18
#> 
#> , , 4, 1
#> 
#>      [,1] [,2] [,3]
#> [1,]   19   21   23
#> [2,]   20   22   24
#> 
#> , , 1, 2
#> 
#>      [,1] [,2] [,3]
#> [1,]   25   27   29
#> [2,]   26   28   30
#> 
#> , , 2, 2
#> 
#>      [,1] [,2] [,3]
#> [1,]   31   33   35
#> [2,]   32   34   36
#> 
#> , , 3, 2
#> 
#>      [,1] [,2] [,3]
#> [1,]   37   39   41
#> [2,]   38   40   42
#> 
#> , , 4, 2
#> 
#>      [,1] [,2] [,3]
#> [1,]   43   45   47
#> [2,]   44   46   48
#> 
#> , , 1, 3
#> 
#>      [,1] [,2] [,3]
#> [1,]   49   51   53
#> [2,]   50   52   54
#> 
#> , , 2, 3
#> 
#>      [,1] [,2] [,3]
#> [1,]   55   57   59
#> [2,]   56   58   60
#> 
#> , , 3, 3
#> 
#>      [,1] [,2] [,3]
#> [1,]   61   63   65
#> [2,]   62   64   66
#> 
#> , , 4, 3
#> 
#>      [,1] [,2] [,3]
#> [1,]   67   69   71
#> [2,]   68   70   72
#> 
#> , , 1, 4
#> 
#>      [,1] [,2] [,3]
#> [1,]   73   75   77
#> [2,]   74   76   78
#> 
#> , , 2, 4
#> 
#>      [,1] [,2] [,3]
#> [1,]   79   81   83
#> [2,]   80   82   84
#> 
#> , , 3, 4
#> 
#>      [,1] [,2] [,3]
#> [1,]   85   87   89
#> [2,]   86   88   90
#> 
#> , , 4, 4
#> 
#>      [,1] [,2] [,3]
#> [1,]   91   93   95
#> [2,]   92   94   96
#> 
#> , , 1, 5
#> 
#>      [,1] [,2] [,3]
#> [1,]   97   99  101
#> [2,]   98  100  102
#> 
#> , , 2, 5
#> 
#>      [,1] [,2] [,3]
#> [1,]  103  105  107
#> [2,]  104  106  108
#> 
#> , , 3, 5
#> 
#>      [,1] [,2] [,3]
#> [1,]  109  111  113
#> [2,]  110  112  114
#> 
#> , , 4, 5
#> 
#>      [,1] [,2] [,3]
#> [1,]  115  117  119
#> [2,]  116  118  120
#> 
```
