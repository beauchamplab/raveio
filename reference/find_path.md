# Try to find path along the root directory

Try to find `path` under root directory even if the original path is
missing; see examples.

## Usage

``` r
find_path(path, root_dir, all = FALSE)
```

## Arguments

- path:

  file path

- root_dir:

  top directory of the search path

- all:

  return all possible paths, default is false

## Value

The absolute path of file if exists, or `NULL` if missing/failed.

## Details

When file is missing, `find_path` concatenates the root directory and
path combined to find the file. For example, if path is `"a/b/c/d"`, the
function first seek for existence of `"a/b/c/d"`. If failed, then
`"b/c/d"`, and then `"~/c/d"` until reaching root directory. If
`all=TRUE`, then all files/directories found along the search path will
be returned

## Examples

``` r

root <- tempdir()

# ------ Case 1: basic use case -------

# Create a path in root
dir_create2(file.path(root, 'a'))

# find path even it's missing. The search path will be
# root/ins/cd/a - missing
# root/cd/a     - missing
# root/a        - exists!
find_path('ins/cd/a', root)
#> [1] "/private/var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T/Rtmp9va47M/a"

# ------ Case 2: priority -------
# Create two paths in root
dir_create2(file.path(root, 'cc/a'))
dir_create2(file.path(root, 'a'))

# If two paths exist, return the first path found
# root/ins/cd/a - missing
# root/cd/a     - exists - returned
# root/a        - exists, but ignored
find_path('ins/cc/a', root)
#> [1] "/private/var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T/Rtmp9va47M/cc/a"

# ------ Case 3: find all -------
# Create two paths in root
dir_create2(file.path(root, 'cc/a'))
dir_create2(file.path(root, 'a'))

# If two paths exist, return the first path found
# root/ins/cd/a - missing
# root/cd/a     - exists - returned
# root/a        - exists - returned
find_path('ins/cc/a', root, all = TRUE)
#> [1] "/private/var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T/Rtmp9va47M/cc/a"
#> [2] "/private/var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T/Rtmp9va47M/a"   
```
