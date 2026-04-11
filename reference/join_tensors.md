# Join Multiple Tensors into One Tensor

Join Multiple Tensors into One Tensor

## Usage

``` r
join_tensors(tensors, temporary = TRUE)
```

## Arguments

- tensors:

  list of [`Tensor`](Tensor.md) instances

- temporary:

  whether to garbage collect space when exiting R session

## Value

A new [`Tensor`](Tensor.md) instance with the last dimension

## Details

Merges multiple tensors. Each tensor must share the same dimension with
the last one dimension as 1, for example, `100x100x1`. Join 3 tensors
like this will result in a `100x100x3` tensor. This function is handy
when each sub-tensors are generated separately. However, it does no
validation test. Use with cautions.

## Author

Zhengjia Wang

## Examples

``` r
tensor1 <- Tensor$new(data = 1:9, c(3,3,1), dimnames = list(
A = 1:3, B = 1:3, C = 1
), varnames = c('A', 'B', 'C'))
tensor2 <- Tensor$new(data = 10:18, c(3,3,1), dimnames = list(
  A = 1:3, B = 1:3, C = 2
), varnames = c('A', 'B', 'C'))
merged <- join_tensors(list(tensor1, tensor2))
merged$get_data()
#> , , C = 1
#> 
#>    B
#> A   1 2 3
#>   1 1 4 7
#>   2 2 5 8
#>   3 3 6 9
#> 
#> , , C = 2
#> 
#>    B
#> A    1  2  3
#>   1 10 13 16
#>   2 11 14 17
#>   3 12 15 18
#> 
```
