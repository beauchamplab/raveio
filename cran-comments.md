New submission:

Self check on local machine (OSX Catalina): 0 error, 0 warning, 0 note

Platforms/methods:
* `devtools::check_rhub()`
* `devtools::check_win_release()`
* `devtools::check_win_devel()`
* `devtools::check_win_oldrelease()`

Potential issues and reasons:

1. Windows Server 2008 R2 SP1, R-devel, 32/64 bit
```
Possibly mis-spelled words in DESCRIPTION:
    intracranial (11:55)
```

> `intracranial` is a legit word. It means "within the skull"

2. https://win-builder.r-project.org/ZtEu9k0gvSw7/00check.log
```
Package required and available but unsuitable version: 'backports'
```

> `backports` is a CRAN package and the version required by `raveio` is `>= 1.1.7` while on CRAN, the compiled version is `1.1.8`
