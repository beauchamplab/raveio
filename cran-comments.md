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
