# Install and configure 'RAVE' server as background service using shiny-server

Works on 'Linux' and 'Mac' only.

## Usage

``` r
rave_server_install(
  url = "https://github.com/rstudio/shiny-server/archive/refs/tags/v1.5.18.987.zip"
)

rave_server_configure(
  ports = 17283,
  user = Sys.info()[["user"]],
  rave_version = c("1", "2")
)
```

## Arguments

- url:

  'URL' to shiny-server 'ZIP' file to download

- ports:

  integer vectors or character, indicating the port numbers to host
  'RAVE' instances a valid port must be within the range from 1024 to
  65535.

- user:

  user to run the service as; default is the login user

- rave_version:

  internally used; might be deprecated in the future

## Value

nothing

## Examples

``` r
if (FALSE) { # \dontrun{

# OS-specific. Please install R package `rpymat` first

# Install rave-server
rave_server_install()

# Let port 17283-17290 to host RAVE instance
rave_server_configure(ports = "17283-17290")

} # }
```
