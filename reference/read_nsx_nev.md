# Read 'BlackRock' event and signal files

Current implementation supports minimum 2.3 file specification version.
Please contact the package maintainer to add specification
configurations if you want us to support older versions.

## Usage

``` r
read_nsx_nev(
  paths,
  nev_path = NULL,
  header_only = FALSE,
  nev_data = TRUE,
  verbose = TRUE,
  ram = FALSE,
  force_update = FALSE,
  temp_path = file.path(tempdir(), "blackrock-temp")
)
```

## Arguments

- paths:

  'NSx' signal files, usually with file extensions such as `'.ns1'`,
  `'.ns2'`, `'.ns3'`, `'.ns4'`, `'.ns5'`.

- nev_path:

  'NEV' event files, with file extension `'.nev'`

- header_only:

  whether to load header information only and avoid reading signal
  arrays

- nev_data:

  whether to load `'.nev'` comments and 'waveforms'

- verbose:

  whether to print out progress when loading signal array

- ram:

  whether to load signals into the memory rather than storing with
  [`filearray`](https://dipterix.org/filearray/reference/filearray.html);
  default is false

- force_update:

  force updating the channel data even if the headers haven't changed

- temp_path:

  temporary directory to store the channel data
