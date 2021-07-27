# `raveio` (0.0.4)

1. Require platform to be little-endian

Self checks indicated no error/warning. 

* The platforms include `windows`, `ubuntu`, `fedora`, and `macOS`


================================ past comments =================================
# `raveio` (0.0.3)

New submission:

Self check on local machine (OSX Catalina): 0 error, 0 warning, 0 note

Last submission was rejected, questions and solutions:


> RAVE is not a package on CRAN, right? Do you mean the RAVE project?

Changed package title from "Utility Toolbox for RAVE Package" to "Utility Toolbox for RAVE Project". 

`rave` is an R package not on CRAN right now, so it's both project name and a package name. We do have plans to submit `rave` as a CRAN package in the near future.

> The package only contains 36 code lines and there is no read/write functionality or any classes as stated in the Description text. Are you sure that this is the correct version?

The package contains more than 50 exported functions and classes in total. Please let me know if this is not the case.


> Further issues concerning (now) your package:

```
Missing Rd-tags:
      raveio/man/as_rave_project.Rd: \value
      raveio/man/catgl.Rd: \value
```

Added value section to all functions 


> If there are references describing the (theoretical background of) methods in your package, please add these in the Description field of your DESCRIPTION file in the form ...

Added one site and 3 papers in the description

> Please explain the RAVE project in your Description text.

1. Added explanation of `RAVE`: 'RAVE' stands for "R analysis and visualization of human intracranial electroencephalography data"
2. Also added the objectives: The whole project aims at providing powerful free-source package that analyze brain recordings from patients with electrodes placed on the cortical surface or inserted into the brain.
3. In addition to `RAVE`, I also added the role of `raveio` in this project: provides tools to read/write neurophysiology data from/to 'RAVE' file structure, as well as several popular formats including 'EDF(+)', 'Matlab', 'BIDS-iEEG', and 'HDF5', etc.

The OpenWetWare link in the description also guides users to a complete documentation and manual of the entire project. 


Besides, since I added several papers in `DESCRIPTION`, I found one possible issue:

```
Possibly mis-spelled words in DESCRIPTION:
  Beauchamp (20:47, 23:14, 25:47)
  Karas (22:58, 24:27)
  Magnotti (20:11, 22:3, 24:42)
  Metzger (21:58, 24:61)
  Nesbitt (22:38)
  Yoshor (22:72, 25:28)
  Zhengjia (20:21, 22:13)
  Zhu (24:77)
  intracranial (12:3)
  neurophysiology (15:56)
```

I added them in `inst/WORDLIST` but windows devel checks still give this note. These are either authors' names or actual legit words. There is no reason to add single quotation marks.
