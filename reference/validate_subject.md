# Validate subject data integrity

Check against existence, validity, and consistency

## Arguments

- subject:

  subject ID (character), or [`RAVESubject`](RAVESubject.md) instance

- method:

  validation method, choices are `'normal'` (default) or `'basic'` for
  fast checks; if set to `'normal'`, four additional validation parts
  will be tested (see parts with `*` in Section 'Value').

- verbose:

  whether to print out the validation messages

- version:

  data version, choices are `1` for 'RAVE' 1.0 data format, and `2`
  ('RAVE' 2.0 data format); default is `2`

## Value

A list of nested validation results. The validation process consists of
the following parts in order:

- **Data paths** (`paths`):

- `path`:

  the subject's root folder

- `path`:

  the subject's 'RAVE' folder (the `'rave'` folder under the root
  directory)

- `raw_path`:

  the subject's raw data folder

- `data_path`:

  a directory storing all the voltage, power, phase data (before
  reference)

- `meta_path`:

  meta directory containing all the electrode coordinates, reference
  table, epoch information, etc.

- `reference_path`:

  a directory storing calculated reference signals

- `preprocess_path`:

  a directory storing all the preprocessing information

- `cache_path` (low priority):

  data caching path

- `freesurfer_path` (low priority):

  subject's 'FreeSurfer' directory

- `note_path` (low priority):

  subject's notes

- `pipeline_path` (low priority):

  a folder containing all saved pipelines for this subject

- **Preprocessing information** (`preprocess`):

- `electrodes_set`:

  whether the subject has a non-empty electrode set

- `blocks_set`:

  whether the session block length is non-zero

- `sample_rate_set`:

  whether the raw sampling frequency is set to a valid, proper positive
  number

- `data_imported`:

  whether all the assigning electrodes have been imported

- `notch_filtered`:

  whether all the 'LFP' and 'EKG' signals have been 'Notch' filtered

- `has_wavelet`:

  whether all the 'LFP' signals are wavelet-transformed

- `has_reference`:

  at least one reference has been generated in the meta folder

- `has_epoch`:

  at least one epoch file has been generated in the meta folder

- `has_electrode_file`:

  meta folder has `electrodes.csv` file

- **Meta information** (`meta`):

- `meta_data_valid`:

  this item only exists when the previous preprocess validation is
  failed or incomplete

- `meta_electrode_table`:

  the `electrodes.csv` file in the meta folder has correct format and
  consistent electrodes numbers to the preprocess information

- `meta_reference_xxx`:

  (`xxx` will be replaced with actual reference names) checks whether
  the reference table contains all electrodes and whether each reference
  data exists

- `meta_epoch_xxx`:

  (`xxx` will be replaced with actual epoch names) checks whether the
  epoch table has the correct formats and whether there are missing
  blocks indicated in the epoch files

- **Voltage data** (`voltage_data*`):

- `voltage_preprocessing`:

  whether the raw preprocessing voltage data are valid. This includes
  data lengths are the same within the same blocks for each signal type

- `voltage_data`:

  whether the voltage data (after 'Notch' filters) exist and readable.
  Besides, the lengths of the data must be consistent with the raw
  signals

- **Spectral power and phase** (`power_phase_data*`):

- `power_data`:

  whether the power data exists for all 'LFP' signals. Besides, to pass
  the validation process, the frequency and time-point lengths must be
  consistent with the preprocess record

- `power_data`:

  same as `power_data` but for the phase data

- **Epoch table** (`epoch_tables*`):

  One or more sub-items depending on the number of epoch tables. To pass
  the validation, the event time for each session block must not exceed
  the actual signal duration. For example, if one session lasts for 200
  seconds, it will invalidate the result if a trial onset time is later
  than 200 seconds.

- **Reference table** (`reference_tables*`):

  One or more sub-items depending on the number of reference tables. To
  pass the validation, the reference data must be valid. The
  inconsistencies, for example, missing file, wrong frequency size,
  invalid time-point lengths will result in failure
