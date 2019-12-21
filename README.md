
<!-- README.md is generated from README.Rmd. Please edit that file -->

## tabraux

This package contains auxiliary functions for package
[tabr](https://github.com/leonawicz/tabr).

While working with the package to copy music sheets (my use case being
to ‘translate’ the bass stave in F-key to the G-key I am used to) I
noticed that the `tabr::pc` function could not handle redundant
white-space and that it toke me some time to add to the notes the octave
and sharp-flat information. While I may add more functions to this
auxilary package in the future, at the moment it contains the functions:

  - `pc2` : just like `tabr::pc` but removes redundant white-space by
    calling `stringr::str_squish`
  - `expand_notes` : this copies the octave number from a previous
    specification and insert sharp and flat information where needed
  - `check_times` : counts the notes in each measure to allow easy
    checking
