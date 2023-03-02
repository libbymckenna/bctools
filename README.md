# bctools R Package

Contains general and plotting tools.  Guidelines for adding functions:
- Function names should be lowercase with underscore separation, eg: bc_drive
- Functions intended to work with ggplot2 should follow ggplot naming for ease of access, eg: geom_boxandwhisker
- Ensure that function names are not the same as any standard packages
- Argument names should be all lowercase with no seperators, eg: middlepoint
- Default arguments should be specified whenever it can increase convenience
- Documentation should include at a minimum: description of the function, descriptions of each argument, example
- For more complicated functions, consider writing a vignette ... (need to sort that out)
