* 0_setup.do
*
* One-time install script for the Stata dependencies of the
* "Fee Optimality in a Two-Sided Market" replication package.
*
* Run from the repository root:
*   stata -b do code/0_setup.do          // batch
* or in interactive Stata:
*   do code/0_setup.do
*
* Installs (from SSC):
*   - ftools     (dependency of reghdfe)
*   - reghdfe    (used in code/explore_yipit/*.do and
*                 code/numerator_menu_pricing/estimate_price_effects_disagg.do)
*   - estout     (used in the same .do files for table export)

version 19

clear all
set more off

local pkgs ftools reghdfe estout

foreach pkg of local pkgs {
    capture which `pkg'
    if _rc != 0 {
        display as text "Installing `pkg' from SSC ..."
        ssc install `pkg', replace
    }
    else {
        display as text "`pkg' already installed."
    }
}

display as text _newline "All Stata dependencies installed."
