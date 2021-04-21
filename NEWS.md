

# vtreat 1.6.3 2020/04/21

 * Fix some links.
 
# vtreat 1.6.2 2020/10/17

 * Move to tinytest

# vtreat 1.6.1 2020/08/12

 * Documentation improvements.

# vtreat 1.6.0 2020/03/10

 * More S3 methods.
 * Back-port pyvtreat recommendation code to Rvtreat.

# vtreat 1.5.2 2020/02/08

 * Control imputation on design_missingness_treatment.
 * Share cross-splits in multinomial mode to minimize data leakage.
 * Earlier argument checking.
 * Default classification target to TRUE.
 * Some documentation improvements.
 * Move wrapr to Depends.

# vtreat 1.5.1 2020/01/16

 * Check for same-frame on prepare paths.
 * Pipe into ft.
 * Rename multinomial arguments to match other methods.
 * Move parallel to a suggestion and configure a global option to shut it down ( vtreat.allow_parallel ).

# vtreat 1.5.0 2020/01/07

 * Fix multinomial variable restriction.
 * Fix FT multinomial columns copy.
 * Work on ft serialization environments.
 * More tests.
 * Improve custom coder flexibility.

# vtreat 1.4.8 2019/12/08

 * Center/scale improvement from nfultz.
 * Documentation updates.
 * fit_tranform interface.

# vtreat 1.4.7 2019/10/01

 * fix wrong y-name in multinomial cross frame.
 
# vtreat 1.4.6 2019/09/23

 * Some doc updates.
 
# vtreat 1.4.5 2019/09/11

 * Some doc updates.

# vtreat 1.4.4 2019/07/27

 * Adjust license to GPL-2 | GPL-3 
 * Some doc updates.

# vtreat 1.4.3 2019/07/16

 * Fix bug in custom coder NA path and let numeric coder center if wanted.
 * More tests.
 
# vtreat 1.4.2 2019/07/01

 * Fix eronious Cohen reference in documentation.
 
# vtreat 1.4.0 2019/05/05

 * Fancy level and variable names.
 * More tests on odd level names (and collisions).

# vtreat 1.3.8 2019/03/31

 * Remove spline from default value variables.
 * Fix ggplot2 vignette dependence issue.
 * Make treatment plans pipeable targets.

# vtreat 1.3.7 2019/02/20

 * Fix design_missingness_treatment() prepare.
 * Move to wrapr test runner.

# vtreat 1.3.6 2019/02/09

 * Add patch_columns_into_frame().
 * Fix column name munging.
 * Start switching to RUnit tests.

# vtreat 1.3.5 2019/01/27

 * Make categorical scoring default TRUE.
 * logit-ize custom coders.
 * spline custom coder.
 * Get stricter about row ordering in building custom coders.
 * neaten up printing a bit.
 * Collar spline and lower its complexity.
 
# vtreat 1.3.4 2019/01/02

 * Accept pre-built approximation tables from numeric custom coders.
 * Fix .customCodeNum() extrapolation, down-sampling, and remote numeric ".center" option.
 * More docs.

# vtreat 1.3.3 2018/12/17

 * remove _clean suffix.
 * Fix non-scalar comparisions (mostly form class vectors).
 * add segmented variable calculation.
 * add pre-computed split plan.
 * bump dependencies.
 * Example higher order coders.

# vtreat 1.3.2 2018/11/05

 * force parent.frame
 * add a drop columns option to design_missingness_treatment()
 * Doc fixes

# vtreat 1.3.1 2018/09/10

 * Fix variable naming in multi class case.
 * Some doc fixes.
 * Start on NaN and Infinity on databases.
 * More tests.

# vtreat 1.3.0 2018/07/20

 * Fix z-split annotation issue.
 * Change designTreatmentsZ() defaults.
 * Documentation fixes.
 * Multiclass treatments.
 * Simple treatments.
 * Fix CRAN Note on pre 3.5.0 R parallel usage.

# vtreat 1.2.3 2018/07/11

  * Smooth catP novel levels scoring a bit.
  * Fix check error on parallel::getDefaultCluster()
  
# vtreat 1.2.2 2018/07/04

  * Fix issue 19.
  * Do not use NULL to signal when parallelism is desired.

# vtreat 1.2.1 2018/06/26

  * default data.table merging on (may be a performance regression to not set this).
  * rqdatatable treatment path.
  * better formatting.
  * add extracols argument to prepare().
  * Improve regexps.

# vtreat 1.2.0 2018/06/19

  * Translate treatment plans to rquery.
  * Minor documentation improvements.
  * Improve error messages on argument checking.
  * Improve name generation (remove dots).
  * Remove dplyr dependence.
 
# vtreat 1.0.4 2018/05/05

  * Add data.table as an optional row-binder.
  * Declare an R version.

# vtreat 1.0.3 2018/03/10

  * parallel indicator calculation.
  * add optional warning on encountering novel levels in prepare.

# vtreat 1.0.2 2018/01/20

  * bind options.
  * fix deps.
  * restore parallel test.
  * kWayStratifiedY performance fix from @khotilov Vadim Khotilovich.

# vtreat 1.0.1 2017/10/16

  * Minor documentation fixes.
  * rm data.table (possibly related to issues 2413 or 2418).

# vtreat 1.0.0 2017/10/04

  * Minor documentation fixes.
  * More work on novel values for non-centered custom coders.
  * Numeric custom coders.
  * Isotone examples.

# vtreat 0.6.0 2017/09/20

  * Add codeRestriction option to design steps.
  * Prepare for custom models.
  * Prefer data.table::rbindlist() for assembling frames.
  * Add forceSplit mode.

# vtreat 0.5.32 2017/06/13

  * Add codeRestriction option to prepare().

# vtreat 0.5.31 2017/04/13


  * make prueSig an optional argument in prepare, and force by-name access.
  * Remove left.op=TRUE from findInterval in "vtreat Rare Levels" vignette (seems to be a new addition to findInterval, so fails CRAN check r-oldrel-windows-ix86+x86_64).


# vtreat 0.5.30 2017/01/21

  * General improvements in documentation and vignettes.
  * Document saving/loading treatment plans.


# vtreat 0.5.29 2016/10/27

  * Add 'rsq' column to scoreFrame (rsq- for numeric targets, pseudo-rsq for categorical targets).

# vtreat 0.5.28 2016/10/24


  * Fix treatment of constant columns in cross-partitions.
  * Switch doCollar default to FALSE.
  * Return indicators on designTreatmentsZ.
  * Fix extraDegreesOfFreedom calculation.
  * Allow repeated application rows in cross frames.
  * Remove lsig and csig from scoreFrame.
  * Add meanY to treatmentplan.
  * Documentation fixes.


# vtreat 0.5.27 2016/08/16

Changes:

  * Change catB variables to delta-logit score.
  * Fix passing of arguments to parallel prepare, and reduce size of data passed.
  * Ensure set of variables consistency in mkCross* methods.
  * More parallelization of level significance calculations, and cheaper chi-square test where appropriate.

# vtreat 0.5.26 2016/07/10

  * Facilities for y-stratified and grouped splitting (useful for unbalanced classes).
  * catScaling=TRUE mode uses logistic regression for y-aware variable scaling.
  * Fix erroneous bad type warning on date columns.


# vtreat 0.5.25 2016/05/02


  * Fix bug that caused catB variables to be scored as "insignificant".
  * Add test to check for above bug.
  * Expose cross validation controls.
  * More guards on significance calculations.


# vtreat 0.5.23 2016/04/28

  * Fix issue of non-significant variables not being scaled.
  * Documentation fixes, document variable types, improve vignettes.
  * Minor performance fixes on result accumulation.
  * Add optional use of dplyr for row binding (of score report frames).


# vtreat 0.5.22 2016/01/07


  * Calculations of non catX significances are now deterministic
  * Catch mis-naming or non-varying y earlier
  * Expose buildEvalSets as a public function
  * More tests


# vtreat 0.5.21 2015/11/23

  * Isolated "cross" or "out of sample" frame generation into mkCrossFrameCExperiment() and mkCrossFrameNExperiment()
  * Many small bug fixes and corner cases patched


# vtreat 0.5.20 2015/11/05


  * Simplified out of sample frame generation
  * Added "no-Y" treatment option
  * Minor documentation fixes


# vtreat 0.5.18 2015/10/07

  * Fixed bugs in cross-validated mode
  * Force out of sample calculations in more situations
  * Fix vignette titles
  * Fix documentation
  * Do not allow small or insufficiently varying data frames (exact conditions in documentation)


# vtreat 0.5.16 2015/09/12


  *  Unified rare level treatment
  *  Separate treatment of insignificant levels
  *  Tests confirming compatibility with 'data.table'
  *  More special case hardening
  *  Over-fit vignette
  *  Minor documentation fixes



# vtreat 0.5.14 2015/09/06

  * First CRAN release
