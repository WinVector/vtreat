

'vtreat' 0.5.23 2016/04/16

Changes:

- Fix issue of non-significant variables not being scaled.
- Documentation fixes, document variable types, improve vignettes.


'vtreat' 0.5.22 2016/01/07

Changes:

- Calculations of non catX significances are now deterministic
- Catch mis-naming or non-varying y earlier
- Expose buildEvalSets as a public function
- More tests


'vtreat' 0.5.21 2015/11/23

Changes:

- Isolated "cross" or "out of sample" frame generation into mkCrossFrameCExperiment() and mkCrossFrameNExperiment()
- Many small bug fixes and corner cases patched


'vtreat' 0.5.20 2015/11/05

Changes:

- Simplified out of sample frame generation
- Added "no-Y" treatment option
- Minor documentation fixes


'vtreat' 0.5.18 2015/10/07

Changes:

- Fixed bugs in cross-validated mode
- Force out of sample calculations in more situations
- Fix vignette titles
- Fix documentation
- Do not allow small or insufficiently varying data frames (exact conditions in documentation)


'vtreat' 0.5.16 2015/09/12

Changes:

-  Unified rare level treatment
-  Separate treatment of insignificant levels
-  Tests confirming compatibility with 'data.table'
-  More special case hardening
-  Over-fit vignette
-  Minor documentation fixes



'vtreat' 0.5.14 2015/09/06

- First CRAN release
