## Test environments
* Local Windows 11 (Build 23H2), R 4.5.2
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 1 note

## Resubmission
This is a resubmission. In this version, I have addressed the comments from the CRAN team:

1. Description field:
   I have removed the linebreaks from the description field in the DESCRIPTION file.

2. References:
   The package implements standard simulation workflows based on 'lme4' model specifications. As no novel statistical methods are introduced, no specific references were added to the description.

3. Examples (\dontrun vs \donttest):
   Since all examples that were previously wrapped in \dontrun{} are example simulations that take longer than 5 seconds to execute, I have replaced \dontrun{} with \donttest{} in all cases

4. Not easily suppressable console messages:
   I have refactored the utility functions (get_fixed_effects_structure, get_random_effects_structure) to return objects with an assigned S3 class. The console output is now handled exclusively via registered S3 print methods instead of direct cat() calls in the main functions.

Note from check:
* Possibly misspelled words in DESCRIPTION: GLMMs, LMMs.
  These are standard acronyms for (Generalized) Linear Mixed Models in the context of statistics and are defined in the Description text.
