# Contributing to {carver}

This outlines how to propose a change to {carver}.

## Making Changes

If you want to make a change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 
Per pfizer-opensource policies, only those listed as collaborators can raise issues.  
If you’ve found a bug, please email the maintainer (`smriti.anand@pfizer.com`) illustrating the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).
Alternatively if you have access to the COSA community Slack channel for {carver}, you can raise it there.  
See guide on [how to create a great issue](https://code-review.tidyverse.org/issues/) for more advice.

### Pull request process

*   Fork the package and clone onto your computer. 

*   If needed, install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). 

*   Make your changes, commit to git, and then create a PR.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Closes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.

### Code Style

*   New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
    You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles, but please don't restyle code that has nothing to do with your PR.  
    
    * To apply the appropriate style with styler please use `styler:::style_active_pkg()` or `styler::style_file()`

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

*  All helper/non-exported functions are documented with roxygen2 as indicated above. 
   Include `#' @keywords internal` to mark the function as internal.
   Any helper functions that appear in examples will need to use the `carver:::` 
   prefix.

### Error Handling

TBD  

### Package Dependencies

Additional package dependencies should be considered after exhausting other possibilities. If you do need to add one,  
email the maintainer or collaborators/start a discussion to make a decision on the necessity of it.  
Once agreed upon, the dependency can be added to the package via the relevant files.  

## Scope

The {carver} package will be an open-source tool and package to enable generation of
common analysis reports (tables and interactive plots) for clinical
review and direct inclusion in submission for regulatory agencies

## Deprecation Cycle

TBD  

## Code of Conduct

Please note that the carver project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.