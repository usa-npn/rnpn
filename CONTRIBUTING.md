# Contributing to and maintaining `rnpn`

This outlines how to propose changes to `rnpn` as well as some common maintenance workflows.
It is largely borrowed from the tidyverse standard CONTRIBUTING.md.
For more details on R package development, refer to <https://r-pkgs.org/>.

## Setup

You'll want to install the `devtools` package, which will also install `usethis`—both of these packages are extremely helpful.
Running `usethis::use_devtools()` will add a line to your .Rprofile file that will automatically load `devtools` when you open this project in an IDE.
`usethis::git_sitrep()` will help you sort out your git situation including creating and storing a GitHub PAT, which is needed for some of the recommended steps below.

## Proposing and making changes

If you want to make a change, it's a good idea to first file an issue and make sure the package maintainer agrees that it is needed.
If you’ve found a bug, please file an issue that illustrates the bug with a minimal [reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).
See the tidyverse guide on [how to create a great issue](https://code-review.tidyverse.org/issues/) for more advice.

### Pull request process

-   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("usa-npn/rnpn", fork = TRUE)`.
-   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing.
-   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.
-   As you make changes, you can test out functions interactively by running `devtools::load_all()` to roughly simulate what happens when the package would be installed and loaded with `library()`
-   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser. The title of your PR should briefly describe the change. The body of your PR should contain `Fixes #issue-number`.
-   For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.
-   After your PR is merged on GitHub, you can use `usethis::pr_finish()` to update your main branch and delete the PR branch both locally and on GitHub.

### Tests

-   Unit tests are written using [`testthat`](https://cran.r-project.org/package=testthat) and can be found in `tests/testthat/`.

-   Many of the tests use `vcr` for webmocking.
    The first time code wrapped in `vcr::use_casette({})` is run successfully, a "fixture" is created in `tests/fixtures/`.
    Subsequent runs of that code will *not* query the API but rather retrieve a cached response from `tets/fixtures/`.
    If the API changes or you are finding inconsistencies between tests and interactively running your code, you may want to regenerate these fixtures.
    You can do this by simply deleting the relevant .yml files and re-running the tests (e.g. with `test()`).

### Documentation

-   We use [`roxygen2`](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.
-   Make changes to documentation in .R files, not in .Rd files
-   Remember to run `document()` and then check the rendered help file with `?function`
-   The documentation (including README.md and vignettes) are used to automatically create a [`pkgdown`](https://pkgdown.r-lib.org/) website for the package. This can be customized with `_pkgdown.yml`.

## Reviewing Pull Requests (for maintainer only)

-   You can get the branch for a pull request locally with `usethis::pr_fetch(<PR number>)`

-   Load that version of `rnpn` including proposed changes with `load_all()` to play around with functions interactively.

-   Run R CMD check locally with `check()` or use `test()` to only run tests.

-   Add review comments via GitHub and make sure that comments are addressed and tests are passing before merging.

-   `usethis::pr_forget()` is useful if you're done looking at the PR for now, but the branch hasn't been merged yet.
    It will delete the branch *locally* only.

-   `usethis::pr_finish()` is useful for "cleaning up" a branch both locally and on GitHub after the associated PR is merged.

## Making a release (for maintainer only)

-   A good place to start is by running `usethis::use_release_issue()` which will create a GitHub issue with a checklist of things to do before making a release.
    It will ask if this is a major, minor, or patch release.

-   The checklist created will include any custom bullets in `R/release-reminders.R`

## Code of Conduct

Please note that the `rnpn` project is released with a [Contributor Code of Conduct](https://ropensci.org/code-of-conduct/).
By contributing to this project you agree to abide by its terms.
