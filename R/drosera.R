#' @title Leaf and petiole size for three African sundew species
#'
#' @description Includes measurements for leaf blade and petiole length and
#'   width for _Drosera capensis_ (typical and red form), _Drosera
#'   madagascarienis_ (typical form) and _Drosera venusta_ (typical and
#'   athocyanin-reduced form).
#'
#' @details  The `drosera` dataset contains 150
#'   observations of leaf and petiole size for five varieties of three African
#'   sundew species: the typical and red forms of _Drosera capensis_,  the
#'   typical form of _Drosera madagascarienis_ and the typical and
#'   anthocyanin-reduced forms _Drosera venusta_. The measurements were
#'   performed in July 2020 on plants growing on my (R.M. Link's) windowsill in
#'   a matter of a couple of hours with a regular caliper and therefore do not
#'   necessarily live up to high levels of scientific rigor. However, when it
#'   comes to illustrating correlation between biometric variables, the dataset
#'   does its job, and therefore can be used as an replacement for the
#'   [iris][datasets::iris] dataset by Edgar Anderson and Ronald A. Fisher,
#'   which has a problematic past due to its publication in the Annals of
#'   Eugenics.
#'
#'   The `drosera` dataset is available as a separate data package under
#'   [https://github.com/r-link/drosera](https://github.com/r-link/drosera). It
#'   is  published under a Creative Commons Zero license and is therefore free
#'   for all kinds of commercial and non-commercial use. In particular, if you
#'   need a replacement dataset for `iris` for the examples in your R package,
#'   feel free to add `drosera` to the package without any additional
#'   precautions.
#'
#'   However, it would be kind if you add a link to the package's Github page in
#'   case you decide to use it (and maybe drop me a line so I can link your
#'   project in the repository), but I won't be mad if you don't.
#'
#' @format A data.frame with 150 rows and 6 variables: \describe{
#'   \item{species}{a factor denoting the sundew species (_Drosera capensis_,
#'   _madagascariensis_ and _venusta_)} \item{variety}{a factor denoting the
#'   variety ("typical" and "rubra" for _D. capensis_, "typical" and "alba" for
#'   _D. venusta_ and "typical" for _D. madagascariensis_)}
#'   \item{petiole_length}{a numeric denoting petiole length in millimeters}
#'   \item{petiole_width}{a numeric denoting petiole width in millimeters}
#'   \item{blade_length}{a numeric denoting leaf blade length in millimeters}
#'   \item{blade_width}{an numeric denoting leaf blade width in millimeters}
#' }
"drosera"
