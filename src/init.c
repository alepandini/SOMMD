#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Declare the C functions
SEXP rio_read_xtc_natoms_(SEXP xtc_filename_);
SEXP rio_read_xtc_nframes_(SEXP xtc_filename_);
SEXP rio_read_xtc_(SEXP xtc_filename_);
SEXP rio_write_xtc_(SEXP xtc_filename_, SEXP coords_, SEXP natoms_, SEXP nframes_);

// Register the C functions
static const R_CallMethodDef CallEntries[] = {
    {"rio_read_xtc_natoms_", (DL_FUNC) &rio_read_xtc_natoms_, 1},
    {"rio_read_xtc_nframes_", (DL_FUNC) &rio_read_xtc_nframes_, 1},
    {"rio_read_xtc_", (DL_FUNC) &rio_read_xtc_, 1},
    {"rio_write_xtc_", (DL_FUNC) &rio_write_xtc_, 4},
    {NULL, NULL, 0}
};

void R_init_SOMMD(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
