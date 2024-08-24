#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <string.h>
#include "xdrfile/xdrfile.h"
#include "xdrfile/xdrfile_xtc.h"

int file_exists(const char * filename){

  /* create file handle */
  FILE *file = NULL;

  if (file == fopen(filename, "r")){
    fclose(file);
    return 1;
  }

  return 0;
}

XDRFILE *rio_xdrfile_open(SEXP xtc_filename_, const char *open_mode){

  /* duplicate string to non-const */
  int xtc_filename_length;

  const char *xtc_filename = CHAR(asChar(xtc_filename_));
  xtc_filename_length = strlen(xtc_filename) + 1;

  char xtc_filename_input[xtc_filename_length];
  strncpy(xtc_filename_input, xtc_filename, xtc_filename_length);

  /* open xtc file handle */
  XDRFILE* xtc_file = xdrfile_open(xtc_filename_input, open_mode);

  return xtc_file;
}

SEXP rio_read_xtc_natoms_(SEXP xtc_filename_)
{

  /* number of atom and status variables */
  int natms;

  /* duplicate string to non-const */
  int xtc_filename_length;

  const char *xtc_filename = CHAR(asChar(xtc_filename_));
  xtc_filename_length = strlen(xtc_filename) + 1;

  char xtc_filename_input[xtc_filename_length];
  strncpy(xtc_filename_input, xtc_filename, xtc_filename_length);

  /* read number of atoms from xtc file */
  read_xtc_natoms(xtc_filename_input, &natms);

  /* return number of atom as R integer */
  return ScalarInteger(natms);
}


SEXP rio_read_xtc_nframes_(SEXP xtc_filename_)
{

  /* number of atom and status variables */
  int natms;
  int status;

  /* step, time, precision variables */
  int step;
  float time;
  float prec;

  /* box 3x3 matrix variable */
  matrix box;

  /* frame counter variable */
  int iframe = 0;

  /* duplicate string to non-const */
  int xtc_filename_length;

  const char *xtc_filename = CHAR(asChar(xtc_filename_));
  xtc_filename_length = strlen(xtc_filename) + 1;

  char xtc_filename_input[xtc_filename_length];
  strncpy(xtc_filename_input, xtc_filename, xtc_filename_length);

  /* read number of atoms from xtc file */
  natms = asInteger(rio_read_xtc_natoms_(xtc_filename_));

  /* allocate memory for coordinate vector */
  rvec *coord_vec = malloc(natms * sizeof(rvec));

  /* open xtc file handle */
  XDRFILE *xtc_file = rio_xdrfile_open(xtc_filename_, "r");

  /* read first frame from xtc file */
  status = read_xtc(xtc_file, natms, &step, &time, box, coord_vec, &prec);

  /* read remaining frames from xtc file */
  while(status == 0){

    /* increment frame counter variable */
    iframe++;

    /* read next frame from xtc file */
    status = read_xtc(xtc_file, natms, &step, &time, box, coord_vec, &prec);
  }

  /* close xtc file handle */
  xdrfile_close(xtc_file);

  /* free memory from coordinate vector*/
  free(coord_vec);

  /* return number of frames as R integer */
  return ScalarInteger(iframe);
}

SEXP rio_read_xtc_(SEXP xtc_filename_)
{

  /* counter variables */
  int i, j;

  /* step, time, precision variables */
  int step;
  float time;
  float prec;

  /* box 3x3 matrix variable */
  matrix box;

  /* frame counter and number of frame variables */
  int iframe = 0;
  int nframes = 0;

  /* Cartesian dimensions, number of atom and status variables */
  int cartesian_dim = 3;
  int natms;
  int status;

  /* read number of atoms from xtc file */
  natms = asInteger(rio_read_xtc_natoms_(xtc_filename_));

  /* allocate memory for coordinate vector */
  rvec *coord_vec = malloc(natms * sizeof(rvec));

  /* read number of frames from xtc file */
  nframes = asInteger(rio_read_xtc_nframes_(xtc_filename_));

  /* open xtc file handle */
  XDRFILE *xtc_file = rio_xdrfile_open(xtc_filename_, "r");

  /* allocate and protect memory for coordinate matrix */
  SEXP coord = PROTECT(allocVector(REALSXP, natms*cartesian_dim*nframes));

  /* read first frame from xtc file */
  status = read_xtc(xtc_file, natms, &step, &time, box, coord_vec, &prec);

  /* read remaining frames from xtc file */
  while(status == 0){

    /* copy frame coordinates into R matrix */
    double *real_coord = REAL(coord);
    for(i = 0; i < natms; i++){
      for(j =0; j < cartesian_dim; j ++){
        real_coord[i + natms*j + natms*cartesian_dim*iframe] = coord_vec[i][j];
      }
    }

    /* increment frame counter variable */
    iframe++;

    /* read next frame from xtc file */
    status = read_xtc(xtc_file, natms, &step, &time, box, coord_vec, &prec);
  }

  /* set dimensions for coordinate matrix */
  SEXP dim = PROTECT(allocVector(INTSXP, 3));
  INTEGER(dim)[0] = natms;
  INTEGER(dim)[1] = cartesian_dim;
  INTEGER(dim)[2] = iframe;
  setAttrib(coord, R_DimSymbol, dim);

  /* unprotect memory*/
  UNPROTECT(2);

  /* close xtc file handle */
  xdrfile_close(xtc_file);

  /* free memory from coordinate vector*/
  free(coord_vec);

  /* return coordinate matrix */
  return coord;
}

SEXP rio_write_xtc_(SEXP xtc_filename_, SEXP coords_, SEXP natoms_, SEXP nframes_)
{

  /* R to C mapping */
  double *coords = REAL(coords_);
  int natoms = asInteger(natoms_);
  int nframes = asInteger(nframes_);
  int nxyz = 3;

  /* status variable */
  int status;

  /* open xtc file handle */
  XDRFILE *xtc_file = rio_xdrfile_open(xtc_filename_, "w");

  /* dummy values */
  int step = 1;
  float time = 0.000;
  float prec = 1000.0;
  matrix box = {
      {0.000f, 0.000f, 0.000f},
      {0.000f, 0.000f, 0.000f},
      {0.000f, 0.000f, 0.000f}
    };

  /* create frame coordinate array */
  rvec *frame_rvec = malloc(natoms * sizeof(rvec));

  /* cycle over frames */
  for(int k =0; k < nframes; k++){

    /* copy frame coordinate array */
    for(int j =0; j < nxyz; j++){
      for(int i =0; i < natoms; i++){
        frame_rvec[i][j] =  (float)coords[i + (j * natoms) + (k * natoms * nxyz)];
      }
    }

    /* save frame coordinate array */
    status = write_xtc(xtc_file,
                         natoms, step, time,
                         box, frame_rvec, prec);

    /* increase step */
    step += 1;

  }

  /* close xtc file handle */
  xdrfile_close(xtc_file);

  /* status variable */
  SEXP return_status = PROTECT(allocVector(INTSXP, 1));
  INTEGER(return_status)[0] = status;
  UNPROTECT(1);

  /* free memory */
  free(frame_rvec);

  return return_status;
}
