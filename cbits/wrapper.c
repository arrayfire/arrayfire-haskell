#include "arrayfire.h"
#include <stdio.h>

af_err af_random_engine_set_type_(af_random_engine engine, const af_random_engine_type rtype) { return af_random_engine_set_type(&engine, rtype); }

af_err af_random_engine_set_seed_(af_random_engine engine, const unsigned long long seed) {
  return af_random_engine_set_seed(&engine, seed);
}

void test_bool () {
  double * data = malloc (sizeof (int) * 5);
  data[0] = 2;
  data[1] = 2;
  data[2] = 2;
  data[3] = 2;
  data[4] = 2;
  data[5] = 2;
  dim_t * dims = malloc(sizeof(dim_t) * 4);
  dims[0] = 5;
  dims[1] = 1;
  dims[2] = 1;
  dims[3] = 1;
  af_array arrin;
  af_create_array(&arrin, data, 1, dims, f64);
  printf("printing input array\n");
  af_print_array(arrin);
  af_array arrout;
  af_product(&arrout, arrin, 0);
  printf("printing output array\n");
  af_print_array(arrout);
}

void test_window () {
  af_window window;
  af_create_window(&window, 100, 100, "foo");
  af_show(window);
}
