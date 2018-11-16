#include "/nix/store/r9rkkv42lhi2p2ywlsi40g07m1x1ypzc-ghc-8.6.1/lib/ghc-8.6.1/template-hsc.h"
#line 7 "Fire.hsc"
#include "fire.h"

int main (void)
{
    hsc_line (1, "src/Data/Array/Fire.hsc");
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("{-# LANGUAGE CPP #-}\n"
           "", hsc_stdout());
    hsc_fputs ("module Data.Array.Fire where\n"
           "\n"
           "import Foreign\n"
           "import Foreign.C.Types\n"
           "\n"
           "", hsc_stdout());
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_fputs ("\n"
           "afVersion = ", hsc_stdout());
#line 9 "Fire.hsc"
    hsc_const (AF_API_VERSION);
    hsc_fputs ("\n"
           "", hsc_stdout());
    hsc_line (10, "src/Data/Array/Fire.hsc");
    hsc_fputs ("\n"
           "-- data DType\n"
           "--   = F32\n"
           "--   | C32\n"
           "--   | F64\n"
           "--   | C64\n"
           "--   | B8\n"
           "--   | S32\n"
           "--   | U32\n"
           "--   | U8\n"
           "--   | S64\n"
           "--   | U64\n"
           "--   | S16\n"
           "--   | U16\n"
           "--   deriving (Show)\n"
           "\n"
           "{ ", hsc_stdout());
#line 26 "Fire.hsc"
    hsc_fputs ("", hsc_stdout());
    hsc_fputs ("}\n"
           "", hsc_stdout());
    hsc_line (27, "src/Data/Array/Fire.hsc");
    hsc_fputs ("", hsc_stdout());
    return 0;
}
