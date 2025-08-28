//---------------------------------------------------------------------------

#include <System.hpp>
#pragma hdrstop

//---------------------------------------------------------------------------

#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#if defined(USEPACKAGES)
 #pragma comment(lib, "IcsCommonCBNewRun.bpi")
#else
 #pragma comment(lib, "IcsCommonCBNewRun")
#endif

extern "C" int _libmain(unsigned long reason)
{
    return 1;
}
//---------------------------------------------------------------------------


