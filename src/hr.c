#include <HsFFI.h>

double r_ValueOfNA() { 
	/* R_NaReal is not initialized until runtime, so we can't use it. */
	static uint64_t ValueOfNA = 0x7ff00000000007a2;
	return *(double *)&ValueOfNA; 
}
