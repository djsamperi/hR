#include <HsFFI.h>

double r_ValueOfNA() { 
	static uint64_t ValueOfNA = 0x7ff00000000007a2;
	return *(double *)&ValueOfNA; 
}
