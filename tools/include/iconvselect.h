#ifndef iconvselect_h
#define iconvselect_h

#include <iconv.h>

#ifdef ICONV_IS_POSIX
inline
size_t
iconv( iconv_t cd, const char ** inbuf, size_t * inbytesleft, char ** outbuf, size_t * outbytesleft )
{
  return iconv( cd,
		const_cast< char ** >( inbuf ), inbytesleft,
		outbuf, outbytesleft );
}
#endif

#endif
