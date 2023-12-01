#ifndef _XAPIAN_EXTRA_H
#define _XAPIAN_EXTRA_H

#include <string>
#include <xapian.h>

inline Xapian::Query
xapian_query_match_all (void)
{
    // Xapian::Query::MatchAll isn't thread safe (a static object with reference
    // counting) so instead reconstruct the equivalent on demand.
    return Xapian::Query (std::string ());
}

#endif
