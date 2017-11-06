#include <sys/time.h>
#include <sys/resource.h>

/*
 * Not thread safe
 */
struct rusage * gauge_getrusage(void)
{
    static struct rusage usage;

    getrusage(RUSAGE_SELF, &usage);
    return &usage;
}

/* user time in microseconds */
long gauge_getrusage_utime(struct rusage *usage)
{
    struct timeval tv;

    tv = usage->ru_utime;
    return tv.tv_sec * 1000000 + tv.tv_usec;
}

/* system time in microseconds */
long gauge_getrusage_stime(struct rusage *usage)
{
    struct timeval tv;

    tv = usage->ru_stime;
    return tv.tv_sec * 1000000 + tv.tv_usec;
}

#define def_long_accessor(f) \
long gauge_getrusage_##f(struct rusage *usage) \
{ \
    return usage->ru_##f; \
}

def_long_accessor(maxrss)
def_long_accessor(minflt)
def_long_accessor(majflt)
def_long_accessor(nvcsw)
def_long_accessor(nivcsw)
