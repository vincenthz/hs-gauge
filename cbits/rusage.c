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

long gauge_getrusage_nivcsw(struct rusage *usage)
{
    return usage->ru_nivcsw;
}

long gauge_getrusage_nvcsw(struct rusage *usage)
{
    return usage->ru_nvcsw;
}
