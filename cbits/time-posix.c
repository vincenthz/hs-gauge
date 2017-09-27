#include <time.h>

void gauge_inittime(void)
{
}

double gauge_gettime(void)
{
    struct timespec ts;

    clock_gettime(CLOCK_MONOTONIC, &ts);

    return ts.tv_sec + ts.tv_nsec * 1e-9;
}


double gauge_getcputime(void)
{
    struct timespec ts;

    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);

    return ts.tv_sec + ts.tv_nsec * 1e-9;
}
