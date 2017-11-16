# WIP 0.2.0

* `Usability`: Simplify and organize the documentation and user APIs.
* `Functionality`: Add measurement and reporting of more performance counters on
  Unices (collected via getrusage) for example page faults, user time, system
  time and rss are now available in verbose mode.
* `Control`: Provide better control over measurement process with
  `--min-samples`, `--min-duration` and `--include-first-iter` flags.
* `Speed:` Add `--quick` flag that provides results much faster (10x) without
  using statistical analysis.
* Reliability:
  * Fix a bug in GC stats collection and reporting with GHC 8.2 that caused
    incorrect reporting of some GC stats.
  * Fix a bug in statistical regression that caused incorrect reporting of mean
    and other stats.
  * Improve reliability by isolating benchmarks from one another using the
    `--measure-with` flag. The results of one benchmark are no longer affected
    by other benchmarks because each benchmark runs in a separate process.
* Modularity:
  * Introduce `--measure-only` flag that allows just measurement and no
    analysis or reporting.
  * Provide modular build, measurement code is cleanly separated from
    statistical analysis code. As a result a leaner version can now be built
    without analysis code (controlled by the `analysis` build flag).
  * Clean, refactor & rewrite source code

# 0.1.3

* Simplify monad handling, remove foundation as dependency

# 0.1.2

* condensed display with `--small`

# 0.1.1

* remove optparse-applicative

# 0.1.0

* remove bunch of dependencies
* initial import of criterion-1.2.2.0
