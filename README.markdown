# Gauge: a clone of criterion

This is a clone of criterion with a code / dependencies on a diet. It works the same way as criterion
for outputing to terminal benchmark data.

## features compared to criterion

missing:

* JSON export
* HTML/javascript pages
* Glob benchmark matching

Added:

* Small condensed output (`-s` or `--small`)
* Raw measurements dumping (CSV)

## Future Feature Plan

* Remove further dependencies
* storing benchmarks data in CSV and JSON
* Add a standalone program taking benchmark data files and rendering to html/javascript/graphs
* Make the library more useful as a standalone library to gather benchmark numbers related to functions in a programatic way

## Small mode

It's hard to compare many benchmarks with criterion, so gauge has a `--small` output:

```
identity                                 mean 41.65 ns  ( +- 2.246 ns  )
slow                                     mean 163.9 ns  ( +- 9.683 ns  )
```

## Direct dependencies removed compared to criterion

Number of total dependencies (direct & indirect):

* gauge: 18 dependencies
* criterion: 63 dependencies

Dependencies removed:

* Glob 0.8.0
* abstract-deque 0.3
* abstract-par 0.3.3
* aeson 1.1.2.0
* ansi-terminal 0.6.3.1
* ansi-wl-pprint 0.6.7.3
* array 0.5.1.1
* attoparsec 0.13.1.0
* base-compat 0.9.3
* base-orphans 0.6
* binary 0.8.3.0
* blaze-builder 0.4.0.2
* bytestring 0.10.8.1
* cassava 0.4.5.1
* cereal 0.5.4.0
* criterion 1.2.2.0
* directory 1.3.0.0
* dlist 0.8.0.3
* erf 2.0.0.0
* exceptions 0.8.3
* filepath 1.4.1.1
* hashable 1.2.6.1
* integer-logarithms 1.0.2
* js-flot 0.8.3
* js-jquery 3.2.1
* microstache 1.0.1.1
* monad-par 0.3.4.8
* monad-par-extras 0.3.3
* mtl 2.2.1
* optparse-applicative 0.13.2.0
* parallel 3.2.1.1
* parsec 3.1.11
* process 1.4.3.0
* random 1.1
* scientific 0.3.5.2
* statistics 0.14.0.2
* stm 2.4.4.1
* tagged 0.8.5
* text 1.2.2.2
* time-locale-compat 0.1.1.3
* transformers-compat 0.5.1.4
* unix 2.7.2.1
* unordered-containers 0.2.8.0
* uuid-types 1.0.3
* vector-algorithms 0.7.0.1
* vector-binary-instances 0.2.3.5
* code-page 0.1.3

Criterion graph of dependencies:

![Criterion](/.README.imgs/criterion.png)

Gauge graph of dependencies:

![Gauge](/.README.imgs/gauge.png)
