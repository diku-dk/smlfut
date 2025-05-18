# Changelog

All user-visible changes are noted here.

## [1.6.3]

* Detect use-after-free when passing inputs to entry points.

## [1.6.2]

* Fixed generated names of SML versions of sum types.

* Arrays of opaque types are now supported.

* Better generated type names for SML types corresponding to Futhark
  sum types.

## [1.6.1]

* Fixed infinite loop on some manifests.

## [1.6.0]

* Better names for structures corresponding to opaque types that are
  arrays and records.

## [1.5.0]

* Futhark tuples now work properly.

* The array signatures now support an `index` function.

* Arrays of records are now supported.

## [1.4.0]

* Arrays modules now have `new_raw` and `values_raw` functions.

* Using an object after it has been freed now causes the exception
  `Free` to be raised, rather than corrupting memory.

## [1.3.0]

* Opaque types now support `store`/`restore` functions.

* Sum types are supported.

## [1.2.0]

* MLKit is now supported.

## [1.1.0]

* Added new `Context` functions: `clearCaches`, `report`,
  `pauseProfiling`, `unpauseProfiling`.

* Added tuning parameters to `cfg`.

* `error` exception is now `Error`.

## [1.0.0]

Initial release.
