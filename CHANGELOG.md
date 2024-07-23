# Changelog

All user-visible changes are noted here.

## [1.5.0]

* Futhark tuples not work properly.

* The array signatures now support an `index` function.

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
