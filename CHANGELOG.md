# Revision history for sport

## 3.1.0.0 -- 7-25-2025

* Fix exclusive flag, was not being set

## 3.0.0.0 -- 6-7-2026

* Update `withSport`, add `withNewSport`
* Remove `runSport` daemon

## 2.0.0.0 -- 6-6-2026

* Add `flushSport`, `withOpenSport`
* Add `displaySportException`, `SportException` instances
* Improve concurrency and resource handling
* Add `getOpenSportCfg`

## 1.0.0.0 -- 4-30-2026

* Fix `SportAlreadyOpen` bug
* Update exception types
* Add `isOpenSport` to STM API

## 0.2.0.0 -- 4-21-2026

* Expose current configuration in STM
* Re-export baud rate from unix package

## 0.1.0.0 -- 4-20-2026

* First version. Released on an unsuspecting world.
