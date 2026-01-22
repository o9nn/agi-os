(use-modules (ice-9 threads))
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog persist))
(define-public (get-count ATOM) (cog-count ATOM))
(define (set-count ATOM CNT) (cog-set-tv! ATOM (CountTruthValue 1 0 CNT)))
(define *-count-upgrade-mutex-* (make-mutex))
(define (count-inc-atom ATM CNT)
"
count-inc-atom ATM CNT -- increment the count by CNT on ATM, and
update storage to hold that count.
This will also automatically fetch the previous count from storage,
so that counting will work correctly, when picking up from a previous
point. This code is thread-safe.
Warning: this is NOT SAFE for distributed processing! That is
because this does NOT grab the count from the database every time,
so if some other process updates the database, this will miss that
update. Multiple distributed counters will continue to clobber
each-other indefinitely
See also: count-one-atom
"
(define (incr-one atom)
(if (not (cog-ctv? (cog-tv atom)))
(begin
(lock-mutex *-count-upgrade-mutex-*)
(if (not (cog-ctv? (cog-tv atom)))
(fetch-atom atom))
(cog-inc-count! atom CNT)
(unlock-mutex *-count-upgrade-mutex-*))
(cog-inc-count! atom CNT))
)
(incr-one ATM)
(store-atom ATM)
)
(define (count-one-atom ATM)
"
count-one-atom ATM -- increment the count by one on ATM, and
update storage to hold that count.
This will also automatically fetch the previous count from storage,
so that counting will work correctly, when picking up from a previous
point. This code is thread-safe.
Warning: this is NOT SAFE for distributed processing! That is
because this does NOT grab the count from the database every time,
so if some other process updates the database, this will miss that
update. Multiple distributed counters will continue to clobber
each-other indefinitely
See also: count-inc-atom
"
(count-inc-atom ATM 1)
)