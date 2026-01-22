(define (count-one-atom ATM)
"
  count-one-atom ATM -- increment the count by one on ATM, and
  update the SQL database to hold that count.
  calls count-one-atom-times with count argument equal to 1.
"
	(count-one-atom-times ATM 1)
)
(define (count-one-atom-times ATM times)
"
  count-one-atom-times ATM times-- increment the count by times on ATM, and
  update the SQL database to hold that count.
  This will also automatically fetch the previous count from
  the SQL database, so that counting will work correctly, when
  picking up from a previous point.
  Warning: this is NOT SAFE for distributed processing! That is
  because this does NOT grab the count from the database every time,
  so if some other process updates the database, this will miss that
  update.
"
	(define (incr-times atom TIMES)
		(if (not (cog-ctv? (cog-tv atom)))
			(fetch-atom atom))
		(cog-inc-count! atom TIMES)
	)
	(begin
		(incr-times ATM times)
		(store-atom ATM))
)