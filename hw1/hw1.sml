(*Convert date to number by year * 10000 + month * 100 + day*)
fun dateToNum(pr: int*int*int) =
    (#1 pr) * 10000 + (#2 pr) * 100 + (#3 pr)

(*produce true if first is before second*)
fun is_older(first: int*int*int, second: int*int*int) =
    let
        val x = dateToNum(first)
        val y = dateToNum(second)
    in
        if x < y
        then true
        else false
    end

(*produce the number of dates in the given month*)
fun number_in_month(dates: (int*int*int) list, month:int) =
    if null dates
    then 0
    else if (#2 (hd dates)) = month
         then 1 + number_in_month(tl dates, month)
         else number_in_month(tl dates, month)

(*return the number of dates in the list of dates that are in any of the months in the list of months.*)
fun number_in_months(dates: (int*int*int) list, months:int list) =
    if null dates orelse null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* return a list of dates that in the given month *)
fun dates_in_month(dates: (int*int*int) list, month:int) =
    if null dates
    then []
    else if (#2 (hd dates)) = month
         then (hd dates) :: dates_in_month(tl dates, month)
         else dates_in_month(tl dates, month)

(* return a list of dates that in the given list of months*)
fun dates_in_months(dates: (int*int*int) list, months:int list) =
    if null dates orelse null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* return the nth element of the list *)
fun get_nth(strs: string list, n: int) =
    if n = 1
    then hd strs
    else get_nth((tl strs), n-1)

(* return a stgring of the form like January 20, 2013 *)
fun date_to_string(date: int*int*int) =
    (* get the string form of the month*)
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, (#2 date)) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(* return an int. n such that the first n elements of the list add to less than sum*)
fun number_before_reaching_sum(sum: int, positives: int list) =
    if null positives
    then 0
    else if (hd positives) >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - (hd positives), (tl positives))

(* return what month the day_of_year is in*)
fun what_month(day_of_year: int) =
    let val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        1 + number_before_reaching_sum(day_of_year, days_in_month)
    end

(* return an int list [m1, m2, ..., mn] where mx is the month of the day in.*)
fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)

(* return the oldest dates in the list *)
fun oldest(dates: (int*int*int) list) =
    if null dates
    then NONE
    else
        let
            val tl_ans = oldest(tl dates)
        in  (* make sure tl_ans is not NONE and check if older *)
            if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
            then tl_ans
            else SOME(hd dates)
        end
