(*
  http://projecteuler.net/index.php?section=problems&id=19

  You are given the following information, but you may prefer to do some research for yourself.

  * 1 Jan 1900 was a Monday.
  * Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
  * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

  How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
*)

type weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
type date = { month : int; year : int; weekday : weekday }
let epoch = { month = 1; year = 1900; weekday = Monday }
let is_leap_year year =
  if year mod 4 = 0 then
    if year mod 100 = 0 then
      if year mod 400 = 0 then true
      else false
    else true
  else false
let days_in_month date =
  match date.month with
    4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap_year date.year then 29 else 28
  | _ -> 31
let days_in_year year =
  if is_leap_year year then 366 else 365

let compare_date (d1 : date) (d2 : date) =
  let year = compare d1.year d2.year in
  if year = 0 then compare d1.month d2.month
  else year

let incr_weekday day =
  match day with
    Sunday -> Monday
  | Monday -> Tuesday
  | Tuesday -> Wednesday
  | Wednesday -> Thursday
  | Thursday -> Friday
  | Friday -> Saturday
  | Saturday -> Sunday

let add_weekday day n =
  let new_day = ref day in
  for i = 1 to (n mod 7) do
    new_day := incr_weekday !new_day
  done;
  !new_day

let incr_date date =
  let days = days_in_month !date in
  date := { month = succ (!date.month mod 12);
            year = !date.year + (!date.month / 12);
            weekday = add_weekday !date.weekday days }

let add_date date months =
  let new_date = ref date in
  for i = 1 to months do
    incr_date new_date
  done;
  !new_date

let _ =
  let cur_date = ref (add_date epoch 12)
  and count = ref 0 in
  while compare_date !cur_date { month = 12; year = 2000; weekday = Sunday } <= 0 do
    if !cur_date.weekday = Sunday then incr count;
    incr_date cur_date
  done;
  print_int !count; print_newline ()
