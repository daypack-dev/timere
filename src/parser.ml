type typ =
  | Years of int list
  | Months of Time.month list
  | Month_days of int list
  | Weekdays of Time.weekday list
  | Hours of int list
  | Minutes of int list
  | Seconds of int list

type token = {
  pos : int * int;
  may_be : typ;
}
