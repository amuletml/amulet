type option 'a = None | Some of 'a

let consume_none = function
 | None, _ -> 1
 | _, None -> 2
 | Some 0, Some 0 -> 3
 | Some _, Some _ -> 4

let consume_one = function
 | None, _ -> 1
 | _, None -> 2
 | Some 0, Some 0 -> 3
 | Some x, Some _ -> x

let consume_many = function
 | None, _ -> 1
 | _, None -> 2
 | Some 0, Some 0 -> 3
 | Some x, Some y -> x + y
