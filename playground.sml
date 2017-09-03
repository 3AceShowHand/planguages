fun append_lst(xs, ys) = 
    case xs of
          [] => ys
        | x::xs => x::append_lst(xs, ys)

fun append_lst2(xs, ys) = 
    case xs of 
          [] => ys
        | x::xs' => x::append_lst2(xs', ys)

fun sum_lst(xs) = 
    case xs of
          [] => 0.0
        | x::xs' => x + sum_lst(xs')